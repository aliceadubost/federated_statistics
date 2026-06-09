# registrar.R
# =====================================================================
# Phase 2 — coordinator registration listener.
#
# A minimal HTTP endpoint that lets a site "call home" once at Join time
# so the coordinator learns its address without anyone typing a URL.
# Run as a background subprocess by the coordinator Shiny app (mirroring
# how site_app.R runs api_server.R). Writes the registry JSON; the Shiny
# app polls that file to display sites.
#
# Hardening (DESIGN_invite_bundle.md section 0):
#   - binds to the Tailscale interface only (shared fed_bind_host logic)
#   - no unauthenticated registration: Bearer invite-token + Ed25519
#     signature over the request body, both verified
#   - per-source rate limiting
#
# Env:
#   FED_REGISTRAR_PORT   default 8731
#   FED_REGISTRY_FILE    default engine/coordinator/registered_sites.json
#   FED_REG_RATE_MAX     max /register attempts per source per window (default 20)
#   FED_REG_RATE_WINDOW  window in seconds (default 60)
#   FED_REG_MAX_SKEW     max allowed |now - body.ts| in seconds (default 300)
# =====================================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(fedstats)   # fed_verify, fed_bind_host, canonical helpers
})

# Sibling registry logic (state machine + persistence + locking).
.script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.here   <- if (length(.script)) dirname(normalizePath(.script[1])) else getwd()
source(file.path(.here, "registry.R"))

PORT        <- as.integer(Sys.getenv("FED_REGISTRAR_PORT", "8731"))
REG_FILE    <- Sys.getenv("FED_REGISTRY_FILE", reg_default_path())
RATE_MAX    <- as.integer(Sys.getenv("FED_REG_RATE_MAX", "20"))
RATE_WINDOW <- as.numeric(Sys.getenv("FED_REG_RATE_WINDOW", "60"))
MAX_SKEW    <- as.numeric(Sys.getenv("FED_REG_MAX_SKEW", "300"))

# ---- per-source rate limiter (in-memory, fixed window) -------------
.rl <- new.env(parent = emptyenv())
rate_ok <- function(ip) {
  now <- as.numeric(Sys.time())
  hits <- .rl[[ip]]
  hits <- if (is.null(hits)) numeric(0) else hits[hits > now - RATE_WINDOW]
  if (length(hits) >= RATE_MAX) { .rl[[ip]] <- hits; return(FALSE) }
  .rl[[ip]] <- c(hits, now)
  TRUE
}

pr <- plumber::Plumber$new()

#* @apiTitle Federated Statistics Coordinator Registrar

# ---- liveness (unauthenticated) --------------------------------
pr$handle("GET", "/ping", function(req, res) {
  list(status = "ok", service = "registrar", port = PORT)
})

# ---- site registration callback --------------------------------
pr$handle("POST", "/register", function(req, res) {
  tryCatch({
    ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
    if (!rate_ok(ip)) {
      res$status <- 429
      cat(sprintf("[registrar] 429 rate-limited %s\n", ip))
      return(list(error = "Too many registration attempts. Try again shortly."))
    }

    # invite token from the Authorization header
    auth <- req$HTTP_AUTHORIZATION
    if (is.null(auth) || !grepl("^Bearer ", auth)) {
      res$status <- 401
      return(list(error = "Missing or malformed Authorization header."))
    }
    token <- sub("^Bearer ", "", auth)

    body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
    sid       <- body$sid
    site_addr <- body$site_addr
    site_pk   <- body$site_pk
    ts        <- body$ts
    sig       <- body$sig
    if (is.null(sid) || is.null(site_addr) || is.null(site_pk) ||
        is.null(ts) || is.null(sig)) {
      res$status <- 400
      return(list(error = "Registration body missing required fields."))
    }

    # freshness (limits replay)
    if (abs(as.numeric(Sys.time()) - as.numeric(ts)) > MAX_SKEW) {
      res$status <- 400
      return(list(error = "Registration timestamp is too old or skewed."))
    }

    # proof the requester holds the private key for the site_pk it claims
    msg <- fed_register_message(sid, site_addr, site_pk, ts)
    if (!fed_verify(msg, sig, site_pk)) {
      res$status <- 401
      return(list(error = "Invalid registration signature."))
    }

    # state machine under the registry lock
    lock <- reg_lock_acquire(REG_FILE)
    on.exit(reg_lock_release(lock), add = TRUE)
    reg <- reg_load(REG_FILE)
    out <- reg_register(reg, sid, token, site_addr, site_pk)
    if (isTRUE(out$changed)) reg_save(out$reg, REG_FILE)

    res$status <- out$status
    cat(sprintf("[registrar] %d /register sid=%s addr=%s ip=%s\n",
                out$status, sid, site_addr, ip))
    out$body
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- start: bind to Tailscale interface (shared helper) --------
bind <- fed_bind_host()
if (bind$tailscale) {
  cat(sprintf("[registrar] Binding to Tailscale interface: %s:%d\n", bind$host, PORT))
} else {
  cat(paste0("[registrar] WARNING: Tailscale not detected. Binding to 0.0.0.0 ",
             "(all interfaces). For production use, connect Tailscale first.\n"))
}
cat(sprintf("[registrar] Registry file: %s\n", REG_FILE))
perm_warn <- reg_check_perms(REG_FILE)
if (nzchar(perm_warn)) cat("[registrar]", perm_warn, "\n")

pr$run(host = bind$host, port = PORT)
