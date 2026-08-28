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
  library(zip)        # builds the self-hosted site kit (see /kit below)
})

# Sibling registry logic (state machine + persistence + locking).
.script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
.here   <- if (length(.script)) dirname(normalizePath(.script[1])) else getwd()
source(file.path(.here, "registry.R"))

PROJECT_ROOT <- normalizePath(file.path(.here, "..", ".."))

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

# auto_unbox=TRUE: plumber's default serializer wraps scalar fields in
# single-element arrays (e.g. {"invite":["FEDSTAT2..."]}), which silently
# turns body$invite into a length-1 list instead of a plain string on the
# client side after httr parses it. Matches api_server.R's convention.
pr$setSerializer(plumber::serializer_json(auto_unbox = TRUE))

#* @apiTitle Federated Statistics Coordinator Registrar

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

render_info_page <- function(title, heading, intro, button_label = NULL, button_href = NULL,
                             steps = character(0), note = NULL, code_value = NULL) {
  steps_html <- if (length(steps)) paste0(
    '<ol>', paste0('<li>', html_escape(steps), '</li>', collapse = ""), '</ol>'
  ) else ""
  button_html <- if (!is.null(button_label) && !is.null(button_href)) paste0(
    '<a class="btn" href="', html_escape(button_href), '">', html_escape(button_label), '</a>'
  ) else ""
  code_html <- if (!is.null(code_value) && nzchar(code_value)) paste0(
    '<div class="codebox"><div class="codebox-label">Join link</div><code>',
    html_escape(code_value), '</code></div>'
  ) else ""
  note_html <- if (!is.null(note) && nzchar(note)) paste0(
    '<p class="note">', html_escape(note), '</p>'
  ) else ""

  paste0(
    '<!doctype html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>', html_escape(title), '</title><style>',
    'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;',
    'background:#F6F8FC;color:#0F172A;margin:0;padding:40px 20px;}',
    '.card{max-width:760px;margin:0 auto;background:#fff;border:1px solid #DCE3EE;border-radius:20px;',
    'padding:34px 34px 28px;box-shadow:0 16px 40px rgba(15,23,42,.08);}',
    '.eyebrow{font-size:.8rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase;color:#3856E8;margin-bottom:10px;}',
    'h1{font-size:2rem;line-height:1.15;margin:0 0 14px 0;}',
    'p{font-size:1.05rem;line-height:1.65;color:#4A5A73;margin:0 0 14px 0;}',
    'ol{margin:18px 0 0 20px;padding:0;color:#0F172A;}',
    'li{margin:0 0 10px 0;line-height:1.55;}',
    '.btn{display:inline-block;margin:18px 0 8px 0;padding:14px 24px;background:#4B5FE8;color:#fff;',
    'text-decoration:none;border-radius:12px;font-weight:700;font-size:1rem;box-shadow:0 8px 18px rgba(75,95,232,.28);}',
    '.btn:hover{background:#394CD0;}',
    '.codebox{margin-top:18px;padding:16px 18px;border:1px solid #D7DDF0;border-radius:14px;background:#F8FAFF;}',
    '.codebox-label{font-size:.8rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase;color:#3043B2;margin-bottom:8px;}',
    'code{display:block;white-space:pre-wrap;word-break:break-word;font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;',
    'font-size:.95rem;color:#13213B;}',
    '.note{margin-top:18px;font-size:.92rem;color:#70819D;}',
    '</style></head><body><div class="card">',
    '<div class="eyebrow">Federated Statistics</div>',
    '<h1>', html_escape(heading), '</h1>',
    '<p>', html_escape(intro), '</p>',
    button_html, steps_html, code_html, note_html,
    '</div></body></html>'
  )
}

# ---- liveness (unauthenticated) --------------------------------
pr$handle("GET", "/ping", function(req, res) {
  list(status = "ok", service = "registrar", port = PORT)
})

# ---- self-hosted site kit ---------------------------------------
# A brand-new site operator needs the software before they can do anything
# else, and previously the only way to get it was "clone/download from my
# GitHub" — which only works for whoever happens to own that fork. Since a
# site operator must already be on this coordinator's tailnet before they'd
# ever reach this registrar at all (same reasoning as the /i/<sid> short
# link above), the registrar can just serve the install package itself:
# works identically for any coordinator, no GitHub involved.
#
# Only what a site actually needs — never engine/coordinator/, analysis/
# templates, or the design/testing docs.
KIT_FILES <- c(
  "Run/SITE_QUICKSTART.md",
  "Run/Windows/Start Site.bat", "Run/Mac/Start Site.command", "Run/Linux/Start Site.sh",
  "engine/site/site_app.R", "engine/site/api_server.R", "engine/setup.R",
  file.path("fedstats", list.files(file.path(PROJECT_ROOT, "fedstats"), recursive = TRUE))
)
.kit_zip_path <- NULL  # built lazily on first request, then reused

build_kit_zip <- function() {
  path <- file.path(tempdir(), "federated-statistics-site-kit.zip")
  zip::zip(zipfile = path, files = KIT_FILES, root = PROJECT_ROOT)
  path
}

pr$handle("GET", "/kit", function(req, res) {
  ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
  if (!rate_ok(ip)) {
    res$status <- 429
    return(list(error = "Too many requests. Try again shortly."))
  }
  if (is.null(.kit_zip_path) || !file.exists(.kit_zip_path))
    .kit_zip_path <<- build_kit_zip()
  bytes <- readBin(.kit_zip_path, "raw", file.info(.kit_zip_path)$size)
  as_attachment(bytes, "federated-statistics-site-kit.zip")
}, serializer = plumber::serializer_content_type("application/zip"))

# A silent, no-page direct download (the /kit route above) is unreliable
# across browsers — some block it outright, none give clear confirmation
# it worked. /get is what the coordinator actually shares: a real page
# that proves the operator is on the right network (if it loads at all,
# connectivity works) with a visible, genuinely clickable download button.
KIT_PAGE_HTML <- paste0(
  render_info_page(
    title = "Federated Statistics - Site Download",
    heading = "Download the site app",
    intro = "Use this page to download the files needed to join the study on this computer.",
    button_label = "Download Site Kit",
    button_href = "/kit",
    steps = c(
      "Download the site kit.",
      "Unzip it anywhere on your computer.",
      "Open the Run folder and start the Site app for your computer.",
      "Paste the Join link you were sent."
    ),
    note = "If this page opens correctly, your secure connection to the coordinator is working."
  )
)

pr$handle("GET", "/get", function(req, res) {
  KIT_PAGE_HTML
}, serializer = plumber::serializer_content_type("text/html"))

# ---- short invite link -------------------------------------------
# Resolves /i/<sid> to the full invite text, so an operator only has to
# be sent (and paste) a short URL instead of the ~450-char invite blob.
# Only reachable on the Tailscale interface, same as everything else here
# — and a site operator must already be on the tailnet before they'd ever
# be pasting an invite, so this adds no new audience beyond who could
# already reach /register.
pr$handle("GET", "/i/<sid>", function(req, res, sid) {
  ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
  if (!rate_ok(ip)) {
    res$status <- 429
    return(list(error = "Too many requests. Try again shortly."))
  }
  reg <- reg_load(REG_FILE)
  r <- reg$sites[[sid]]
  # Stop handing out an invite once it is no longer needed: expired by time,
  # or already consumed/revoked. After a site has registered, nobody needs
  # the short link again, so a consumed invite (which still carries its
  # token) is never served to another tailnet peer who guesses the sid.
  spent <- !is.null(r) && isTRUE(r$invite_state %in% c("consumed", "revoked", "expired"))
  if (is.null(r) || is.null(r$invite) || is.na(r$invite) || spent ||
      (!is.na(r$exp) && as.integer(Sys.time()) >= r$exp)) {
    res$status <- 404
    return(list(error = "Invite not found or expired."))
  }
  wants_json <- identical(req$HTTP_X_FEDSTATS_CLIENT, "site-app")
  if (isTRUE(wants_json)) return(list(invite = r$invite))

  site_name <- if (!is.null(r$name) && nzchar(trimws(r$name))) trimws(r$name) else "this site"
  invite_url <- sprintf("http://%s/i/%s",
                        if (!is.null(req$HTTP_HOST) && nzchar(req$HTTP_HOST)) req$HTTP_HOST else PORT,
                        sid)
  page <- render_info_page(
    title = paste0("Federated Statistics - Join link for ", site_name),
    heading = paste0("Join link for ", site_name),
    intro = "This link is meant to be pasted into the Federated Site app.",
    button_label = "Download Site App",
    button_href = "/get",
    steps = c(
      "Open the Federated Site app on your computer.",
      "Copy the Join link below.",
      "Paste it into the app and click Join.",
      "Choose your data file and click Start Server."
    ),
    note = "If you already have the app installed, you can ignore the download button and just use the Join link below.",
    code_value = invite_url
  )
  res$setHeader("Content-Type", "text/html; charset=utf-8")
  return(page)
  list(invite = r$invite)
}, serializer = plumber::serializer_content_type("text/html"))

# ---- saved-site status check ------------------------------------
# Lets a site distinguish "I still have a local config file" from
# "the coordinator still recognizes and authorizes this site". Used on
# app startup so a revoked site does not keep presenting itself as
# joined after the coordinator has removed it.
pr$handle("GET", "/site-status/<sid>", function(req, res, sid) {
  ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
  if (!rate_ok(ip)) {
    res$status <- 429
    return(list(error = "Too many requests. Try again shortly."))
  }

  auth <- req$HTTP_AUTHORIZATION
  if (is.null(auth) || !grepl("^Bearer ", auth)) {
    res$status <- 401
    return(list(error = "Missing or malformed Authorization header."))
  }
  token <- sub("^Bearer ", "", auth)

  reg <- reg_expire(reg_load(REG_FILE))

  if (reg_is_revoked(reg, token)) {
    res$status <- 403
    return(list(status = "revoked", active = FALSE,
                message = "This site's access has been revoked."))
  }

  r <- reg$sites[[sid]]
  if (is.null(r)) {
    res$status <- 404
    return(list(status = "unknown", active = FALSE,
                message = "This site is no longer registered with the coordinator."))
  }
  if (is.na(r$token) || !fed_ct_equal(token, r$token)) {
    res$status <- 401
    return(list(error = "Invalid token for this site."))
  }

  if (identical(r$invite_state, "expired")) {
    res$status <- 403
    return(list(status = "expired", active = FALSE,
                message = "This invite has expired."))
  }
  if (identical(r$invite_state, "revoked")) {
    res$status <- 403
    return(list(status = "revoked", active = FALSE,
                message = "This site's access has been revoked."))
  }

  list(
    status = "active",
    active = TRUE,
    study = r$study,
    name = r$name,
    invite_state = r$invite_state,
    conn_status = r$conn_status
  )
})

# ---- site joined callback --------------------------------------
# Sent as soon as the operator accepts the invite, before the local
# server starts. This lets the coordinator show progress as
# "joined, server not running yet" instead of leaving the site on
# plain "invited" until Start Server.
pr$handle("POST", "/site-joined", function(req, res) {
  tryCatch({
    ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
    if (!rate_ok(ip)) {
      res$status <- 429
      cat(sprintf("[registrar] 429 rate-limited %s\n", ip))
      return(list(error = "Too many requests. Try again shortly."))
    }

    auth <- req$HTTP_AUTHORIZATION
    if (is.null(auth) || !grepl("^Bearer ", auth)) {
      res$status <- 401
      return(list(error = "Missing or malformed Authorization header."))
    }
    token <- sub("^Bearer ", "", auth)

    body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
    sid     <- body$sid
    site_pk <- body$site_pk
    ts      <- body$ts
    sig     <- body$sig
    if (is.null(sid) || is.null(site_pk) || is.null(ts) || is.null(sig)) {
      res$status <- 400
      return(list(error = "Join body missing required fields."))
    }

    if (abs(as.numeric(Sys.time()) - as.numeric(ts)) > MAX_SKEW) {
      res$status <- 400
      return(list(error = "Join timestamp is too old or skewed."))
    }

    msg <- fed_register_message(sid, "", site_pk, ts)
    if (!fed_verify(msg, sig, site_pk)) {
      res$status <- 401
      return(list(error = "Invalid join signature."))
    }

    lock <- reg_lock_acquire(REG_FILE)
    on.exit(reg_lock_release(lock), add = TRUE)
    reg <- reg_load(REG_FILE)
    out <- reg_mark_joined(reg, sid, token, site_pk)
    if (isTRUE(out$changed)) reg_save(out$reg, REG_FILE)

    res$status <- out$status
    cat(sprintf("[registrar] %d /site-joined sid=%s ip=%s\n",
                out$status, sid, ip))
    out$body
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
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
if (isTRUE(bind$forced)) {
  cat(sprintf("[registrar] Binding to FED_BIND_HOST override: %s:%d\n", bind$host, PORT))
} else if (bind$tailscale) {
  cat(sprintf("[registrar] Binding to Tailscale interface: %s:%d\n", bind$host, PORT))
} else {
  cat(paste0("[registrar] WARNING: Tailscale not detected. Binding to loopback ",
             "(127.0.0.1) — sites on other machines cannot reach this registrar ",
             "until Tailscale is connected. (Set FED_BIND_HOST to override.)\n"))
}
cat(sprintf("[registrar] Registry file: %s\n", REG_FILE))
perm_warn <- reg_check_perms(REG_FILE)
if (nzchar(perm_warn)) cat("[registrar]", perm_warn, "\n")

pr$run(host = bind$host, port = PORT)
