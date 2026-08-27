# registry.R
# =====================================================================
# Phase 2 — coordinator-side registered-sites registry.
#
# Pure registry/state-machine logic plus JSON persistence and file
# hardening. Sourced by BOTH the registrar subprocess (registrar.R,
# which mutates the registry on /register) and the coordinator Shiny
# app (which reads it for display and issues invites / revocations).
#
# The registry is a single JSON file holding per-site records and a
# revocation list. It contains tokens -> it is gitignored and
# permission-hardened (see DESIGN_invite_bundle.md sections 3, 5, 5a).
#
# Invite lifecycle states: issued -> in_use -> consumed, plus terminal
# expired / revoked. consumed is reached ONLY after a successful
# registration callback; identity (site_addr + site_pk) is recorded on
# entry to in_use and used to tell a retry from a leaked-invite
# collision.
# =====================================================================

suppressPackageStartupMessages(library(jsonlite))

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------
reg_default_path <- function() {
  file.path("engine", "coordinator", "registered_sites.json")
}

# ------------------------------------------------------------------
# File hardening: tokens are secrets (Q2)
# ------------------------------------------------------------------

# Restrict a file to the current user. Best-effort; pairs with
# reg_check_perms() which warns if it didn't take.
reg_harden_file <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  if (requireNamespace("fedstats", quietly = TRUE))
    return(invisible(fedstats::fed_harden_file(path)))
  if (.Platform$OS.type != "windows")
    try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  invisible(TRUE)
}

# Returns a warning string if the file is readable beyond its owner,
# else "". Includes a best-effort ACL check on Windows.
reg_check_perms <- function(path) {
  if (!file.exists(path)) return("")
  if (requireNamespace("fedstats", quietly = TRUE)) {
    msg <- fedstats::fed_check_file_perms(path)
    if (nzchar(msg)) return(paste0(msg, " This file contains site tokens."))
    return("")
  }
  if (.Platform$OS.type == "windows") return("")
  mode <- file.info(path)$mode
  if (is.na(mode)) return("")
  if (bitwAnd(as.integer(mode), strtoi("077", 8L)) != 0L) {
    return(sprintf(
      "WARNING: %s is readable by other users (mode %s) and contains site tokens.",
      path, format(mode)))
  }
  ""
}

# ------------------------------------------------------------------
# Load / save
# ------------------------------------------------------------------
reg_empty <- function() list(version = 1L, sites = list(), revoked = character(0))

# Fill in optional fields that JSON drops when NULL/empty.
reg_normalize_row <- function(r) {
  defaults <- list(
    sid = NA_character_, name = "", study = "", token = NA_character_,
    site_addr = NULL, site_pk = NULL, invite_state = "issued",
    conn_status = "unknown", created_at = NA_integer_, exp = NA_integer_,
    registered_at = NULL, last_seen = NULL, pending = NULL,
    invite = NA_character_  # full invite text, so the registrar can serve
                             # it back at a short link (/i/<sid>) instead of
                             # the operator having to paste the whole thing
  )
  for (k in names(defaults)) if (is.null(r[[k]])) r[[k]] <- defaults[[k]]
  r
}

reg_load <- function(path = reg_default_path()) {
  if (!file.exists(path)) return(reg_empty())
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(raw)) return(reg_empty())
  reg <- reg_empty()
  reg$version <- if (!is.null(raw$version)) as.integer(raw$version) else 1L
  reg$revoked <- if (!is.null(raw$revoked)) unique(as.character(unlist(raw$revoked))) else character(0)
  if (!is.null(raw$sites)) {
    reg$sites <- lapply(raw$sites, reg_normalize_row)
    names(reg$sites) <- vapply(reg$sites, function(r) r$sid, character(1))
  }
  reg
}

# ------------------------------------------------------------------
# Cross-process locking
#
# The registrar subprocess (on /register) and the Shiny app (issuing or
# revoking invites) both read-modify-write the same file. dir.create is
# atomic on the usual filesystems, so it serves as a portable mutex.
# ------------------------------------------------------------------
reg_lock_acquire <- function(path, timeout = 5) {
  lock <- paste0(path, ".lock")
  t0 <- Sys.time()
  repeat {
    if (suppressWarnings(dir.create(lock, showWarnings = FALSE))) return(lock)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout)
      stop("Could not acquire registry lock (held by another process?).")
    Sys.sleep(0.05)
  }
}

reg_lock_release <- function(lock) unlink(lock, recursive = TRUE, force = TRUE)

# Read-modify-write under lock. `fn(reg)` returns the new registry.
reg_modify <- function(path, fn) {
  lock <- reg_lock_acquire(path)
  on.exit(reg_lock_release(lock))
  reg <- reg_load(path)
  reg <- fn(reg)
  reg_save(reg, path)
  invisible(reg)
}

reg_save <- function(reg, path = reg_default_path()) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  writeLines(jsonlite::toJSON(reg, auto_unbox = TRUE, null = "null",
                              pretty = TRUE), tmp)
  reg_harden_file(tmp)            # harden before it becomes the live file
  file.rename(tmp, path)          # atomic replace
  reg_harden_file(path)
  invisible(path)
}

# ------------------------------------------------------------------
# Lazy expiry
# ------------------------------------------------------------------
reg_expire <- function(reg, now = as.integer(Sys.time())) {
  now <- as.integer(now)
  for (sid in names(reg$sites)) {
    r <- reg$sites[[sid]]
    if (!is.na(r$exp) && now >= r$exp &&
        r$invite_state %in% c("issued", "in_use")) {
      reg$sites[[sid]]$invite_state <- "expired"
    }
  }
  reg
}

# ------------------------------------------------------------------
# Invite issuance / revocation (called by the Shiny app)
# ------------------------------------------------------------------
reg_add_invite <- function(reg, sid, name, study, token, exp,
                           invite = NA_character_, now = as.integer(Sys.time())) {
  reg$sites[[sid]] <- reg_normalize_row(list(
    sid = sid, name = name, study = study, token = token,
    invite_state = "issued", conn_status = "unknown",
    created_at = as.integer(now), exp = as.integer(exp),
    invite = invite
  ))
  reg
}

# Revoke and Remove: token onto the revocation list, then drop the row.
reg_revoke_remove <- function(reg, sid) {
  r <- reg$sites[[sid]]
  if (!is.null(r) && !is.na(r$token))
    reg$revoked <- unique(c(reg$revoked, r$token))
  reg$sites[[sid]] <- NULL
  reg
}

reg_is_revoked <- function(reg, token) {
  isTRUE(token %in% reg$revoked)
}

# Approve a held different-identity registration (UI action): adopt the
# pending identity and consume.
reg_approve_pending <- function(reg, sid, now = as.integer(Sys.time())) {
  r <- reg$sites[[sid]]
  if (is.null(r) || is.null(r$pending)) return(reg)
  r$site_addr     <- r$pending$site_addr
  r$site_pk       <- r$pending$site_pk
  r$invite_state  <- "consumed"
  r$registered_at <- as.integer(now)
  r$last_seen     <- as.integer(now)
  r$pending       <- NULL
  reg$sites[[sid]] <- r
  reg
}

# ------------------------------------------------------------------
# The /register state machine (called by the registrar)
#
# Returns list(status, body, reg, changed). `status` is the HTTP code
# the registrar should set. `changed` says whether to persist.
# ------------------------------------------------------------------
reg_register <- function(reg, sid, token, site_addr, site_pk,
                         now = as.integer(Sys.time())) {
  now <- as.integer(now)
  deny <- function(status, msg)
    list(status = status, body = list(error = msg), reg = reg, changed = FALSE)
  ok <- function(msg, changed = TRUE)
    list(status = 200L, body = list(status = "ok", message = msg),
         reg = reg, changed = changed)

  r <- reg$sites[[sid]]
  if (is.null(r))                       return(deny(404L, "Unknown invite."))
  # Constant-time compare so the per-site token can't be recovered byte by
  # byte through response timing.
  if (is.na(r$token) || !fedstats::fed_ct_equal(token, r$token))
                                        return(deny(401L, "Invalid token for this invite."))
  if (reg_is_revoked(reg, token) || identical(r$invite_state, "revoked"))
                                        return(deny(403L, "This invite has been revoked."))
  if (!is.na(r$exp) && now >= r$exp) {
    reg$sites[[sid]]$invite_state <- "expired"
    return(list(status = 403L, body = list(error = "This invite has expired."),
                reg = reg, changed = TRUE))
  }
  if (identical(r$invite_state, "expired"))
                                        return(deny(403L, "This invite has expired."))

  same_identity <- function(r) identical(r$site_addr, site_addr) &&
                               identical(r$site_pk, site_pk)

  # Record-and-consume for a fresh invite, or a matching retry.
  consume <- function(reg) {
    r2 <- reg$sites[[sid]]
    r2$site_addr     <- site_addr
    r2$site_pk       <- site_pk
    r2$invite_state  <- "consumed"
    r2$conn_status   <- "registered"
    if (is.null(r2$registered_at)) r2$registered_at <- now
    r2$last_seen     <- now
    r2$pending       <- NULL
    reg$sites[[sid]] <- r2
    reg
  }

  hold_pending <- function(reg, note) {
    r2 <- reg$sites[[sid]]
    r2$pending <- list(site_addr = site_addr, site_pk = site_pk,
                       pk_matches = identical(r2$site_pk, site_pk),
                       note = note, at = now)
    reg$sites[[sid]] <- r2
    list(status = 409L,
         body = list(error = paste0(
           "A different host is trying to register with this invite. ",
           "Awaiting coordinator approval.")),
         reg = reg, changed = TRUE)
  }

  switch(r$invite_state,
    "issued" = {
      # enter in_use by recording identity, then consume on success
      reg$sites[[sid]]$invite_state <- "in_use"
      reg$sites[[sid]]$site_addr    <- site_addr
      reg$sites[[sid]]$site_pk      <- site_pk
      reg <- consume(reg)
      ok("Registered.")
    },
    "in_use" = {
      if (same_identity(r)) { reg <- consume(reg); ok("Registered.") }
      else hold_pending(reg, "registration in progress from another host")
    },
    "consumed" = {
      if (same_identity(r)) {
        reg$sites[[sid]]$last_seen   <- now
        reg$sites[[sid]]$conn_status <- "registered"
        ok("Already registered (heartbeat).")
      } else {
        hold_pending(reg, "site already registered from another host")
      }
    },
    deny(409L, paste0("Invite is in state '", r$invite_state, "'."))
  )
}
