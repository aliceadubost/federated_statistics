# netutil.R
# =====================================================================
# Phase 2 — shared Tailscale / network helpers.
#
# The bind-host logic here is the single source of truth reused by both
# the site API server (engine/site/api_server.R) and the coordinator
# registrar, so the "bind to the Tailscale interface, warn and fall back
# to 0.0.0.0" behaviour is defined in exactly one place (no reinvention).
# =====================================================================

#' The machine's Tailscale IPv4 address, or ""
#'
#' @return The \code{100.x.y.z} address as a string, or "" if Tailscale
#'   is not running / not detected.
#' @export
fed_tailscale_ip <- function() {
  tryCatch({
    out <- suppressWarnings(system("tailscale ip -4", intern = TRUE,
                                   ignore.stderr = TRUE))
    out <- trimws(out)
    out <- out[nzchar(out)]
    if (length(out) >= 1L && grepl("^100\\.", out[1L])) out[1L] else ""
  }, error = function(e) "")
}

#' The machine's Tailscale MagicDNS hostname, or ""
#'
#' Preferred over the raw IP in invites because it survives IP changes
#' (DESIGN_invite_bundle.md, Q3). Falls back to "" when MagicDNS is off
#' or the status cannot be read.
#'
#' @return The hostname (e.g. \code{"coordinator.tailnet.ts.net"}, no
#'   trailing dot), or "".
#' @export
fed_tailscale_hostname <- function() {
  tryCatch({
    raw <- suppressWarnings(system("tailscale status --json", intern = TRUE,
                                   ignore.stderr = TRUE))
    if (!length(raw)) return("")
    st <- jsonlite::fromJSON(paste(raw, collapse = ""), simplifyVector = TRUE)
    dns <- st$Self$DNSName
    if (is.null(dns) || !nzchar(dns)) return("")
    sub("\\.$", "", dns)   # strip trailing dot from the FQDN
  }, error = function(e) "")
}

#' Preferred advertised host: MagicDNS hostname if available, else IP
#'
#' @return MagicDNS hostname, or the Tailscale IP, or "" if neither.
#' @export
fed_advertised_host <- function() {
  h <- fed_tailscale_hostname()
  if (nzchar(h)) return(h)
  fed_tailscale_ip()
}

#' Restrict a file to the current user (best-effort, cross-platform)
#'
#' Used for files that hold secrets (tokens / private keys). Pairs with
#' \code{\link{fed_check_file_perms}}, which warns if it did not take.
#'
#' @param path File to harden.
#' @return TRUE if the file existed and hardening was attempted.
#' @export
fed_harden_file <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  if (.Platform$OS.type == "windows") {
    user <- Sys.getenv("USERNAME")
    if (nzchar(user))
      try(system2("icacls", c(shQuote(path), "/inheritance:r",
                              "/grant:r", shQuote(paste0(user, ":F"))),
                  stdout = FALSE, stderr = FALSE), silent = TRUE)
  } else {
    try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  }
  invisible(TRUE)
}

#' Warn if a secret file is readable beyond its owner
#'
#' @param path File to check.
#' @return A warning string, or "" if fine / not checkable.
#' @export
fed_check_file_perms <- function(path) {
  if (!file.exists(path)) return("")
  if (.Platform$OS.type == "windows") return("")  # rely on harden-on-write
  mode <- file.info(path)$mode
  if (is.na(mode)) return("")
  if (bitwAnd(as.integer(mode), strtoi("077", 8L)) != 0L)
    return(sprintf("WARNING: %s is readable by other users (mode %s).",
                   path, format(mode)))
  ""
}

#' Translate a low-level HTTP/connection failure into a friendly message
#'
#' Only the three specific failure modes named in the Phase 3 charter are
#' translated; anything else returns \code{NULL} so the caller keeps its
#' existing (more specific) message — e.g. a 400 from a \code{min_n}
#' validation failure already carries a useful, specific message that must
#' not be replaced by a generic one.
#'
#' @param e Condition object from a failed \code{httr::POST()}/\code{GET()}
#'   call (connection-level failure), or \code{NULL} if the call completed
#'   but returned a non-2xx status.
#' @param status_code HTTP status code, or \code{NULL} for a connection-level
#'   failure.
#'
#' @return A friendly message string, or \code{NULL} if this isn't one of
#'   the recognised cases.
#' @export
fed_friendly_http_error <- function(e = NULL, status_code = NULL) {
  if (!is.null(status_code)) {
    if (status_code == 401)
      return("Authentication failed — check your site's token.")
    return(NULL)
  }
  if (!is.null(e)) {
    msg <- conditionMessage(e)
    if (grepl("timeout|timed out", msg, ignore.case = TRUE))
      return("Site not responding — it may be offline.")
    if (grepl("Failed to connect|Connection refused|couldn't connect|No route to host",
              msg, ignore.case = TRUE))
      return("Site unreachable — is the server running?")
  }
  NULL
}

#' Decide the interface to bind a server to
#'
#' Security-critical: the Tailscale interface is the trust boundary for the
#' whole system. When Tailscale is present we bind to it. When it is NOT, we
#' deliberately fall back to loopback (\code{127.0.0.1}) — reachable only
#' from the same machine — rather than \code{0.0.0.0}, so a
#' misconfigured/offline-Tailscale site never exposes patient-derived
#' aggregates to the local network or internet. An operator who genuinely
#' runs on a different private network can set \code{FED_BIND_HOST} to an
#' explicit address (informed opt-in); that always wins.
#'
#' @return A list: \code{host} (the bind address), \code{tailscale}
#'   (logical; FALSE means Tailscale was not detected), and \code{forced}
#'   (logical; TRUE when \code{FED_BIND_HOST} overrode the choice).
#' @export
fed_bind_host <- function() {
  forced <- Sys.getenv("FED_BIND_HOST", unset = "")
  if (nzchar(forced))
    return(list(host = forced, tailscale = nzchar(fed_tailscale_ip()), forced = TRUE))
  ip <- fed_tailscale_ip()
  if (nzchar(ip)) list(host = ip,          tailscale = TRUE,  forced = FALSE)
  else            list(host = "127.0.0.1", tailscale = FALSE, forced = FALSE)
}
