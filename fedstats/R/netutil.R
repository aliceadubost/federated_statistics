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

#' Decide the interface to bind a server to
#'
#' Binds to the Tailscale interface when present; otherwise signals a
#' fall back to all interfaces. Callers print their own warning so each
#' server keeps its own wording.
#'
#' @return A list: \code{host} (the bind address) and \code{tailscale}
#'   (logical; FALSE means the 0.0.0.0 fallback is in effect).
#' @export
fed_bind_host <- function() {
  ip <- fed_tailscale_ip()
  if (nzchar(ip)) list(host = ip, tailscale = TRUE)
  else           list(host = "0.0.0.0", tailscale = FALSE)
}
