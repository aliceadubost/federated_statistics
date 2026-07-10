# invite.R
# =====================================================================
# Phase 2 — invite-bundle crypto and format helpers.
#
# Pure, GUI-free functions used by both the coordinator (to mint and
# verify signed invites and registration callbacks) and the site
# launcher (to verify invites and sign its own registration). See
# DESIGN_invite_bundle.md sections 1, 5 and 5a.
#
# Wire representations: keys, signatures and tokens are carried as
# base64url strings so they drop straight into JSON. Ed25519 is provided
# by the sodium package.
# =====================================================================

INVITE_PREFIX <- "FEDSTAT2."

# ---- base64url (no padding) ----------------------------------------
# JSON/email/URL-safe: no '+', '/', or '=' to be mangled in transit.
.b64url_encode <- function(raw) {
  s <- jsonlite::base64_enc(raw)
  s <- gsub("[[:space:]]", "", s)   # base64_enc line-wraps; drop newlines
  s <- gsub("+", "-", s, fixed = TRUE)
  s <- gsub("/", "_", s, fixed = TRUE)
  gsub("=", "", s, fixed = TRUE)
}

.b64url_decode <- function(str) {
  s <- gsub("[[:space:]]", "", str)
  s <- gsub("-", "+", s, fixed = TRUE)
  s <- gsub("_", "/", s, fixed = TRUE)
  pad <- nchar(s) %% 4L
  if (pad > 0L) s <- paste0(s, strrep("=", 4L - pad))
  jsonlite::base64_dec(s)
}

.key_raw <- function(b64) {
  out <- tryCatch(.b64url_decode(b64), error = function(e) NULL)
  if (is.null(out) || !length(out))
    stop("Invalid key/signature encoding.")
  out
}

# ---- canonical JSON ------------------------------------------------
# Deterministic bytes for signing/verifying: sort object keys
# recursively, no insignificant whitespace, scalars unboxed.
.sort_recursive <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x))]
    x <- lapply(x, .sort_recursive)
  }
  x
}

.canonical_json <- function(x) {
  as.character(jsonlite::toJSON(.sort_recursive(x),
                                auto_unbox = TRUE, null = "null"))
}

# Fixed field set/types so create and verify produce identical bytes
# regardless of how JSON round-tripping typed the values.
.normalize_payload <- function(p) {
  list(
    v     = as.integer(p$v),
    study = as.character(p$study),
    coord = as.character(p$coord),
    sid   = as.character(p$sid),
    name  = if (is.null(p$name)) "" else as.character(p$name),
    tok   = as.character(p$tok),
    iat   = as.integer(p$iat),
    exp   = as.integer(p$exp)
  )
}

# ---- key / token / id generation -----------------------------------

#' Generate an Ed25519 keypair
#'
#' @return A list with base64url-encoded \code{private} and \code{public}
#'   keys. The private key is a secret; persist it only in a
#'   permission-hardened, gitignored file.
#' @export
fed_keypair <- function() {
  priv <- sodium::sig_keygen()
  pub  <- sodium::sig_pubkey(priv)
  list(private = .b64url_encode(priv),
       public  = .b64url_encode(pub))
}

#' Derive the public key from an Ed25519 private key
#'
#' @param private_key base64url private key from \code{\link{fed_keypair}}.
#' @return base64url public key.
#' @export
fed_public_key <- function(private_key) {
  .b64url_encode(sodium::sig_pubkey(.key_raw(private_key)))
}

#' Generate a random per-site bearer token
#'
#' @param nbytes Number of random bytes (default 24 -> 32 base64url chars).
#' @return A base64url token string.
#' @export
fed_token <- function(nbytes = 24L) {
  .b64url_encode(sodium::random(as.integer(nbytes)))
}

#' Constant-time string equality
#'
#' Compares two strings without an early-exit branch, so the time taken does
#' not depend on how many leading characters match. Use this (not \code{==}
#' or \code{identical()}) when comparing a secret such as a bearer token, so
#' the secret cannot be recovered character-by-character through a timing
#' side-channel. Token length is not itself secret (tokens have a fixed
#' length), so returning early on a length mismatch is acceptable.
#'
#' @param a,b Character scalars.
#' @return \code{TRUE} iff the two strings are byte-for-byte identical.
#' @export
fed_ct_equal <- function(a, b) {
  if (!is.character(a) || !is.character(b) ||
      length(a) != 1L || length(b) != 1L ||
      is.na(a) || is.na(b)) return(FALSE)
  ra <- as.integer(charToRaw(a)); rb <- as.integer(charToRaw(b))
  if (length(ra) != length(rb)) return(FALSE)
  sum(bitwXor(ra, rb)) == 0L
}

#' Generate a unique site id
#'
#' @return A short id like \code{"s_7f3a9c2e1b0d"}.
#' @export
fed_sid <- function() {
  paste0("s_", sodium::bin2hex(sodium::random(6L)))
}

#' Short, human-comparable fingerprint of a public key
#'
#' For optional out-of-band verification of a coordinator's key. The
#' same key always yields the same fingerprint.
#'
#' @param public_key base64url public key.
#' @return A grouped hex string, e.g. \code{"a1b2-c3d4-e5f6-7890"}.
#' @export
fed_fingerprint <- function(public_key) {
  hex   <- sodium::bin2hex(sodium::sha256(.key_raw(public_key)))
  short <- substr(hex, 1L, 16L)
  paste(substring(short, seq(1L, 13L, 4L), seq(4L, 16L, 4L)), collapse = "-")
}

# ---- detached sign / verify (used for registration bodies) ---------

#' Sign a message with an Ed25519 private key
#'
#' @param message A single character string (the exact bytes signed).
#' @param private_key base64url private key.
#' @return base64url detached signature.
#' @export
fed_sign <- function(message, private_key) {
  .b64url_encode(sodium::sig_sign(charToRaw(message), .key_raw(private_key)))
}

#' Verify an Ed25519 signature over a message
#'
#' @param message The signed character string.
#' @param signature base64url signature from \code{\link{fed_sign}}.
#' @param public_key base64url public key.
#' @return \code{TRUE} if valid, \code{FALSE} otherwise (never errors).
#' @export
fed_verify <- function(message, signature, public_key) {
  tryCatch(
    isTRUE(sodium::sig_verify(charToRaw(message),
                              .key_raw(signature), .key_raw(public_key))),
    error = function(e) FALSE
  )
}

#' Canonical message a site signs for a registration callback
#'
#' Shared by the site (to sign) and the registrar (to verify) so the
#' signed bytes are identical on both ends. See DESIGN_invite_bundle.md
#' section 5a.
#'
#' @param sid Site id from the invite.
#' @param site_addr The site's own base URL.
#' @param site_pk The site's base64url public key.
#' @param ts Unix timestamp (integer seconds) of the request.
#' @return A canonical JSON string.
#' @export
fed_register_message <- function(sid, site_addr, site_pk, ts) {
  .canonical_json(list(
    sid       = as.character(sid),
    site_addr = as.character(site_addr),
    site_pk   = as.character(site_pk),
    ts        = as.integer(ts)
  ))
}

# ---- invite create / parse -----------------------------------------

#' Create a signed invite string
#'
#' Builds the payload, signs canonical(payload) with the coordinator's
#' private key, wraps payload + public key + signature into the
#' \code{FEDSTAT2.} envelope. See DESIGN_invite_bundle.md section 1.
#'
#' @param study Study name shown to the operator.
#' @param coord Coordinator address (MagicDNS host or IP) + registrar port.
#' @param sid Site id (\code{\link{fed_sid}}).
#' @param token Per-site bearer token (\code{\link{fed_token}}).
#' @param private_key Coordinator base64url private key.
#' @param name Optional human label for the site.
#' @param ttl_days Invite lifetime in days (default 7).
#' @param now Current unix time (override for testing).
#' @return The invite string (prefix + base64url envelope).
#' @export
fed_invite_create <- function(study, coord, sid, token, private_key,
                              name = "", ttl_days = 7,
                              now = as.integer(Sys.time())) {
  now <- as.integer(now)
  payload <- .normalize_payload(list(
    v = 1L, study = study, coord = coord, sid = sid, name = name,
    tok = token, iat = now, exp = as.integer(now + ttl_days * 86400L)
  ))
  priv_raw <- .key_raw(private_key)
  sig <- sodium::sig_sign(charToRaw(.canonical_json(payload)), priv_raw)
  envelope <- list(
    payload = payload,
    pk      = .b64url_encode(sodium::sig_pubkey(priv_raw)),
    sig     = .b64url_encode(sig)
  )
  json <- as.character(jsonlite::toJSON(envelope, auto_unbox = TRUE, null = "null"))
  paste0(INVITE_PREFIX, .b64url_encode(charToRaw(json)))
}

#' Parse and verify a signed invite string
#'
#' Decodes the envelope, verifies the Ed25519 signature over
#' canonical(payload) using the embedded public key, and checks expiry.
#' Does NOT itself establish first-contact authenticity — that is the
#' caller's TOFU pin / optional fingerprint check (see
#' DESIGN_invite_bundle.md section 5).
#'
#' @param invite The invite string.
#' @param now Current unix time (override for testing).
#' @return A list: \code{ok} (logical), \code{reason} ("" or an error
#'   message), \code{payload} (normalized, or NULL), \code{pk} (the
#'   embedded public key, or NULL), \code{expired} (logical).
#' @export
fed_invite_parse <- function(invite, now = as.integer(Sys.time())) {
  fail <- function(reason)
    list(ok = FALSE, reason = reason, payload = NULL, pk = NULL, expired = FALSE)

  invite <- trimws(invite)
  if (!startsWith(invite, INVITE_PREFIX))
    return(fail("This does not look like a FEDSTAT2 invite (wrong prefix)."))

  b64 <- substr(invite, nchar(INVITE_PREFIX) + 1L, nchar(invite))
  json <- tryCatch(rawToChar(.b64url_decode(b64)), error = function(e) NULL)
  if (is.null(json))
    return(fail("Invite is corrupted or was truncated when copied."))

  env <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(env) || is.null(env$payload) || is.null(env$pk) || is.null(env$sig))
    return(fail("Invite is malformed (missing payload, key, or signature)."))

  payload <- tryCatch(.normalize_payload(env$payload), error = function(e) NULL)
  if (is.null(payload) || is.na(payload$exp))
    return(fail("Invite payload is malformed."))

  pk  <- as.character(env$pk)
  sig <- as.character(env$sig)
  ok_sig <- fed_verify(.canonical_json(payload), sig, pk)
  if (!ok_sig)
    return(fail("Invite signature is invalid (tampered, corrupted, or forged)."))

  if (as.integer(now) >= payload$exp)
    return(list(ok = FALSE, reason = "This invite has expired. Ask the coordinator for a new one.",
                payload = payload, pk = pk, expired = TRUE))

  list(ok = TRUE, reason = "", payload = payload, pk = pk, expired = FALSE)
}
