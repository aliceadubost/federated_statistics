# Network + comparison helpers: the fail-safe bind default and the
# constant-time token comparison.

test_that("fed_ct_equal is correct on equal/unequal/edge inputs", {
  expect_true(fed_ct_equal("abc123", "abc123"))
  expect_false(fed_ct_equal("abc123", "abc124"))   # same length, one diff
  expect_false(fed_ct_equal("abc", "abcd"))        # different length
  expect_false(fed_ct_equal(NA_character_, "x"))
  expect_false(fed_ct_equal("x", NA_character_))
  expect_false(fed_ct_equal("x", 1))               # non-character
  expect_false(fed_ct_equal(c("a", "b"), "a"))     # non-scalar
})

test_that("fed_bind_host falls back to loopback, never 0.0.0.0", {
  withr_bind <- function(val, code) {
    old <- Sys.getenv("FED_BIND_HOST", unset = NA)
    on.exit(if (is.na(old)) Sys.unsetenv("FED_BIND_HOST")
            else Sys.setenv(FED_BIND_HOST = old))
    if (is.na(val)) Sys.unsetenv("FED_BIND_HOST") else Sys.setenv(FED_BIND_HOST = val)
    force(code)
  }

  # No explicit override: whatever Tailscale returns, the host is either a
  # Tailscale IP or loopback — but NEVER 0.0.0.0 (that was the old leak).
  b <- withr_bind(NA, fed_bind_host())
  expect_false(identical(b$host, "0.0.0.0"))
  expect_true(b$host == "127.0.0.1" || grepl("^100\\.", b$host))
  expect_false(isTRUE(b$forced))

  # Explicit override always wins.
  b2 <- withr_bind("10.0.0.5", fed_bind_host())
  expect_equal(b2$host, "10.0.0.5")
  expect_true(isTRUE(b2$forced))
})

test_that("fed_friendly_http_error only translates the known cases", {
  expect_match(fed_friendly_http_error(status_code = 401), "Authentication")
  expect_null(fed_friendly_http_error(status_code = 400))   # keep specific msg
  expect_match(fed_friendly_http_error(e = simpleError("Timeout was reached")),
               "not responding")
  expect_match(fed_friendly_http_error(e = simpleError("Failed to connect to host")),
               "unreachable")
  expect_null(fed_friendly_http_error(e = simpleError("some other error")))
})
