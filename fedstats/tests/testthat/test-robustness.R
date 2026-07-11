# A per-site failure during an analysis must name the offending site, so the
# coordinator knows who to chase instead of getting an opaque error.

test_that("a failing site is named in the error (fed_lm)", {
  good <- create_server(data.frame(y = rnorm(50), x = rnorm(50)),
                        min_n = 1, label = "Denmark")
  bad  <- list(label = "Sweden",
               lm_suffstats = function(formula) stop("Site unreachable — is the server running?"))
  expect_error(fed_lm(list(good, bad), y ~ x), "Site 'Sweden'")
})

test_that("a failing site is named in the error (fed_numeric)", {
  bad <- list(label = "Norway",
              summary_numeric = function(v) stop("timeout"))
  expect_error(fed_numeric(list(bad), "age"), "Site 'Norway'")
})

test_that("a failing site is named in logistic regression", {
  bad <- list(label = "Finland",
              termnames = function(f) stop("Connection refused"))
  expect_error(
    fed_logistic_newton(list(bad), y ~ x, robust_cluster = FALSE),
    "Site 'Finland'")
})

test_that("the site label round-trips through create_server / create_remote_server", {
  expect_equal(create_server(data.frame(x = 1:30), label = "Iceland")$label, "Iceland")
  expect_equal(create_remote_server("http://100.1.1.1:8000", label = "Iceland")$label,
               "Iceland")
  # sensible default when no label is given
  expect_equal(create_remote_server("http://100.1.1.1:8000")$label,
               "http://100.1.1.1:8000")
})
