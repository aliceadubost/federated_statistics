# Privacy: small cells must never leave a site. These guard against a future
# change silently reintroducing an individual-patient disclosure.

test_that("group_summaries withholds a group below min_cell", {
  d <- data.frame(age = c(rnorm(60, 50, 10), 88, 91),
                  grp = c(rep("BIG", 60), "RARE", "RARE"))
  gs <- create_server(d, min_n = 1, min_cell = 5)$group_summaries("age", "grp")
  expect_true(isTRUE(gs$stats[["RARE"]]$suppressed))
  expect_null(gs$stats[["RARE"]]$sum)      # no value leaves the site
  expect_null(gs$stats[["RARE"]]$n)
  expect_equal(gs$stats[["BIG"]]$n, 60)    # adequately-sized group is intact
})

test_that("group with exactly min_cell members is NOT suppressed", {
  d <- data.frame(x = c(rep(1, 5), rep(2, 20)),
                  g = c(rep("A", 5), rep("B", 20)))
  gs <- create_server(d, min_n = 1, min_cell = 5)$group_summaries("x", "g")
  expect_false(isTRUE(gs$stats[["A"]]$suppressed))
  expect_equal(gs$stats[["A"]]$n, 5)
})

test_that("counts_2x2 withholds the whole table when any cell is small", {
  # cell x0y1 = 2 (< 5) -> entire table suppressed
  d <- data.frame(x = c(rep(0, 40), rep(1, 40)),
                  y = c(rep(0, 38), rep(1, 2), rep(0, 20), rep(1, 20)))
  c2 <- create_server(d, min_n = 1, min_cell = 5)$counts_2x2("x", "y")
  expect_true(isTRUE(c2$suppressed))
  expect_null(c2$n00)
  expect_null(c2$n11)
})

test_that("counts_2x2 returns all cells when every cell clears min_cell", {
  d <- data.frame(x = c(rep(0, 40), rep(1, 40)),
                  y = c(rep(0, 20), rep(1, 20), rep(0, 20), rep(1, 20)))
  c2 <- create_server(d, min_n = 1, min_cell = 5)$counts_2x2("x", "y")
  expect_null(c2$suppressed)
  expect_equal(c2$n11, 20)
})

test_that("validate_data never returns exact min/max or out-of-range values", {
  d <- data.frame(v = c(rnorm(30, 10, 2), 999))
  vr <- create_server(d, min_n = 1, min_cell = 5)$validate_data(
    list(v = list(type = "numeric", min = 0, max = 100)))
  rep <- vr$var_reports$v
  expect_null(rep$min)
  expect_null(rep$max)
  # a count of out-of-range values, but never the value 999 itself
  expect_false(grepl("999", rep$range_warning))
  expect_true(grepl("1 value", rep$range_warning))
})

test_that("validate_data suppresses mean/sd for a tiny sample", {
  d  <- data.frame(v = c(1, 2, 3))
  vr <- create_server(d, min_n = 1, min_cell = 5)$validate_data(
    list(v = list(type = "numeric")))
  expect_true(isTRUE(vr$var_reports$v$summary_suppressed))
  expect_null(vr$var_reports$v$mean)
})

test_that("a caller cannot lower the threshold over the wire", {
  # validate_data's own min_n argument only affects the formula-feasibility
  # flag; suppression uses the server's fixed min_cell.
  d  <- data.frame(v = c(1, 2, 3))
  vr <- create_server(d, min_n = 1, min_cell = 5)$validate_data(
    list(v = list(type = "numeric")), formula = NULL, min_n_formula = 1L)
  expect_null(vr$var_reports$v$mean)   # still suppressed despite min_n_formula = 1
})

test_that("categorical level counts below min_cell are suppressed", {
  d  <- data.frame(g = c(rep("common", 50), "rare", "rare"))
  vr <- create_server(d, min_n = 1, min_cell = 5)$validate_data(
    list(g = list(type = "categorical")))
  lc <- vr$var_reports$g$level_counts
  expect_null(lc[["rare"]])
  expect_false(is.null(lc[["common"]]))
  expect_equal(vr$var_reports$g$n_levels_suppressed, 1L)
})

test_that("inferential tests refuse rather than bias on a suppressed cell", {
  d  <- data.frame(age = c(rnorm(60, 50, 10), 88, 91),
                   grp = c(rep("BIG", 60), "RARE", "RARE"))
  sv <- create_server(d, min_n = 1, min_cell = 5)
  expect_error(fed_welch_t(list(sv), "age", "grp", "BIG", "RARE"),
               "Cannot compute the Welch")

  d2 <- data.frame(x = c(rep(0, 40), rep(1, 40)),
                   y = c(rep(0, 38), rep(1, 2), rep(0, 20), rep(1, 20)))
  sv2 <- create_server(d2, min_n = 1, min_cell = 5)
  expect_error(fed_chisq_2x2(list(sv2), "x", "y"), "Cannot compute the 2x2")
})

test_that("fed_group_numeric warns and excludes the suppressed site", {
  d  <- data.frame(age = c(rnorm(60, 50, 10), 88, 91),
                   grp = c(rep("BIG", 60), "RARE", "RARE"))
  sv <- create_server(d, min_n = 1, min_cell = 5)
  expect_warning(res <- fed_group_numeric(list(sv), "age", "grp"),
                 "Privacy suppression")
  expect_equal(res[["RARE"]]$n_sites_suppressed, 1L)
})
