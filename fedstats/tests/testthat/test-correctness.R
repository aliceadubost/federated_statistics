# Correctness: with adequately-sized cells, the federated result must equal
# the result of running on the full pooled data (base R). These guard against
# the privacy machinery ever changing a legitimate result.

make_two_sites <- function(seed = 1, n = 400) {
  set.seed(seed)
  dat <- data.frame(
    age  = round(rnorm(n, 55, 12), 2),
    grp  = sample(c("A", "B", "C"), n, replace = TRUE),
    sexM = rbinom(n, 1, 0.5),
    ybin = rbinom(n, 1, 0.4)
  )
  i <- seq_len(n %/% 2)
  list(dat = dat,
       servers = list(create_server(dat[i, ],  min_n = 1, min_cell = 5),
                      create_server(dat[-i, ], min_n = 1, min_cell = 5)))
}

test_that("fed_numeric matches pooled mean/sd", {
  s <- make_two_sites()
  r <- fed_numeric(s$servers, "age")
  expect_equal(r$mean, mean(s$dat$age))
  expect_equal(r$sd,   sd(s$dat$age))
  expect_equal(r$n,    nrow(s$dat))
})

test_that("fed_group_numeric matches pooled per-group mean/sd", {
  s <- make_two_sites()
  g <- fed_group_numeric(s$servers, "age", "grp")
  for (lv in c("A", "B", "C")) {
    expect_equal(g[[lv]]$mean, mean(s$dat$age[s$dat$grp == lv]))
    expect_equal(g[[lv]]$sd,   sd(s$dat$age[s$dat$grp == lv]))
    expect_equal(g[[lv]]$n_sites_suppressed, 0L)
  }
})

test_that("fed_welch_t matches base t.test", {
  s  <- make_two_sites()
  fw <- fed_welch_t(s$servers, "age", "grp", "A", "B")
  bt <- t.test(s$dat$age[s$dat$grp == "A"], s$dat$age[s$dat$grp == "B"])
  expect_equal(fw$t, unname(bt$statistic))
  expect_equal(fw$p, bt$p.value)
})

test_that("fed_chisq_2x2 matches base chisq.test", {
  s  <- make_two_sites()
  fc <- fed_chisq_2x2(s$servers, "sexM", "ybin")
  bc <- suppressWarnings(chisq.test(table(s$dat$sexM, s$dat$ybin)))
  expect_equal(fc$statistic, unname(bc$statistic))
  expect_equal(fc$p,         bc$p.value)
})

test_that("fed_lm matches base lm", {
  s  <- make_two_sites()
  fl <- fed_lm(s$servers, age ~ sexM)
  bl <- lm(age ~ sexM, data = s$dat)
  expect_equal(unname(fl$coefficients), unname(coef(bl)))
  expect_equal(unname(fl$se), unname(summary(bl)$coefficients[, 2]), tolerance = 1e-7)
})

test_that("fed_logistic_newton matches base glm", {
  s  <- make_two_sites()
  fg <- fed_logistic_newton(s$servers, ybin ~ age + sexM,
                            robust_cluster = FALSE, verbose = FALSE)
  bg <- glm(ybin ~ age + sexM, data = s$dat, family = binomial)
  expect_equal(unname(fg$coefficients), unname(coef(bg)), tolerance = 1e-6)
  expect_true(fg$converged)
})

test_that("in-process and pooled agree regardless of how sites are split", {
  s3 <- make_two_sites(seed = 7)
  # three-way split of the same data must give the same pooled lm
  dat <- s3$dat; idx <- cut(seq_len(nrow(dat)), 3, labels = FALSE)
  srv3 <- lapply(1:3, function(k) create_server(dat[idx == k, ], min_n = 1))
  a <- fed_lm(srv3, age ~ sexM)
  b <- lm(age ~ sexM, data = dat)
  expect_equal(unname(a$coefficients), unname(coef(b)))
})
