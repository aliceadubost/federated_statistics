#' Federated numeric summary
#'
#' Computes the pooled count, mean, and standard deviation for a numeric
#' variable by collecting \code{n}, \code{sum}, and \code{sum-of-squares}
#' from each site.
#'
#' @param servers List of server objects (in-process or remote).
#' @param varname Character. Variable name to summarise.
#'
#' @return A list with elements \code{n}, \code{mean}, \code{sd}, \code{sum}.
#'
#' @export
fed_numeric <- function(servers, varname) {
  n <- 0; s <- 0; ss <- 0
  for (srv in servers) {
    r  <- srv$summary_numeric(varname)
    n  <- n  + r$n
    s  <- s  + r$sum
    ss <- ss + r$sumsq
  }
  list(n = n, mean = s / n, sd = sqrt((ss - s^2 / n) / (n - 1)), sum = s)
}

#' Federated per-group numeric summary
#'
#' Returns pooled count, mean, and standard deviation of \code{varname}
#' separately for every level of \code{groupvar}, by collecting per-group
#' sufficient statistics (n, sum, sum-of-squares) from each site.
#'
#' @param servers List of server objects (in-process or remote).
#' @param varname Character. Numeric variable to summarise.
#' @param groupvar Character. Grouping variable (any type; converted to character).
#'
#' @return A named list keyed by group level. Each entry is a list with
#'   \code{n}, \code{mean}, \code{sd}, \code{sum}, and
#'   \code{n_sites_suppressed} (how many sites withheld this group because it
#'   was below their privacy threshold; those sites are excluded from the
#'   pooled figures). If any cell was suppressed, a \code{warning()} is
#'   raised naming the affected groups.
#'
#' @export
fed_group_numeric <- function(servers, varname, groupvar) {
  # Collect per-site, per-group sufficient statistics
  site_stats <- lapply(servers, function(srv)
    srv$group_summaries(varname, groupvar)$stats)

  # Union of all group levels seen across sites
  all_levels <- sort(unique(unlist(lapply(site_stats, names))))

  # Pool across sites for each level, skipping any site that withheld the
  # group for privacy (a group below the site's min_cell comes back as
  # {suppressed:true} with no statistics to add).
  res <- lapply(stats::setNames(all_levels, all_levels), function(lv) {
    n  <- 0; s <- 0; ss <- 0; n_supp <- 0L
    for (sg in site_stats) {
      g <- sg[[lv]]
      if (is.null(g)) next
      if (isTRUE(g$suppressed)) { n_supp <- n_supp + 1L; next }
      n <- n + g$n; s <- s + g$sum; ss <- ss + g$sumsq
    }
    list(n = n, mean = if (n > 0) s / n else NA_real_,
         sd  = if (n > 1) sqrt((ss - s^2 / n) / (n - 1)) else NA_real_,
         sum = s, n_sites_suppressed = n_supp)
  })

  affected <- names(res)[vapply(res, function(e) e$n_sites_suppressed > 0L, logical(1))]
  if (length(affected) > 0) {
    total <- sum(vapply(res, function(e) e$n_sites_suppressed, integer(1)))
    warning(sprintf(
      paste0("Privacy suppression: %d small subgroup cell(s) hidden for '%s' by '%s' ",
             "(a site had fewer than its privacy threshold of patients in the group). ",
             "Affected group(s): %s. Pooled figures for these groups exclude the ",
             "withheld site(s)."),
      total, varname, groupvar, paste(affected, collapse = ", ")),
      call. = FALSE)
  }
  res
}

#' Federated Welch t-test
#'
#' Tests whether the mean of \code{varname} differs between two levels of
#' \code{groupvar} using Welch's (unequal-variance) t-test. Collects
#' per-group sufficient statistics from each site.
#'
#' @param servers List of server objects.
#' @param varname Character. Numeric outcome variable.
#' @param groupvar Character. Grouping variable.
#' @param group1 Value of \code{groupvar} defining the first group.
#' @param group2 Value of \code{groupvar} defining the second group.
#'
#' @return A list with \code{n1}, \code{n2}, \code{mean1}, \code{mean2},
#'   \code{sd1}, \code{sd2}, \code{t}, \code{df}, \code{p}.
#'
#' @export
fed_welch_t <- function(servers, varname, groupvar, group1, group2) {
  n1 <- s1 <- ss1 <- 0
  n2 <- s2 <- ss2 <- 0

  for (srv in servers) {
    stats <- srv$group_summaries(varname, groupvar)$stats
    a <- stats[[as.character(group1)]]
    b <- stats[[as.character(group2)]]
    # A requested group withheld for privacy at any site would silently bias
    # the test if we just skipped it. Refuse instead — never a wrong number.
    if (isTRUE(a$suppressed) || isTRUE(b$suppressed))
      stop(sprintf(
        paste0("Cannot compute the Welch t-test: at one site, group '%s' or '%s' has ",
               "fewer patients than the privacy threshold, so its statistics are ",
               "withheld. Returning a result would either expose an individual or ",
               "silently bias the test. Use larger/merged groups, or drop that site ",
               "explicitly."),
        group1, group2), call. = FALSE)
    if (!is.null(a)) { n1 <- n1 + a$n; s1 <- s1 + a$sum; ss1 <- ss1 + a$sumsq }
    if (!is.null(b)) { n2 <- n2 + b$n; s2 <- s2 + b$sum; ss2 <- ss2 + b$sumsq }
  }

  mean1 <- s1 / n1;  mean2 <- s2 / n2
  var1  <- (ss1 - s1^2 / n1) / (n1 - 1)
  var2  <- (ss2 - s2^2 / n2) / (n2 - 1)

  se <- sqrt(var1 / n1 + var2 / n2)
  t  <- (mean1 - mean2) / se
  df <- (var1 / n1 + var2 / n2)^2 /
    ((var1 / n1)^2 / (n1 - 1) + (var2 / n2)^2 / (n2 - 1))
  p  <- 2 * (1 - pt(abs(t), df = df))

  list(n1 = n1, n2 = n2, mean1 = mean1, mean2 = mean2,
       sd1 = sqrt(var1), sd2 = sqrt(var2),
       t = t, df = df, p = p)
}

#' Federated 2×2 chi-square test
#'
#' Aggregates a 2×2 contingency table across sites by pooling cell counts,
#' then applies \code{chisq.test}.
#'
#' @param servers List of server objects.
#' @param xvar Character. Binary (0/1) row variable.
#' @param yvar Character. Binary (0/1) column variable.
#' @param correct Logical. Yates' continuity correction (default \code{TRUE}).
#'
#' @return A list with \code{table} (2×2 matrix), \code{statistic},
#'   \code{df}, \code{p}, \code{N}.
#'
#' @export
fed_chisq_2x2 <- function(servers, xvar, yvar, correct = TRUE) {
  n00 <- n01 <- n10 <- n11 <- 0
  for (srv in servers) {
    r   <- srv$counts_2x2(xvar, yvar)
    # If a site withheld its table (a cell below the privacy threshold), the
    # pooled test cannot be formed without that site. Refuse rather than
    # compute on an incomplete table.
    if (isTRUE(r$suppressed))
      stop(sprintf(
        paste0("Cannot compute the 2x2 chi-square test: at one site a cell of the ",
               "%s x %s table is below the privacy threshold, so the whole table is ",
               "withheld (releasing three cells plus the total would reveal the ",
               "fourth). Use broader categories, or drop that site explicitly."),
        xvar, yvar), call. = FALSE)
    n00 <- n00 + r$n00; n01 <- n01 + r$n01
    n10 <- n10 + r$n10; n11 <- n11 + r$n11
  }
  tbl  <- matrix(c(n00, n01, n10, n11), nrow = 2, byrow = TRUE,
                 dimnames = list(paste0(xvar,  c("=0", "=1")),
                                 paste0(yvar, c("=0", "=1"))))
  test <- suppressWarnings(chisq.test(tbl, correct = correct))
  list(table     = tbl,
       statistic = unname(test$statistic),
       df        = unname(test$parameter),
       p         = unname(test$p.value),
       N         = sum(tbl))
}

#' Federated ordinary least-squares regression
#'
#' Computes the exact pooled OLS fit by aggregating sufficient statistics
#' (\eqn{X'X}, \eqn{X'y}, \eqn{y'y}) across sites. The result is
#' numerically identical to fitting on the full pooled dataset.
#'
#' @param servers List of server objects.
#' @param formula An R formula.
#'
#' @return A list with \code{coefficients}, \code{se}, \code{t}, \code{p},
#'   \code{sigma}, \code{df}, \code{N}, \code{vcov}.
#'
#' @export
fed_lm <- function(servers, formula) {
  XtX <- NULL; Xty <- NULL; yTy <- 0; N <- 0; termnames <- NULL

  for (srv in servers) {
    r <- srv$lm_suffstats(formula)
    if (is.null(termnames)) termnames <- r$termnames
    if (is.null(XtX)) {
      XtX <- r$XtX; Xty <- r$Xty
    } else {
      XtX <- XtX + r$XtX; Xty <- Xty + r$Xty
    }
    yTy <- yTy + r$yTy
    N   <- N   + r$n
  }

  beta   <- as.vector(solve(XtX, Xty)); names(beta) <- termnames
  p      <- length(beta)
  RSS    <- yTy - 2 * sum(beta * Xty) + as.numeric(t(beta) %*% XtX %*% beta)
  sigma2 <- RSS / (N - p)
  vcov   <- sigma2 * solve(XtX)
  se     <- sqrt(diag(vcov))
  tstat  <- beta / se
  pval   <- 2 * (1 - pt(abs(tstat), df = N - p))

  list(coefficients = beta, se = se, t = tstat, p = pval,
       sigma = sqrt(sigma2), df = N - p, N = N, vcov = vcov)
}

#' Federated logistic regression via distributed Newton-IRLS
#'
#' Fits a logistic regression model by iteratively aggregating the gradient
#' and Hessian of the binomial log-likelihood across sites. Converges to
#' the same coefficients as fitting on the full pooled dataset.
#'
#' Optionally computes cluster-robust (sandwich) standard errors treating
#' each site as one cluster.
#'
#' @param servers List of server objects.
#' @param formula An R formula with a binary (0/1) outcome.
#' @param max_iter Integer. Maximum Newton iterations (default \code{50}).
#' @param tol_score Numeric. Score-norm convergence threshold (default
#'   \code{1e-8}).
#' @param tol_loglik Numeric. Log-likelihood change convergence threshold
#'   (default \code{1e-10}).
#' @param robust_cluster Logical. Compute cluster-robust SEs when
#'   \code{length(servers) >= 2} (default \code{TRUE}).
#' @param verbose Logical. Print Newton iteration progress (default
#'   \code{FALSE}).
#'
#' @return A list with \code{coefficients}, \code{se}, \code{vcov},
#'   \code{logLik}, \code{N}, \code{iterations}, \code{converged}, and
#'   optionally \code{se_robust}, \code{vcov_robust}, \code{clusters}.
#'
#' @export
fed_logistic_newton <- function(servers, formula,
                                max_iter       = 50,
                                tol_score      = 1e-8,
                                tol_loglik     = 1e-10,
                                robust_cluster = TRUE,
                                verbose        = FALSE) {
  termnames <- servers[[1]]$termnames(formula)
  p    <- length(termnames)
  beta <- setNames(rep(0, p), termnames)

  federated_loglik <- function(b) {
    total <- 0
    for (srv in servers) total <- total + srv$grad_hess(formula, b)$ll
    total
  }

  loglik    <- federated_loglik(beta)
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    grad <- rep(0, p);  hess <- matrix(0, p, p)
    for (srv in servers) {
      r    <- srv$grad_hess(formula, beta)
      grad <- grad + r$grad
      hess <- hess + r$hess
    }
    score_norm <- sqrt(sum(grad^2))
    if (verbose) cat(sprintf("Newton iter %02d | score norm = %.3e\n", iter, score_norm))
    if (score_norm < tol_score) { converged <- TRUE; break }

    beta_new   <- beta - solve(hess, grad); names(beta_new) <- termnames
    loglik_new <- federated_loglik(beta_new)
    if (abs(loglik_new - loglik) < tol_loglik) {
      beta <- beta_new; loglik <- loglik_new; converged <- TRUE; break
    }
    beta <- beta_new; loglik <- loglik_new
  }

  hess        <- matrix(0, p, p)
  site_scores <- vector("list", length(servers))
  N           <- 0
  for (i in seq_along(servers)) {
    r              <- servers[[i]]$grad_hess(formula, beta)
    hess           <- hess + r$hess
    site_scores[[i]] <- as.numeric(r$grad)
    N              <- N + r$n
  }

  vcov <- solve(-hess); se <- sqrt(diag(vcov))
  out  <- list(coefficients = beta, se = se, vcov = vcov,
               logLik = federated_loglik(beta),
               N = N, iterations = iter, converged = converged)

  if (robust_cluster && length(servers) >= 2) {
    G    <- length(servers)
    meat <- matrix(0, p, p)
    for (u in site_scores) meat <- meat + tcrossprod(u)
    cr1           <- (G / (G - 1)) * ((N - 1) / (N - p))
    vcov_robust   <- cr1 * (vcov %*% meat %*% vcov)
    out$vcov_robust <- vcov_robust
    out$se_robust   <- sqrt(diag(vcov_robust))
    out$clusters    <- G
  }
  out
}
