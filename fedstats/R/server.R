# Internal helper: build model design matrix, dropping incomplete cases.
.build_design <- function(data, formula) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  list(
    X = model.matrix(formula, data = mf),
    y = as.numeric(model.response(mf))
  )
}

#' Create an in-process site server
#'
#' Wraps a data frame into a site server object that exposes only aggregate
#' statistics. This is the in-process equivalent of the HTTP API defined in
#' \code{api_server.R} and is primarily used for local simulation and
#' testing without running separate server processes.
#'
#' @param site_data A \code{data.frame} or a path to a CSV file.
#' @param min_n Integer. Minimum number of rows required. Sites with fewer
#'   rows throw an error (default \code{1}).
#' @param min_cell Integer. Privacy threshold for individual cells: any
#'   per-group summary cell or 2x2 contingency cell covering fewer than
#'   \code{min_cell} patients is withheld (never returned), because a small
#'   cell can reveal an individual patient's exact value. This is the
#'   statistical-disclosure-control "threshold rule" (default \code{5}, the
#'   most widely used value in medical data disclosure control). It is
#'   separate from \code{min_n} (whole-site participation) and cannot be
#'   lowered by the caller over the wire.
#' @param label Human-readable site name used in error messages. Defaults to
#'   \code{"site"}.
#'
#' @return A named list of functions with the same interface as
#'   \code{\link{create_remote_server}}: \code{termnames},
#'   \code{grad_hess}, \code{lm_suffstats}, \code{summary_numeric},
#'   \code{group_summaries}, \code{counts_2x2}, \code{validate_data}.
#'
#' @examples
#' \dontrun{
#' dat <- read.csv("data/sweden.csv")
#' srv <- create_server(dat)
#' srv$summary_numeric("age")
#' }
#'
#' @export
create_server <- function(site_data, min_n = 1, min_cell = 5, label = "site") {
  if (is.character(site_data)) {
    site_data <- read.csv(site_data, stringsAsFactors = FALSE,
                          na.strings = c("", "NA", "NaN", "NULL"))
  }
  if (!is.data.frame(site_data))
    stop("create_server(): site_data must be a data.frame or a CSV path.")
  df <- site_data
  min_cell <- max(1L, as.integer(min_cell))   # a threshold of 0 would disable protection
  if (nrow(df) < min_n)
    stop(sprintf("Site has fewer than %d rows and cannot participate.", min_n))

  list(
    label = label,

    # ---- design-matrix term names ----
    termnames = function(formula) {
      colnames(.build_design(df, formula)$X)
    },

    # ---- logistic: gradient, Hessian, log-likelihood ----
    grad_hess = function(formula, beta) {
      d   <- .build_design(df, formula)
      X   <- d$X; y <- d$y
      if (length(y) < min_n)
        stop(sprintf(
          "Query covers only %d complete case(s); minimum is %d. ",
          length(y), min_n
        ))
      eta <- as.vector(X %*% beta)
      mu  <- 1 / (1 + exp(-eta))
      w   <- mu * (1 - mu)
      list(
        n    = length(y),
        grad = as.vector(crossprod(X, y - mu)),
        hess = -crossprod(X, X * w),
        ll   = sum(y * log(mu + 1e-12) + (1 - y) * log(1 - mu + 1e-12))
      )
    },

    # ---- linear: sufficient statistics (X'X, X'y, y'y) ----
    lm_suffstats = function(formula) {
      d <- .build_design(df, formula)
      X <- d$X; y <- d$y
      if (length(y) < min_n)
        stop(sprintf(
          "Query covers only %d complete case(s); minimum is %d. ",
          length(y), min_n
        ))
      list(
        n         = length(y),
        termnames = colnames(X),
        XtX       = crossprod(X),
        Xty       = as.vector(crossprod(X, y)),
        yTy       = sum(y * y)
      )
    },

    # ---- numeric summary: n, sum, sum-of-squares ----
    summary_numeric = function(varname) {
      x <- suppressWarnings(as.numeric(df[[varname]]))
      x <- x[!is.na(x)]
      if (length(x) < min_n)
        stop(sprintf(
          "Query covers only %d non-missing value(s); minimum is %d. ",
          length(x), min_n
        ))
      list(type = "numeric", n = length(x), sum = sum(x), sumsq = sum(x * x))
    },

    # ---- per-group numeric summary ----
    group_summaries = function(varname, groupvar) {
      x  <- suppressWarnings(as.numeric(df[[varname]]))
      g  <- as.character(df[[groupvar]])
      ok <- !(is.na(x) | is.na(g))
      x  <- x[ok]; g <- g[ok]
      if (length(x) < min_n)
        stop(sprintf(
          "Query covers only %d complete case(s); minimum is %d. ",
          length(x), min_n
        ))
      lvls  <- sort(unique(g))
      stats <- lapply(lvls, function(lv) {
        xi <- x[g == lv]
        # Per-group privacy gate: a group of, say, 1 patient would return
        # sum = that patient's exact value. Withhold the whole cell (no n,
        # no sum, no sumsq) — only the level name and a flag leave the site.
        if (length(xi) < min_cell)
          list(suppressed = TRUE)
        else
          list(n = length(xi), sum = sum(xi), sumsq = sum(xi * xi))
      })
      names(stats) <- lvls
      list(type = "group_numeric", stats = stats, min_cell = min_cell)
    },

    # ---- 2x2 contingency counts ----
    counts_2x2 = function(xvar, yvar) {
      x  <- suppressWarnings(as.numeric(df[[xvar]]))
      y  <- suppressWarnings(as.numeric(df[[yvar]]))
      ok <- !(is.na(x) | is.na(y))
      x  <- x[ok]; y <- y[ok]
      if (length(x) < min_n)
        stop(sprintf(
          "Query covers only %d complete case(s); minimum is %d. ",
          length(x), min_n
        ))
      if (any(!x %in% c(0, 1)) || any(!y %in% c(0, 1)))
        stop("counts_2x2(): both variables must be binary (0/1).")
      cells <- c(n00 = sum(x == 0 & y == 0), n01 = sum(x == 0 & y == 1),
                 n10 = sum(x == 1 & y == 0), n11 = sum(x == 1 & y == 1))
      # If ANY non-empty cell is below the privacy threshold, withhold the
      # ENTIRE table. Suppressing just the small cell is useless: it can be
      # recovered by subtraction from the row/column margins and the total.
      # (A genuine zero cell is not disclosive, so it doesn't trigger this.)
      if (any(cells > 0 & cells < min_cell))
        return(list(type = "2x2", suppressed = TRUE,
                    n = length(x), min_cell = min_cell))
      list(type = "2x2",
           n00 = unname(cells["n00"]), n01 = unname(cells["n01"]),
           n10 = unname(cells["n10"]), n11 = unname(cells["n11"]),
           n   = length(x))
    },

    # ---- pre-analysis data validation ----
    # `min_n_formula` is the caller's recommended complete-case count for the
    # formula feasibility flag only. All privacy gating below uses the
    # server's own min_n / min_cell (captured in this closure), NEVER a
    # caller-supplied value — so a client cannot lower the threshold to
    # unlock small-sample values over the wire.
    validate_data = function(vars_spec, formula = NULL, min_n_formula = 20L) {
      # Order statistics (quartiles/median) can coincide with an individual's
      # value, so they need a comfortably large sample before release.
      quant_gate <- max(min_n, min_cell)

      var_reports <- lapply(names(vars_spec), function(vname) {
        spec  <- vars_spec[[vname]]
        vtype <- spec$type
        vrep  <- list(variable = vname, type = vtype, exists = FALSE)

        if (!vname %in% names(df)) return(vrep)
        vrep$exists <- TRUE

        col <- df[[vname]]
        n_total   <- length(col)
        n_missing <- sum(is.na(col))
        vrep$n_total    <- n_total
        vrep$n_missing  <- n_missing
        vrep$pct_missing <- round(100 * n_missing / n_total, 1)

        if (vtype %in% c("numeric", "binary")) {
          x         <- suppressWarnings(as.numeric(col))
          n_coerced <- sum(is.na(x)) - n_missing
          if (n_coerced > 0)
            vrep$type_warning <- paste0(n_coerced, " non-numeric value(s) coerced to NA.")
          x_valid      <- x[!is.na(x)]
          vrep$n_valid <- length(x_valid)
          if (length(x_valid) > 0) {
            # For a binary variable, a tiny class makes even the mean
            # disclosive (mean * n recovers the small class count), so treat
            # a small class the same as a small sample.
            small_binary <- FALSE
            if (vtype == "binary") {
              n0 <- sum(x_valid == 0); n1 <- sum(x_valid == 1)
              small_binary <- (n0 > 0 && n0 < min_cell) ||
                              (n1 > 0 && n1 < min_cell)
            }
            # Aggregate summaries only when enough patients back them — a
            # mean/sd over 1-2 people is effectively an individual value.
            # min/max are always the exact values of specific patients and
            # are never returned.
            if (length(x_valid) >= min_cell && !small_binary) {
              vrep$mean <- round(mean(x_valid), 4)
              vrep$sd   <- if (length(x_valid) > 1) round(sd(x_valid), 4) else NA_real_
            } else {
              vrep$summary_suppressed <- TRUE
            }
            if (length(x_valid) >= quant_gate && !small_binary) {
              vrep$q25    <- unname(quantile(x_valid, 0.25))
              vrep$median <- unname(median(x_valid))
              vrep$q75    <- unname(quantile(x_valid, 0.75))
            }
            if (!is.null(spec$min) && !is.null(spec$max)) {
              oor_mask            <- x_valid < spec$min | x_valid > spec$max
              n_oor               <- sum(oor_mask)
              vrep$expected_range <- c(spec$min, spec$max)
              vrep$n_out_of_range <- n_oor
              if (n_oor > 0)
                # Report only HOW MANY values are out of range, never the
                # values themselves — an out-of-range value is exactly the
                # kind of unique outlier that identifies a patient.
                vrep$range_warning <- paste0(
                  n_oor, " value(s) outside expected range [",
                  spec$min, ", ", spec$max, "].")
            }
            if (vtype == "binary") {
              n_invalid <- sum(!x_valid %in% c(0, 1))
              # Class counts are disclosive when a class is tiny; release
              # them only when both classes clear the threshold.
              if (!small_binary) { vrep$n0 <- n0; vrep$n1 <- n1 }
              else vrep$counts_suppressed <- TRUE
              if (n_invalid > 0)
                vrep$binary_error <- paste0(n_invalid, " value(s) not in {0, 1}.")
            }
          }
        } else if (vtype == "categorical") {
          col_char  <- as.character(col)
          col_valid <- col_char[!is.na(col) & nzchar(col_char) & col_char != "NA"]
          tab        <- table(col_valid)
          found_lvls <- sort(unique(col_valid))
          # Report which levels exist, but withhold the COUNT of any level
          # below the threshold — a level of size 1-4 pinpoints individuals.
          small <- tab > 0 & tab < min_cell
          vrep$levels_found        <- as.list(found_lvls)
          vrep$level_counts        <- as.list(tab[!small])
          vrep$n_levels_suppressed <- as.integer(sum(small))
          if (!is.null(spec$levels)) {
            exp_lvls <- unlist(spec$levels)
            unexp    <- setdiff(found_lvls, exp_lvls)
            miss_lvl <- setdiff(exp_lvls, found_lvls)
            if (length(unexp) > 0)
              vrep$level_warning <- paste0("Unexpected level(s): ",
                                           paste(unexp, collapse = ", "))
            if (length(miss_lvl) > 0)
              vrep$missing_levels <- paste0("Expected level(s) absent: ",
                                            paste(miss_lvl, collapse = ", "))
          }
        }
        vrep
      })
      names(var_reports) <- names(vars_spec)

      formula_report <- NULL
      if (!is.null(formula)) {
        formula_report <- tryCatch({
          d  <- .build_design(df, formula)
          nc <- length(d$y)
          list(formula    = paste(deparse(formula), collapse = ""),
               n_complete = nc,
               feasible   = nc > 0,
               low_n      = nc > 0 && nc < as.integer(min_n_formula))
        }, error = function(e) {
          list(formula  = paste(deparse(formula), collapse = ""),
               feasible = FALSE, error = conditionMessage(e))
        })
      }

      list(n_rows = nrow(df), var_reports = var_reports,
           formula_report = formula_report)
    }
  )
}
