# test_privacy_suite.R
# =====================================================================
# END-TO-END TEST OF THE PRIVACY AND VALIDATION LAYERS
#
# This is not a clinical analysis — it is a deliberate attempt to make
# the federation leak something, and a check that every layer refuses.
# Run it against data/edge/, which contains fictitious patients with
# defects and tiny subgroups planted on purpose:
#
#     Rscript data/make_test_sites.R        # (re)generate the CSVs
#     Rscript analysis/test_privacy_suite.R
#
# Against live sites over Tailscale:
#
#     FED_SITE_URLS="http://100.x.x.x:8001,http://100.x.x.x:8002" \
#     FED_API_TOKEN="..." Rscript analysis/test_privacy_suite.R
#
# Or simply load it in the coordinator GUI and press Run.
#
# Every check prints PASS / FAIL and lands in the "Security checks" tab.
# A check named "must refuse" PASSES when the engine raises an error:
# refusing to answer is the correct behaviour.
#
# The layers under test
# ---------------------------------------------------------------------
#   A  min_n     — a site refuses to answer at all below 20 patients
#   B  min_cell  — an individual cell below 5 patients is never returned
#   C  disclosure control in the validation report (no min/max, no
#                  outlier values, no tiny class counts, no quartiles
#                  from small samples), and the threshold cannot be
#                  lowered by the coordinator over the wire
#   D  data-quality gates (missing column, bad binary, free text,
#                  missing data, cross-site heterogeneity)
#   E  correctness — the federated result must equal the pooled result
#                  exactly, and a failing site must be named
# =====================================================================

library(fedstats)

ANALYSIS_TITLE <- "Privacy & Security Test Suite (fictitious data)"

# ---------------------------------------------------------------------
# Variables under test
# ---------------------------------------------------------------------
VARS_SPEC <- list(
  age              = list(type = "numeric",  min = 18, max = 100),
  sexM             = list(type = "binary"),
  bmi              = list(type = "numeric",  min = 10, max = 80),
  smoker           = list(type = "binary"),        # 2 smokers at Turku
  diag_grp         = list(type = "categorical",
                          levels = c("radiculopathy", "myelopathy")),
  diag_myelo       = list(type = "binary"),
  sick_leave_preop = list(type = "binary"),        # one value coded 2 at Turku
  nrs_arm_preop    = list(type = "numeric",  min = 0,  max = 10),
  ndi_index_preop  = list(type = "numeric",  min = 0,  max = 100),
  eq5d_vas_preop   = list(type = "numeric",  min = 0,  max = 100),  # free text at Oslo
  pmjoa_index_preop = list(type = "numeric", min = 0,  max = 18),   # absent at Oslo
  any_complication = list(type = "binary"),        # 3 events at Turku
  mcid_arm_12m     = list(type = "binary"),
  delta_nrs_arm_12m = list(type = "numeric", min = -10, max = 10),
  ndi_index_24m    = list(type = "numeric",  min = 0,  max = 100)   # 12 values at Turku
)

# Models. Both use numeric predictors only: the federated protocol
# transmits a design matrix, so a categorical predictor whose levels
# differ between sites would produce incompatible matrices (check E3).
MODEL_LM       <- delta_nrs_arm_12m ~ age + sexM + diag_myelo + nrs_arm_preop + bmi
MODEL_LOGISTIC <- mcid_arm_12m      ~ age + sexM + diag_myelo + nrs_arm_preop
MODEL_BAD      <- nrs_arm_12m       ~ nrs_arm_preop + diag_grp   # 3 levels at Oslo only

# ---------------------------------------------------------------------
# Server setup
# ---------------------------------------------------------------------
# The GUI injects `servers`. Standalone, we read the CSVs directly and
# apply the SAME thresholds the real site server uses (api_server.R
# defaults: FED_MIN_N = 20, FED_MIN_CELL = 5), so a local run tests the
# same gates as a live federation.
MIN_N    <- as.integer(Sys.getenv("FED_MIN_N",    "20"))
MIN_CELL <- as.integer(Sys.getenv("FED_MIN_CELL", "5"))
DATA_DIR <- Sys.getenv("FED_DATA_DIR", file.path("data", "edge"))

LOCAL_CSVS <- character(0)   # set only in local mode; enables the pooled cross-check

if (!exists("servers", inherits = FALSE)) {
  .urls <- trimws(strsplit(Sys.getenv("FED_SITE_URLS", ""), ",")[[1]])
  .urls <- .urls[nzchar(.urls)]

  if (length(.urls) > 0) {
    .tok    <- Sys.getenv("FED_API_TOKEN", "")
    servers <- lapply(.urls, function(u)
      create_remote_server(u, token = if (nzchar(.tok)) .tok else NULL, label = u))
    cat(sprintf("Connected to %d remote site(s).\n\n", length(servers)))
  } else {
    LOCAL_CSVS <- sort(list.files(DATA_DIR, pattern = "[.]csv$", full.names = TRUE))
    if (!length(LOCAL_CSVS))
      stop("No sites found. Run: Rscript data/make_test_sites.R  (or set FED_SITE_URLS).")
    servers <- lapply(LOCAL_CSVS, function(f)
      create_server(f, min_n = MIN_N, min_cell = MIN_CELL,
                    label = tools::file_path_sans_ext(basename(f))))
    cat(sprintf("Loaded %d local site(s) from %s/  (min_n = %d, min_cell = %d)\n\n",
                length(servers), DATA_DIR, MIN_N, MIN_CELL))
  }
}

SITE_LABELS <- vapply(servers, function(s) s$label, character(1))
clear_outputs()

# ---------------------------------------------------------------------
# Test harness
# ---------------------------------------------------------------------
.results <- list()

# Evaluate an expression, capturing both its value and any warnings,
# without letting either abort the suite.
attempt <- function(expr) {
  warns <- character(0)
  out <- withCallingHandlers(
    tryCatch(list(ok = TRUE, value = expr, error = NA_character_),
             error = function(e) list(ok = FALSE, value = NULL,
                                      error = conditionMessage(e))),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  out$warnings <- warns
  out
}

check <- function(id, layer, what, pass, detail = "") {
  .results[[length(.results) + 1L]] <<- data.frame(
    Check = id, Layer = layer, What = what,
    Result = if (isTRUE(pass)) "PASS" else "FAIL",
    Detail = detail, stringsAsFactors = FALSE
  )
  cat(sprintf("  [%s] %-4s %s\n", if (isTRUE(pass)) "PASS" else "FAIL", id, what))
  if (nzchar(detail)) cat(sprintf("         %s\n", detail))
  invisible(isTRUE(pass))
}

# Shorten a long engine message for the report table.
brief <- function(x, n = 110) {
  x <- gsub("[\r\n]+", " ", paste(x, collapse = " "))
  if (nchar(x) > n) paste0(substr(x, 1, n), "...") else x
}

# =====================================================================
# LAYER A — min_n: the whole-site participation floor
# =====================================================================
cat("A. Site floor (min_n)\n")

# A1: a site holding fewer than min_n patients cannot even be started.
if (length(LOCAL_CSVS)) {
  tiny <- read.csv(LOCAL_CSVS[1], nrows = 12)
  r <- attempt(create_server(tiny, min_n = MIN_N, min_cell = MIN_CELL, label = "tiny"))
  check("A1", "min_n", "a site below the floor cannot participate at all",
        !r$ok && grepl("cannot participate", r$error), brief(r$error))
}

# A2: a query whose complete cases fall below the floor is refused, and
#     the error names the site that refused (Turku holds 12 values here).
r <- attempt(fed_numeric(servers, "ndi_index_24m"))
check("A2", "min_n", "a query below the floor is refused, naming the site",
      !r$ok && grepl("minimum is", r$error) && grepl("^Site '", r$error),
      brief(r$error))

# =====================================================================
# LAYER B — min_cell: small-cell suppression
# =====================================================================
cat("\nB. Small-cell suppression (min_cell)\n")

# B1: pooling per-group means must warn and exclude the withholding site,
#     never silently pool a partial group.
r <- attempt(fed_group_numeric(servers, "nrs_arm_preop", "diag_grp"))
grp <- r$value
supp <- if (r$ok) vapply(grp, function(g) g$n_sites_suppressed, integer(1)) else integer(0)
check("B1", "min_cell", "small subgroups are withheld and the pooling warns",
      r$ok && any(supp > 0) && length(r$warnings) > 0,
      brief(r$warnings))

# B2: the withheld cell must carry no numbers at all — not n, not sum,
#     not sum of squares. Only the level name and a flag may leave.
raw_grp <- lapply(servers, function(s)
  attempt(s$group_summaries("nrs_arm_preop", "diag_grp"))$value)
supp_cells <- unlist(lapply(raw_grp, function(rg)
  lapply(rg$stats, function(z) if (isTRUE(z$suppressed)) names(z) else NULL)),
  use.names = FALSE)
check("B2", "min_cell", "a withheld cell carries no n / sum / sumsq",
      length(supp_cells) > 0 && all(supp_cells == "suppressed"),
      sprintf("%d withheld cell(s); fields returned: {%s}",
              length(supp_cells), paste(unique(supp_cells), collapse = ", ")))

# B3: a t-test that would need a withheld group must refuse. Returning a
#     number computed from the remaining sites would be silently biased.
r <- attempt(fed_welch_t(servers, "nrs_arm_preop", "diag_grp",
                         "radiculopathy", "myelopathy"))
check("B3", "min_cell", "Welch t on a withheld group must refuse",
      !r$ok && grepl("privacy threshold", r$error), brief(r$error))

# B4: ...but the same test on adequately sized groups must still work.
#     A blanket refusal would be useless, so this is the control.
r_welch <- attempt(fed_welch_t(servers, "ndi_index_preop", "sexM", 1, 0))
w <- r_welch$value
check("B4", "min_cell", "Welch t on adequate groups still works (control)",
      r_welch$ok && is.finite(w$p),
      if (r_welch$ok)
        sprintf("men n=%d mean=%.1f vs women n=%d mean=%.1f | t=%.2f, p=%.4f",
                w$n1, w$mean1, w$n2, w$mean2, w$t, w$p) else brief(r_welch$error))

# B5: one small cell poisons the whole 2x2 table — releasing the other
#     three plus the total would give the small one away by subtraction.
r <- attempt(fed_chisq_2x2(servers, "diag_myelo", "any_complication"))
check("B5", "min_cell", "2x2 with a small cell: the whole table is withheld",
      !r$ok && grepl("privacy threshold", r$error), brief(r$error))

# B6: control — a 2x2 whose cells all clear the threshold must work.
r_chi <- attempt(fed_chisq_2x2(servers, "sexM", "mcid_arm_12m"))
ch <- r_chi$value
check("B6", "min_cell", "2x2 with adequate cells still works (control)",
      r_chi$ok && is.finite(ch$p),
      if (r_chi$ok) sprintf("N=%d, chi2=%.2f, df=%d, p=%.4f",
                            ch$N, ch$statistic, ch$df, ch$p) else brief(r_chi$error))

# B7: the withheld table must arrive with no cell counts on it.
raw_2x2 <- lapply(servers, function(s)
  attempt(s$counts_2x2("diag_myelo", "any_complication"))$value)
supp_tables <- Filter(function(z) isTRUE(z$suppressed), raw_2x2)
cells_leaked <- any(vapply(supp_tables, function(z)
  any(c("n00", "n01", "n10", "n11") %in% names(z)), logical(1)))
check("B7", "min_cell", "a withheld 2x2 returns no cell counts",
      length(supp_tables) > 0 && !cells_leaked,
      sprintf("%d site(s) withheld the table; fields returned: {%s}",
              length(supp_tables),
              paste(unique(unlist(lapply(supp_tables, names))), collapse = ", ")))

# =====================================================================
# LAYER C — disclosure control inside the validation report
# =====================================================================
cat("\nC. Disclosure control in the validation report\n")

val <- fed_validate(servers, VARS_SPEC, formula = MODEL_LM, min_n = MIN_N)
print_validation_report(val)

reports <- val$site_reports
vrep <- function(site_idx, var) reports[[site_idx]]$var_reports[[var]]

# C1: no exact minimum, maximum or raw value may appear anywhere in the
#     report. Those are always some individual patient's own number.
#     Walk the entire returned structure and look for such a field.
FORBIDDEN <- c("min", "max", "minimum", "maximum", "values", "raw",
               "outliers", "rows", "ids")
scan_names <- function(x) {
  if (!is.list(x)) return(character(0))
  c(names(x), unlist(lapply(x, scan_names), use.names = FALSE))
}
leaked <- intersect(tolower(scan_names(reports)), FORBIDDEN)
check("C1", "disclosure", "no exact min/max or raw value anywhere in the report",
      length(leaked) == 0,
      if (length(leaked)) paste("LEAKED FIELDS:", paste(leaked, collapse = ", "))
      else "scanned every field of every site report")

# C2: out-of-range values never leak exact values; tiny outlier counts are
#     also suppressed. (Oslo holds three impossible ages, 15/16/16.)
oor_count <- vapply(seq_along(reports), function(i) {
  v <- vrep(i, "age")
  if (is.null(v$n_out_of_range)) 0L else as.integer(unlist(v$n_out_of_range))
}, integer(1))
oor_flag <- vapply(seq_along(reports), function(i) {
  v <- vrep(i, "age")
  isTRUE(unlist(v$out_of_range_detected))
}, logical(1))
check("C2", "disclosure", "out-of-range values flagged without leaking tiny outlier counts",
      any(oor_count > 0 | oor_flag),
      if (any(oor_flag))
        "Out-of-range ages were detected and the tiny count was suppressed"
      else
        sprintf("%d out-of-range age(s) flagged; the offending values never left the site",
                sum(oor_count)))

# C3: a binary variable with a tiny class: mean and class counts are both
#     withheld (mean x n would recover the small class exactly).
sm <- Filter(function(v) isTRUE(unlist(v$counts_suppressed)),
             lapply(seq_along(reports), function(i) vrep(i, "smoker")))
sm_has_numbers <- any(vapply(sm, function(v)
  any(c("n0", "n1", "mean") %in% names(v)), logical(1)))
check("C3", "disclosure", "tiny binary class: mean and class counts withheld",
      length(sm) > 0 && !sm_has_numbers,
      sprintf("%d site(s) withheld the smoker class counts", length(sm)))

# C4: quartiles are order statistics — they can coincide exactly with an
#     individual's value, so they need a bigger sample than the mean does.
#     Turku has 12 values of ndi_index_24m: mean released, quartiles not.
#     (`gated` also guards against this check passing vacuously, i.e. with
#     no site actually below the gate.)
gate  <- max(MIN_N, MIN_CELL)
gated <- 0L
q_ok  <- vapply(seq_along(reports), function(i) {
  v  <- vrep(i, "ndi_index_24m")
  nv <- if (is.null(v$n_valid)) NA_integer_ else as.integer(unlist(v$n_valid))
  if (is.na(nv) || nv >= gate) return(TRUE)                   # not a small sample
  gated <<- gated + 1L
  is.null(v$median) && is.null(v$q25) && is.null(v$q75)       # must be withheld
}, logical(1))
check("C4", "disclosure", "quartiles withheld for small samples (mean still given)",
      gated > 0 && all(q_ok),
      sprintf("%d site(s) fell below the order-statistic gate (n < %d) and returned no median/q25/q75",
              gated, gate))

# C5: a rare category: the count is withheld, and the rare level need not be named.
lv <- lapply(seq_along(reports), function(i) vrep(i, "diag_grp"))
n_sup_lv <- sum(vapply(lv, function(v)
  if (is.null(v$n_levels_suppressed)) 0L else as.integer(unlist(v$n_levels_suppressed)),
  integer(1)))
rare_name_leaked <- any(vapply(lv, function(v)
  "rare" %in% unlist(v$levels_present_safe, use.names = FALSE), logical(1)))
check("C5", "disclosure", "rare category: count withheld and rare levels can stay hidden",
      n_sup_lv > 0 && !rare_name_leaked,
      sprintf("%d level count(s) withheld across sites", n_sup_lv))

# C6: the coordinator asks the site to lower its threshold to 1 — the
#     site's own min_cell must still apply. This is the important one:
#     the threshold is the site's setting, not a request parameter.
lowered <- lapply(servers, function(s)
  attempt(s$validate_data(VARS_SPEC, NULL, 1L))$value)
still_suppressed <- any(vapply(lowered, function(rpt) {
  v <- rpt$var_reports[["smoker"]]
  isTRUE(unlist(v$counts_suppressed)) || isTRUE(unlist(v$summary_suppressed))
}, logical(1)))
check("C6", "disclosure", "coordinator cannot lower the threshold over the wire",
      still_suppressed,
      "asked every site for min_n = 1; the small classes stayed withheld")

# =====================================================================
# LAYER D — data-quality gates
# =====================================================================
cat("\nD. Data-quality gates\n")

has_err  <- function(pat) any(grepl(pat, val$errors,   ignore.case = TRUE))
has_warn <- function(pat) any(grepl(pat, val$warnings, ignore.case = TRUE))

check("D1", "validation", "a missing column is a blocking error",
      has_err("not found"),
      brief(grep("not found", val$errors, value = TRUE)))

check("D2", "validation", "a binary variable holding a 2 is a blocking error",
      has_err("not in \\{0, 1\\}"),
      brief(grep("not in", val$errors, value = TRUE)))

check("D3", "validation", "free text in a numeric column is flagged",
      has_warn("coerced"),
      brief(grep("coerced", val$warnings, value = TRUE)))

check("D4", "validation", "a mostly-missing variable is flagged",
      has_warn("% missing"),
      brief(grep("% missing", val$warnings, value = TRUE)))

check("D5", "validation", "an impossible value is flagged (count only)",
      has_warn("outside expected range"),
      brief(grep("outside expected range", val$warnings, value = TRUE)))

i2 <- val$heterogeneity[["eq5d_vas_preop"]]
check("D6", "validation", "cross-site heterogeneity is measured and flagged",
      !is.null(i2) && is.finite(i2$i2),
      if (!is.null(i2))
        sprintf("eq5d_vas_preop: I2 = %.1f%%, site means [%s]",
                i2$i2, paste(sprintf("%.1f", i2$site_means), collapse = ", "))
      else "not computed")

# =====================================================================
# LAYER E — correctness of the federated result
# =====================================================================
cat("\nE. Correctness (federated must equal pooled, exactly)\n")

r_lm  <- attempt(fed_lm(servers, MODEL_LM))
fit   <- r_lm$value
r_glm <- attempt(fed_logistic_newton(servers, MODEL_LOGISTIC, verbose = FALSE))
glmf  <- r_glm$value

if (length(LOCAL_CSVS)) {
  # Only possible in local mode: pool the raw rows ourselves and compare.
  # This is exactly what the federation must never require in production.
  pooled <- do.call(rbind, lapply(LOCAL_CSVS, function(f) {
    d <- read.csv(f, na.strings = c("", "NA", "NaN", "NULL"))
    d[, all.vars(MODEL_LM)]
  }))
  ref_lm <- lm(MODEL_LM, data = pooled)
  d_lm   <- max(abs(coef(ref_lm) - fit$coefficients))
  check("E1", "correctness", "fed_lm equals lm() on the pooled data",
        r_lm$ok && d_lm < 1e-8,
        sprintf("largest coefficient difference = %.2e over %d terms, N = %d",
                d_lm, length(fit$coefficients), fit$N))

  pooled_g <- do.call(rbind, lapply(LOCAL_CSVS, function(f) {
    d <- read.csv(f, na.strings = c("", "NA", "NaN", "NULL"))
    d[, all.vars(MODEL_LOGISTIC)]
  }))
  ref_glm <- glm(MODEL_LOGISTIC, data = pooled_g, family = binomial())
  d_glm   <- max(abs(coef(ref_glm) - glmf$coefficients))
  check("E2", "correctness", "fed_logistic_newton equals glm() on the pooled data",
        r_glm$ok && glmf$converged && d_glm < 1e-6,
        sprintf("largest coefficient difference = %.2e after %d Newton iterations, N = %d",
                d_glm, glmf$iterations, glmf$N))
} else {
  check("E1", "correctness", "fed_lm runs against the live sites",
        r_lm$ok, if (r_lm$ok) sprintf("N = %d", fit$N) else brief(r_lm$error))
  check("E2", "correctness", "fed_logistic_newton converges against the live sites",
        r_glm$ok && isTRUE(glmf$converged),
        if (r_glm$ok) sprintf("N = %d, %d iterations", glmf$N, glmf$iterations)
        else brief(r_glm$error))
}

# E3: the design matrix must mean the same thing at every site. A factor
#     with an extra level at one site (Oslo's "ossification") silently
#     widens its matrix — fed_lm would then pool incompatible columns.
#     Check the term names agree BEFORE trusting any pooled model.
tn <- lapply(servers, function(s) attempt(s$termnames(MODEL_BAD))$value)
agree <- length(unique(lapply(tn, function(x) sort(unlist(x))))) == 1L
check("E3", "correctness", "term-name mismatch across sites is detected",
      !agree,   # with the planted data the sites MUST disagree
      sprintf("caught as designed: %s",
              paste(vapply(seq_along(tn), function(i)
                sprintf("%s=%d terms", SITE_LABELS[i], length(tn[[i]])),
                character(1)), collapse = ", ")))

# E4: when a site fails, the error must say WHICH site.
r <- attempt(fed_numeric(servers, "a_column_that_does_not_exist"))
check("E4", "correctness", "a failing call names the site responsible",
      !r$ok && grepl("^Site '", r$error), brief(r$error))

# E5: cluster-robust standard errors treat each site as a cluster.
check("E5", "correctness", "cluster-robust SEs computed across sites",
      r_glm$ok && !is.null(glmf$se_robust),
      if (r_glm$ok && !is.null(glmf$se_robust))
        sprintf("%d clusters; model SE %.3f vs robust SE %.3f (intercept)",
                glmf$clusters, glmf$se[1], glmf$se_robust[1]) else "not computed")

# =====================================================================
# Report
# =====================================================================
res <- do.call(rbind, .results)
n_pass <- sum(res$Result == "PASS")

cat(sprintf("\n=====================================================\n"))
cat(sprintf(" %d / %d checks passed\n", n_pass, nrow(res)))
if (n_pass < nrow(res)) {
  cat(" FAILED:\n")
  for (i in which(res$Result == "FAIL"))
    cat(sprintf("   %s  %s\n", res$Check[i], res$What[i]))
}
cat(sprintf("=====================================================\n"))

register_output("Security checks", res, "table", sprintf(
  "%d / %d checks passed. A 'must refuse' check passes when the engine raises an error.",
  n_pass, nrow(res)))

register_output("Validation", paste(
  c(sprintf("Sites: %d  |  Status: %s", length(val$site_reports),
            if (val$ok) "PASS" else "FAIL (see errors below)"),
    "",
    if (length(val$errors))   c("Blocking errors:", paste0("  - ", val$errors)),
    if (length(val$warnings)) c("", "Warnings:", paste0("  - ", val$warnings))),
  collapse = "\n"),
  "text",
  "Pre-analysis validation. With the edge-case data, errors here are the expected result.")

# What each site actually sent back, in plain words. This is the whole
# privacy claim, made auditable.
register_output("What left each site", paste(c(
  "For every query in this suite, the only things transmitted by a site were:",
  "",
  "  summary_numeric   n, sum, sum of squares         (3 numbers per variable)",
  "  group_summaries   n, sum, sum of squares per group, or {suppressed} if < min_cell",
  "  counts_2x2        4 cell counts, or {suppressed} if any cell < min_cell",
  "  lm_suffstats      X'X, X'y, y'y                  (a p x p matrix, p >= 1 vectors)",
  "  grad_hess         gradient, Hessian, log-likelihood at a given beta",
  "  validate_data     counts, %missing, mean, sd, quartiles (all threshold-gated)",
  "",
  "Never transmitted, at any point: a patient row, an identifier, an exact",
  "minimum or maximum, the value of an out-of-range observation, or the count",
  "of any group smaller than the site's own min_cell.",
  "",
  sprintf("Thresholds in force for this run: min_n = %d, min_cell = %d.", MIN_N, MIN_CELL),
  "Both are the site's own settings. The coordinator cannot lower them (check C6)."),
  collapse = "\n"), "text", "Data-flow audit")

# Pooled models, for the record.
if (r_lm$ok) {
  lm_tbl <- data.frame(
    Term        = names(fit$coefficients),
    Coefficient = round(fit$coefficients, 4),
    SE          = round(fit$se, 4),
    t           = round(fit$t, 2),
    p           = signif(fit$p, 3),
    row.names   = NULL, stringsAsFactors = FALSE
  )
  register_output("Linear model", lm_tbl, "table", sprintf(
    "Change in arm pain at 12 months. Federated OLS, N = %d, pooled across %d sites.",
    fit$N, length(servers)))
}

if (r_glm$ok) {
  or  <- exp(glmf$coefficients)
  lo  <- exp(glmf$coefficients - 1.96 * glmf$se)
  hi  <- exp(glmf$coefficients + 1.96 * glmf$se)
  glm_tbl <- data.frame(
    Term       = names(glmf$coefficients),
    OR         = round(or, 3),
    `95% CI`   = sprintf("%.2f - %.2f", lo, hi),
    p          = signif(2 * (1 - pnorm(abs(glmf$coefficients / glmf$se))), 3),
    check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE
  )
  register_output("Logistic model", glm_tbl, "table", sprintf(
    "Odds of reaching the arm-pain MCID at 12 months. Federated Newton-IRLS, N = %d.",
    glmf$N))

  register_output("Odds ratios", function() {
    k   <- seq_along(or)[-1]                       # drop the intercept
    op  <- par(mar = c(4.5, 11, 3, 2)); on.exit(par(op))
    rng <- range(c(lo[k], hi[k]), na.rm = TRUE)
    plot(or[k], k, xlim = rng, ylim = c(0.5, max(k) + 0.5), log = "x",
         pch = 19, yaxt = "n", xlab = "Odds ratio (log scale)", ylab = "",
         main = "Reaching the arm-pain MCID at 12 months")
    segments(lo[k], k, hi[k], k, lwd = 2)
    abline(v = 1, lty = 2, col = "grey50")
    axis(2, at = k, labels = names(or)[k], las = 1)
  }, "plot", "Federated odds ratios with 95% confidence intervals")
}

# Heterogeneity, which is what actually decides whether pooling is honest.
if (length(val$heterogeneity)) {
  register_output("Heterogeneity", function() {
    h  <- val$heterogeneity
    i2 <- vapply(h, function(z) z$i2, numeric(1))
    o  <- order(i2)
    op <- par(mar = c(4.5, 11, 3, 2)); on.exit(par(op))
    bp <- barplot(i2[o], horiz = TRUE, las = 1, xlim = c(0, 100),
                  xlab = "I2 (%)", main = "Cross-site heterogeneity",
                  col = ifelse(i2[o] > 75, "#c0392b",
                        ifelse(i2[o] > 50, "#e67e22", "#7f8c8d")))
    abline(v = c(50, 75), lty = 2, col = "grey60")
    text(i2[o] + 2, bp, sprintf("%.0f", i2[o]), adj = 0, cex = 0.8)
  }, "plot",
  "I2 above 75% means the sites disagree enough that a single pooled mean is misleading.")
}

cat("\nDone. Outputs registered:", length(get_outputs()), "\n")
