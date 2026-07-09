# templates/demo_descriptives.R
# ─────────────────────────────────────────────────────────────────────
# Federated descriptive statistics — baseline characteristics
#
# Computes pooled means, SDs, and proportions without sharing any
# individual patient rows. Each site contributes only its count, sum,
# and sum-of-squares (sufficient statistics).
#
# Run standalone:  Rscript templates/demo_descriptives.R
# Run via GUI:     load this file in the coordinator app
# ─────────────────────────────────────────────────────────────────────

library(fedstats)

ANALYSIS_TITLE <- "Federated Descriptives — Baseline Characteristics"

# ── ADAPT: list every variable used in the analysis below ────────────
# type = "numeric"     → continuous variable; set realistic min/max
# type = "binary"      → coded 0/1 in the CSV
# type = "categorical" → text values; list all expected levels
VARS_SPEC <- list(
  age              = list(type = "numeric",     min = 18, max = 100),
  bmi              = list(type = "numeric",     min = 10, max = 80),
  sexM             = list(type = "binary"),
  nrs_arm_preop    = list(type = "numeric",     min = 0,  max = 10),
  diag_grp         = list(type = "categorical",
                           levels = c("radiculopathy", "myelopathy"))
)

# ── Server setup ─────────────────────────────────────────────────────
# GUI injects `servers` automatically. Standalone: reads CSV files from
# the data/ folder, or uses URLs from the FED_SITE_URLS environment variable.
if (!exists("servers", inherits = FALSE)) {
  .urls <- trimws(strsplit(Sys.getenv("FED_SITE_URLS", ""), ",")[[1]])
  .urls <- .urls[nzchar(.urls)]
  if (length(.urls) > 0) {
    .tok <- Sys.getenv("FED_API_TOKEN", "")
    servers <- lapply(.urls, function(u)
      create_remote_server(u, token = if (nzchar(.tok)) .tok else NULL))
  } else {
    .csvs <- sort(list.files("data", pattern = "[.]csv$", full.names = TRUE))
    if (!length(.csvs)) stop("No sites found. Add CSV files to ./data/ or set FED_SITE_URLS.")
    servers <- lapply(.csvs, create_server)
    cat(sprintf("Loaded %d local CSV site(s).\n\n", length(servers)))
  }
}

clear_outputs()

# ── Step 1: Validate data across all sites ───────────────────────────
# Checks that every required variable exists, is the right type, and
# has no extreme outliers. Warnings are informational (not errors).
val <- fed_validate(servers, VARS_SPEC)
if (!val$ok) warning("Validation issues — review the Validation tab.")

register_output("Validation", {
  lines <- sprintf("Sites: %d  |  Status: %s", length(val$site_reports),
                   if (val$ok) "PASS" else "FAIL")
  if (length(val$errors)   > 0) lines <- c(lines, "Errors:",   paste0("  ", val$errors))
  if (length(val$warnings) > 0) lines <- c(lines, "Warnings:", paste0("  ", val$warnings))
  paste(lines, collapse = "\n")
}, "text", "Pre-analysis data validation report")

# ── Step 2: Compute pooled summaries ─────────────────────────────────
# fed_numeric() returns: $n (count), $mean, $sd, $sum
# For binary variables: $sum = number of 1s, $mean = proportion

# Continuous variables
s_age  <- fed_numeric(servers, "age")
s_bmi  <- fed_numeric(servers, "bmi")
s_nrs  <- fed_numeric(servers, "nrs_arm_preop")

# Binary variables (sum = count of 1s, mean = proportion)
s_sex    <- fed_numeric(servers, "sexM")

# Print a quick summary to the Console tab
cat(sprintf("%-22s  n = %5d  mean (SD) = %.1f (%.1f)\n",
            "Age (years):",   s_age$n, s_age$mean, s_age$sd))
cat(sprintf("%-22s  n = %5d  mean (SD) = %.1f (%.1f)\n",
            "BMI (kg/m²):",   s_bmi$n, s_bmi$mean, s_bmi$sd))
cat(sprintf("%-22s  n = %5d  mean (SD) = %.1f (%.1f)\n",
            "NRS arm preop:", s_nrs$n, s_nrs$mean, s_nrs$sd))
cat(sprintf("%-22s  n = %5d  %.0f / %d (%.1f%%)\n",
            "Male sex:",      s_sex$n, s_sex$sum, s_sex$n, 100 * s_sex$mean))

# ── Step 3: Breakdown by subgroup ────────────────────────────────────
# fed_group_numeric() returns a named list keyed by group level.
# Each entry has $n, $mean, $sd for that group.
# ── ADAPT: change the variable name and groupvar to match your data ──
grp <- fed_group_numeric(servers, "nrs_arm_preop", "diag_grp")

cat("\nNRS arm preop by diagnosis:\n")
for (g in names(grp))
  cat(sprintf("  %-18s  n = %5d  mean = %.1f  SD = %.1f\n",
              g, grp[[g]]$n, grp[[g]]$mean, grp[[g]]$sd))

# ── Step 4: Build Table 1 and register for the GUI ───────────────────
# ── ADAPT: add or remove rows to match your variables ────────────────
tbl1 <- data.frame(
  Variable = c(
    "Age (years)", "BMI (kg/m²)", "Male sex (%)", "NRS arm — preop"
  ),
  N = c(s_age$n, s_bmi$n, s_sex$n, s_nrs$n),
  "Mean (SD) or n (%)" = c(
    sprintf("%.1f (%.1f)", s_age$mean,    s_age$sd),
    sprintf("%.1f (%.1f)", s_bmi$mean,    s_bmi$sd),
    sprintf("%.0f (%.1f%%)", s_sex$sum,   100 * s_sex$mean),
    sprintf("%.1f (%.1f)", s_nrs$mean,    s_nrs$sd)
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

register_output("Descriptives", tbl1, "table",
                "Table 1: Baseline characteristics — pooled across all federated sites")

cat("\nDone. Outputs registered:", length(get_outputs()), "\n")
