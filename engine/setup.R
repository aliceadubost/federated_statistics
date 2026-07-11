# engine/setup.R
# ─────────────────────────────────────────────────────────────────────
# Called by all launchers to ensure required R packages are installed.
# Usage (from project root):
#   Rscript engine/setup.R site
#   Rscript engine/setup.R coordinator
# Exits with code 1 if any package cannot be installed, so the launcher
# can detect failure and show a clear message.
# ─────────────────────────────────────────────────────────────────────

role <- commandArgs(trailingOnly = TRUE)
role <- if (length(role) > 0) role[1] else "coordinator"

# ── Ensure a writable R library exists ───────────────────────────────
# On Linux with system-installed R (e.g. via apt), the default library
# /usr/local/lib/R/site-library is owned by root and not writable by
# normal users. R won't fall back automatically in non-interactive mode,
# so install.packages() just fails. We detect this and set up a personal
# library in ~/R/library before attempting any installation.
(function() {
  writable <- Filter(function(p) file.access(p, mode = 2) == 0, .libPaths())
  if (length(writable) > 0) return(invisible(NULL))

  # No writable path found — use R_LIBS_USER or fall back to ~/R/library
  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (!nzchar(user_lib)) user_lib <- file.path(path.expand("~"), "R", "library")

  if (!dir.exists(user_lib))
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

  .libPaths(c(user_lib, .libPaths()))
  cat(sprintf("  Using personal R library: %s\n", user_lib))
})()

# ── CRAN packages needed by each role ────────────────────────────────
# site:        shiny + processx (GUI), plumber + jsonlite (API server),
#              sodium (invite verification + site keypair)
# coordinator: shiny (GUI), httr + jsonlite (remote site connections),
#              processx (registrar subprocess), sodium (invite signing),
#              future + promises (parallel, non-blocking site pings),
#              zip (builds the self-hosted site kit for /kit),
#              writexl (Excel export of results — pure, no Java/deps)
# fedstats itself declares httr + jsonlite + sodium as Imports — they
# will also be pulled in when fedstats is installed below.
cran_pkgs <- if (role == "site") {
  c("shiny", "processx", "plumber", "jsonlite", "sodium", "httr")
} else {
  c("shiny", "httr", "jsonlite", "processx", "sodium", "future", "promises",
    "zip", "writexl")
}

# ── Step A: CRAN packages ─────────────────────────────────────────────
need <- cran_pkgs[!cran_pkgs %in% rownames(installed.packages())]
if (length(need) == 0) {
  cat("  ✓  CRAN packages already installed.\n")
} else {
  cat(sprintf("  Installing: %s\n", paste(need, collapse = ", ")))
  cat("  (This may take a few minutes on first run...)\n")
  # quiet = FALSE so compilation errors are visible
  install.packages(need, repos = "https://cloud.r-project.org", quiet = FALSE)

  # Verify — install.packages() does not throw on failure by default
  still_missing <- need[!need %in% rownames(installed.packages())]
  if (length(still_missing) > 0) {
    cat("\n  ✗  Could not install:", paste(still_missing, collapse = ", "), "\n\n")
    cat("  On Linux, missing system libraries are the most common cause.\n")
    cat("  Try running this in your terminal first:\n")
    cat("    sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev\n")
    cat("  Then run this launcher again.\n\n")
    quit(status = 1)
  }
  cat("  ✓  CRAN packages installed.\n")
}

# ── Step B: fedstats (local package) ─────────────────────────────────
if (requireNamespace("fedstats", quietly = TRUE)) {
  cat("  ✓  fedstats already installed.\n")
} else {
  pkg_path <- normalizePath(file.path(getwd(), "fedstats"), mustWork = FALSE)
  cat(sprintf("  Installing fedstats from: %s\n", pkg_path))

  if (!dir.exists(pkg_path)) {
    cat("  ✗  fedstats/ folder not found.\n")
    cat("  Make sure you are running this from the project root directory.\n")
    quit(status = 1)
  }

  # When repos = NULL, R ignores dependencies = TRUE and won't fetch CRAN
  # packages automatically. Pre-install fedstats' declared Imports first.
  fedstats_imports <- c("httr", "jsonlite", "sodium")
  need_imports <- fedstats_imports[!fedstats_imports %in% rownames(installed.packages())]
  if (length(need_imports) > 0) {
    cat(sprintf("  Installing fedstats dependencies: %s\n",
                paste(need_imports, collapse = ", ")))
    install.packages(need_imports, repos = "https://cloud.r-project.org")
    still_missing <- need_imports[!need_imports %in% rownames(installed.packages())]
    if (length(still_missing) > 0) {
      cat("  ✗  Could not install:", paste(still_missing, collapse = ", "), "\n")
      quit(status = 1)
    }
  }

  install.packages(pkg_path, repos = NULL, type = "source")

  if (!requireNamespace("fedstats", quietly = TRUE)) {
    cat("  ✗  fedstats installation failed.\n")
    cat("  Try running this manually in R:\n")
    cat(sprintf('    install.packages("%s", repos = NULL, type = "source")\n', pkg_path))
    quit(status = 1)
  }
  cat("  ✓  fedstats installed.\n")
}

cat("  Setup complete.\n")
