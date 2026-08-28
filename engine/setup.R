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

# Helpers: detect packages that are missing, broken, or built under a
# different major.minor R version than the current interpreter. Warnings like
# "package X was built under R 4.5.2" are often harmless, but when an older or
# mismatched install is actually broken we want to proactively replace it
# during setup rather than fail later while launching the app.
.current_r_minor <- function() {
  parts <- strsplit(R.version$minor, "\\.", fixed = FALSE)[[1]]
  paste0(R.version$major, ".", parts[1])
}

.package_built_minor <- function(pkg) {
  built <- tryCatch(utils::packageDescription(pkg, fields = "Built"),
                    error = function(e) NA_character_)
  if (!is.character(built) || length(built) != 1L || !nzchar(built)) return(NA_character_)
  m <- regmatches(built, regexpr("R [0-9]+\\.[0-9]+", built))
  if (!length(m) || !nzchar(m)) return(NA_character_)
  sub("^R ", "", m)
}

.package_needs_reinstall <- function(pkg) {
  if (!pkg %in% rownames(installed.packages())) return(TRUE)
  ok_namespace <- tryCatch(requireNamespace(pkg, quietly = TRUE),
                           error = function(e) FALSE)
  if (!ok_namespace) return(TRUE)
  built_minor <- .package_built_minor(pkg)
  if (!is.na(built_minor) && !identical(built_minor, .current_r_minor())) return(TRUE)
  FALSE
}

.remove_installed_package <- function(pkg) {
  lib <- tryCatch(find.package(pkg), error = function(e) NULL)
  if (is.null(lib)) return(invisible(FALSE))
  lib_root <- dirname(lib)
  tryCatch(remove.packages(pkg, lib = lib_root),
           error = function(e) invisible(FALSE))
  invisible(TRUE)
}

# ── Step A: CRAN packages ─────────────────────────────────────────────
need <- cran_pkgs[vapply(cran_pkgs, .package_needs_reinstall, logical(1))]
if (length(need) == 0) {
  cat("  ✓  CRAN packages already installed.\n")
} else {
  stale <- need[need %in% rownames(installed.packages())]
  if (length(stale) > 0) {
    cat(sprintf("  Reinstalling incompatible or broken packages: %s\n",
                paste(stale, collapse = ", ")))
    invisible(lapply(stale, .remove_installed_package))
  }
  cat(sprintf("  Installing: %s\n", paste(need, collapse = ", ")))
  cat("  (This may take a few minutes on first run...)\n")
  # quiet = FALSE so compilation errors are visible
  install.packages(need, repos = "https://cloud.r-project.org", quiet = FALSE)

  # Verify — install.packages() does not throw on failure by default
  still_missing <- need[vapply(need, .package_needs_reinstall, logical(1))]
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
# "Installed" is not the same as "up to date". fedstats ships with the engine,
# so the two must always match: the moment the engine calls a function argument
# an older fedstats doesn't have, the site server dies with an obscure error
# (e.g. "unused argument (min_cell = MIN_CELL)") and never registers, so the
# coordinator just sees a site that never connects.
#
# A version check cannot catch this — DESCRIPTION's Version rarely changes
# between commits. Compare timestamps instead: if any source file is newer than
# the installed copy, reinstall. Git checkouts and unzipped kits both refresh
# the source mtimes, so this fires exactly when it should.
pkg_path <- normalizePath(file.path(getwd(), "fedstats"), mustWork = FALSE)

.newest_source <- function(path) {
  if (!dir.exists(path)) return(NA)
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(NA)
  max(file.mtime(files))
}
.installed_build_time <- function(lib) {
  meta <- file.path(lib, "Meta", "package.rds")
  if (!file.exists(meta)) return(NA)
  file.mtime(meta)
}
.installed_exports <- function(lib) {
  ns <- file.path(lib, "NAMESPACE")
  if (!file.exists(ns)) return(character(0))
  lines <- tryCatch(readLines(ns, warn = FALSE), error = function(e) character(0))
  hits <- sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\([^)]+\\)$", lines, value = TRUE))
  unique(trimws(hits))
}
.fedstats_required_exports <- function(role) {
  common <- c("fed_keypair", "fed_fingerprint", "fed_check_file_perms",
              "fed_register_message", "fed_sign", "fed_invite_parse")
  if (role == "site") {
    unique(c(common, "fed_bind_host", "fed_harden_file", "fed_ct_equal"))
  } else {
    unique(c(common, "fed_advertised_host", "fed_friendly_http_error",
             "fed_sid", "fed_token", "fed_invite_create", "fed_validate"))
  }
}
.fedstats_exports_ok <- function(lib, role) {
  req <- .fedstats_required_exports(role)
  got <- .installed_exports(lib)
  length(req) > 0 && all(req %in% got)
}

# find.package() locates the package WITHOUT loading it. requireNamespace()
# would load it, and on Windows install.packages() then cannot overwrite a DLL
# that is in use ("cannot remove prior installation") — which would make the
# reinstall below fail on exactly the machines that need it most.
fedstats_lib       <- tryCatch(find.package("fedstats"), error = function(e) NULL)
fedstats_installed <- !is.null(fedstats_lib)
src_time  <- .newest_source(pkg_path)
inst_time <- if (fedstats_installed) .installed_build_time(fedstats_lib) else NA
fedstats_stale <- fedstats_installed && !is.na(src_time) && !is.na(inst_time) &&
                  src_time > inst_time
fedstats_built_minor <- if (fedstats_installed) .package_built_minor("fedstats") else NA_character_
fedstats_built_mismatch <- fedstats_installed && !is.na(fedstats_built_minor) &&
                           !identical(fedstats_built_minor, .current_r_minor())
fedstats_exports_ok <- fedstats_installed && .fedstats_exports_ok(fedstats_lib, role)
fedstats_broken <- fedstats_installed &&
                   !tryCatch(requireNamespace("fedstats", quietly = TRUE),
                             error = function(e) FALSE)

if (fedstats_installed && !fedstats_stale && !fedstats_built_mismatch &&
    fedstats_exports_ok && !fedstats_broken) {
  cat("  ✓  fedstats already installed and up to date.\n")
} else {
  if (fedstats_stale)
    cat("  fedstats is out of date (the engine has changed since it was installed).\n")
  if (fedstats_built_mismatch)
    cat(sprintf("  fedstats was built for R %s, but this machine is running R %s.\n",
                fedstats_built_minor, .current_r_minor()))
  if (fedstats_installed && !fedstats_exports_ok)
    cat("  fedstats is missing exported functions required by this version of the app.\n")
  if (fedstats_broken)
    cat("  fedstats is installed but cannot be loaded correctly.\n")
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

  if (fedstats_installed) {
    cat("  Removing old fedstats installation first.\n")
    .remove_installed_package("fedstats")
  }
  install.packages(pkg_path, repos = NULL, type = "source")

  fedstats_ok <- tryCatch(requireNamespace("fedstats", quietly = TRUE),
                          error = function(e) FALSE)
  fedstats_lib_new <- tryCatch(find.package("fedstats"), error = function(e) NULL)
  exports_ok_new <- !is.null(fedstats_lib_new) && .fedstats_exports_ok(fedstats_lib_new, role)
  built_ok_new <- is.null(fedstats_lib_new) || {
    built_minor <- .package_built_minor("fedstats")
    is.na(built_minor) || identical(built_minor, .current_r_minor())
  }
  if (!fedstats_ok || !exports_ok_new || !built_ok_new) {
    cat("  ✗  fedstats installation failed.\n")
    cat("  Try running this manually in R:\n")
    cat(sprintf('    install.packages("%s", repos = NULL, type = "source")\n', pkg_path))
    quit(status = 1)
  }
  cat("  ✓  fedstats installed.\n")
}

cat("  Setup complete.\n")
