# coordinator_app.R
# =====================================================================
# Federated Statistics — Coordinator GUI (Shiny)
#
# Generalised: loads any .R analysis script that follows the contract:
#   ANALYSIS_TITLE  — shown in the window title
#   VARS_SPEC       — used for pre-analysis validation
#   register_output(name, value, type, caption) — drives dynamic tabs
#
# Sites onboard via signed invite bundles — no pasted URLs or manually
# typed tokens. This app runs a background registrar subprocess
# (registrar.R) that receives site registration callbacks, and polls the
# registry file to display registered sites. Per-site tokens are read
# from the registry.
#
# Start with the launcher (double-click):
#   Run/Mac/Start Coordinator.command  (macOS)
#   Run/Windows/Start Coordinator.bat  (Windows)
#   Run/Linux/Start Coordinator.sh     (Linux)
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(fedstats)
  library(processx)
  library(future)
  library(promises)
})

# Registry state-machine + persistence (sibling file; wd is this dir).
source(file.path(getwd(), "registry.R"))

# Ping runs each site's /health check in a background worker process so it
# doesn't block this session's own reactivity (other buttons stay usable
# while a ping is in flight) and so N sites take max(latency) instead of
# sum(latency). Workers are torn down in the onStop() below.
future::plan(future::multisession)

# ---------------------------------------------------------------
# Paths and configuration
# ---------------------------------------------------------------
APP_DIR          <- getwd()
REG_FILE         <- normalizePath(file.path(APP_DIR, "registered_sites.json"),
                                  mustWork = FALSE)
KEY_FILE         <- file.path(APP_DIR, "coordinator_key.json")
REGISTRAR_SCRIPT <- file.path(APP_DIR, "registrar.R")
REGISTRAR_PORT   <- as.integer(Sys.getenv("FED_REGISTRAR_PORT", "8731"))
INVITE_TTL_DAYS  <- as.integer(Sys.getenv("FED_INVITE_TTL_DAYS", "7"))

# ---- Coordinator signing key: load existing or create once ----------
.load_or_create_key <- function() {
  if (file.exists(KEY_FILE)) {
    k <- tryCatch(jsonlite::fromJSON(KEY_FILE, simplifyVector = TRUE),
                  error = function(e) NULL)
    if (!is.null(k) && !is.null(k$private) && !is.null(k$public)) return(k)
  }
  kp <- fed_keypair()
  writeLines(jsonlite::toJSON(kp, auto_unbox = TRUE), KEY_FILE)
  reg_harden_file(KEY_FILE)
  kp
}
COORD_KEY  <- .load_or_create_key()
COORD_FP   <- fed_fingerprint(COORD_KEY$public)
COORD_HOST <- fed_advertised_host()                  # MagicDNS preferred (Q3)
COORD_ADDR <- if (nzchar(COORD_HOST))
                sprintf("%s:%d", COORD_HOST, REGISTRAR_PORT) else ""

# ---- Registrar subprocess (single, app-level) -----------------------
.start_registrar <- function() {
  envv <- Sys.getenv()
  envv["FED_REGISTRY_FILE"]  <- REG_FILE
  envv["FED_REGISTRAR_PORT"] <- as.character(REGISTRAR_PORT)
  envv["FED_INVITE_TTL_DAYS"] <- as.character(INVITE_TTL_DAYS)
  processx::process$new("Rscript", REGISTRAR_SCRIPT, env = envv, wd = APP_DIR,
                        stdout = "|", stderr = "|")
}
registrar_proc <- tryCatch(.start_registrar(), error = function(e) NULL)
onStop(function() {
  try(if (!is.null(registrar_proc) && registrar_proc$is_alive())
        registrar_proc$kill(), silent = TRUE)
  try(future::plan(future::sequential), silent = TRUE)  # tear down ping workers
})

# Warn (in the launcher console) if the registry is world-readable.
{
  w <- reg_check_perms(REG_FILE)
  if (nzchar(w)) cat("[coordinator]", w, "\n")
}

# ---------------------------------------------------------------
# Script metadata extraction
# ---------------------------------------------------------------
.extract_meta <- function(script_path) {
  lines  <- readLines(script_path, warn = FALSE)
  cutoff <- grep("if\\s*\\(!exists\\s*\\(", lines)[1]
  if (is.na(cutoff)) cutoff <- length(lines) + 1L
  code   <- paste(lines[seq_len(cutoff - 1L)], collapse = "\n")
  env    <- new.env(parent = globalenv())
  tryCatch(eval(parse(text = code), envir = env), error = function(e) NULL)
  list(
    title = if (exists("ANALYSIS_TITLE", envir = env, inherits = FALSE))
              env$ANALYSIS_TITLE
            else tools::file_path_sans_ext(basename(script_path)),
    vars_spec = if (exists("VARS_SPEC", envir = env, inherits = FALSE))
                  env$VARS_SPEC
                else NULL
  )
}

# Bundled analyses (analysis/templates/*.R), listed by their human title so
# the coordinator can pick one from a dropdown instead of hunting for an .R
# file. Returns a named character vector value=path, name=title (empty if the
# folder isn't found — the Browse fallback still works).
TEMPLATES_DIR <- normalizePath(file.path(APP_DIR, "..", "..", "analysis", "templates"),
                               mustWork = FALSE)
.scan_templates <- function() {
  if (!dir.exists(TEMPLATES_DIR)) return(character(0))
  files <- sort(list.files(TEMPLATES_DIR, pattern = "\\.R$", full.names = TRUE))
  if (!length(files)) return(character(0))
  titles <- vapply(files, function(f)
    tryCatch(.extract_meta(f)$title,
             error = function(e) tools::file_path_sans_ext(basename(f))),
    character(1))
  setNames(files, titles)
}
ANALYSIS_TEMPLATES <- .scan_templates()

# ---------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------
fmt_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p) || !is.finite(p)) return("")
  if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
}

cap_print <- function(expr) {
  con <- textConnection("buf", "w", local = TRUE)
  sink(con); on.exit({ sink(); close(con) }, add = TRUE)
  force(expr)
  paste(buf, collapse = "\n")
}

# A "mailto:" link opens the user's own already-signed-in email client
# with the message pre-filled — no SMTP server, no credentials stored in
# the app, nothing new to maintain. They just add a recipient and hit
# send. (Real send-from-the-app was considered and rejected: it would
# need stored email credentials and its own deliverability/spam problems
# for marginal convenience over this.)
build_mailto_link <- function(subject, body) {
  sprintf("mailto:?subject=%s&body=%s",
         utils::URLencode(subject, reserved = TRUE),
         utils::URLencode(body, reserved = TRUE))
}

# One ready-to-send message for a brand-new site operator with nothing
# installed yet: Tailscale (the one unavoidable manual step — nothing can
# reach a machine that isn't on the tailnet yet), the self-hosted kit link,
# then the study invite. A repeat operator who already has the software
# just needs the short invite link (shown separately, above this).
build_onboarding_message <- function(study, kit_link, invite_link) {
  paste(
    sprintf('You\'ve been invited to join the "%s" federated study.', study),
    "",
    "STEP 1 - Install Tailscale (one-time, about 2 minutes)",
    "Download: https://tailscale.com/download",
    "After installing, wait for an invite from me to join our private network,",
    "then accept it and sign in.",
    "",
    "STEP 2 - Get the study software (one-time)",
    "Once Tailscale is connected, open this link and click the download button:",
    kit_link,
    "Unzip the downloaded file anywhere, then open the \"Start Site\" file for",
    "your computer (inside the Run folder).",
    "",
    "STEP 3 - Join the study",
    "Paste this into the app and click Join:",
    invite_link,
    "",
    "Then pick your data file and click Start Server. That's it.",
    sep = "\n"
  )
}

# Pure function (no Shiny dependency): case-/whitespace-insensitive check
# for whether `name` is already used by a site in the registry. Two sites
# named "Sweden" would be indistinguishable in the table and in any
# message referring to them by name, so this is enforced at invite-creation
# time, not just cosmetically discouraged.
site_name_exists <- function(reg, name) {
  if (is.null(reg) || !length(reg$sites)) return(FALSE)
  existing <- vapply(reg$sites, function(r) tolower(trimws(r$name)), character(1))
  tolower(trimws(name)) %in% existing
}

ping_one <- function(url, token, idx, name = "") {
  label <- if (nzchar(name)) name else paste("Site", idx)
  tryCatch({
    hdrs <- if (nzchar(token))
      httr::add_headers(Authorization = paste("Bearer", token)) else NULL
    r <- httr::GET(paste0(sub("/+$", "", url), "/health"), hdrs, httr::timeout(8))
    if (httr::status_code(r) == 200) {
      rows <- as.integer(unlist(httr::content(r, as = "parsed")$rows))
      sprintf("%s  [OK]   n = %d rows   %s", label, rows, url)
    } else {
      friendly <- fed_friendly_http_error(status_code = httr::status_code(r))
      sprintf("%s  [ERR]  %s   %s", label,
             if (!is.null(friendly)) friendly
             else sprintf("HTTP %d", httr::status_code(r)), url)
    }
  }, error = function(e) {
    friendly <- fed_friendly_http_error(e = e)
    sprintf("%s  [ERR]  %s", label, if (!is.null(friendly)) friendly else conditionMessage(e))
  })
}

# Ping every site in parallel (one future worker per site) instead of one
# at a time. Resolves to a character vector of ping_one() result lines, in
# the same order as `sites`.
ping_sites_async <- function(sites) {
  proms <- lapply(sites, function(s)
    promises::future_promise(ping_one(s$url, s$token, 0, s$name)))
  promises::promise_all(.list = proms) %...>% unlist
}

# Stale/offline threshold for the live status badge (item 3): a successful
# ping older than this reads as "Stale" rather than "Connected".
PING_STALE_S <- 120

# Pure function (no Shiny dependency) so it's directly unit-testable:
# given a ping_status entry, returns "connected" / "stale" / "offline", or
# NULL if the site has never been pinged.
ping_badge_state <- function(st, now = Sys.time()) {
  if (is.null(st)) return(NULL)
  if (!isTRUE(st$ok)) return("offline")
  age <- as.numeric(difftime(now, st$checked_at, units = "secs"))
  if (age < PING_STALE_S) "connected" else "stale"
}

# Pure function (no Shiny dependency): a countdown label for an invite
# that hasn't been consumed yet, or NULL when there's nothing useful to
# show (already registered — the expiry only ever governed the invite
# window, not the site itself; already expired — its own badge already
# says so; or no exp value at all).
invite_expiry_label <- function(exp, invite_state, now = as.integer(Sys.time())) {
  if (!isTRUE(invite_state %in% c("issued", "in_use"))) return(NULL)
  if (is.null(exp) || is.na(exp)) return(NULL)
  secs_left <- exp - now
  if (secs_left <= 0) return(NULL)
  days_left <- secs_left %/% 86400L
  if (days_left >= 1L) sprintf("expires in %dd", days_left)
  else sprintf("expires in %dh", max(1L, secs_left %/% 3600L))
}

# Pure function (no Shiny dependency): a plain-language readiness line
# shown only in the "setup" phase (see #workspace phase toggle in server())
# — replaces the earlier numbered-stepper widget, which implied a required
# Script-then-Sites order that was never actually enforced.
build_readiness_note <- function(step_script, n_sites) {
  missing <- c(
    if (!step_script) "choose an analysis",
    if (n_sites == 0) "invite at least one site"
  )
  if (length(missing) == 0) return("Ready — click Validate or Run when you are.")
  sprintf("To get started: %s.", paste(missing, collapse = " and "))
}

# ---------------------------------------------------------------
# Saving results: build a single self-contained HTML report bundling
# every output (tables, plots, text) so the coordinator can keep, print
# (to PDF), or share the results. No external files, no dependencies
# beyond what's already loaded (jsonlite for base64). Pure functions so
# they're testable without a Shiny session.
# ---------------------------------------------------------------
.html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.df_to_html <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  th <- paste0("<th>", vapply(names(df), .html_escape, character(1)), "</th>", collapse = "")
  cells <- lapply(df, function(col) vapply(col, function(v) .html_escape(as.character(v)), character(1)))
  body <- if (nrow(df) == 0) "" else paste(vapply(seq_len(nrow(df)), function(i) {
    tds <- paste0("<td>", vapply(cells, `[`, character(1), i), "</td>", collapse = "")
    paste0("<tr>", tds, "</tr>")
  }, character(1)), collapse = "\n")
  paste0("<table class='rt'><thead><tr>", th, "</tr></thead><tbody>", body, "</tbody></table>")
}

# Render a plot output (a zero-arg function or a ggplot) to a base64 PNG
# data URI so it embeds directly in the standalone HTML file.
.plot_to_data_uri <- function(fn, width = 900, height = 520, res = 110) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  tryCatch({
    if (is.function(fn)) fn()
    else if (inherits(fn, "ggplot")) print(fn)
    else { graphics::plot.new(); graphics::text(0.5, 0.5, "Cannot render plot.") }
  }, error = function(e) {
    graphics::plot.new(); graphics::text(0.5, 0.5, paste("Plot error:", conditionMessage(e)))
  })
  grDevices::dev.off(); on.exit()
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  paste0("data:image/png;base64,", gsub("[[:space:]]", "", jsonlite::base64_enc(raw)))
}

# Assemble the full report. `outputs` is get_outputs()-shaped; `warnings`
# are the run's captured warnings (privacy suppression etc.).
build_results_html <- function(title, outputs = NULL, val_txt = NULL,
                               console_log = NULL, warnings = character(0),
                               n_sites = NA_integer_, now = Sys.time()) {
  esc  <- .html_escape
  ttl  <- if (is.null(title) || !nzchar(title)) "Federated Analysis" else title
  when <- format(now, "%Y-%m-%d %H:%M")

  meta_bits <- c(
    if (!is.na(n_sites)) sprintf("%d site%s", n_sites, if (n_sites == 1) "" else "s"),
    paste("generated", when)
  )

  sections <- character(0)

  priv <- grep("Privacy suppression", warnings, value = TRUE)
  if (length(priv) > 0)
    sections <- c(sections, paste0(
      "<div class='note privacy'><b>Privacy protection applied.</b> ",
      "One or more small subgroups were withheld to protect individual patients. ",
      "Affected pooled figures exclude the withheld site(s).<ul>",
      paste0("<li>", esc(priv), "</li>", collapse = ""), "</ul></div>"))

  if (!is.null(outputs)) for (out in outputs) {
    inner <- switch(out$type,
      "table" = .df_to_html(out$value),
      "plot"  = sprintf("<img alt='%s' src='%s'>", esc(out$name), .plot_to_data_uri(out$value)),
      "text"  = paste0("<pre>", esc(if (is.character(out$value)) out$value
                                    else paste(utils::capture.output(print(out$value)), collapse = "\n")),
                       "</pre>"),
      paste0("<pre>", esc("(unsupported output type)"), "</pre>"))
    cap <- if (!is.null(out$caption)) paste0("<div class='cap'>", esc(out$caption), "</div>") else ""
    sections <- c(sections, sprintf("<section><h2>%s</h2>%s%s</section>",
                                    esc(out$name), inner, cap))
  }

  if (!is.null(val_txt) && nzchar(val_txt))
    sections <- c(sections, sprintf("<section><h2>Validation report</h2><pre>%s</pre></section>",
                                    esc(val_txt)))

  if (!is.null(console_log) && nzchar(console_log))
    sections <- c(sections, sprintf("<section><h2>Console log</h2><pre>%s</pre></section>",
                                    esc(console_log)))

  if (length(sections) == 0)
    sections <- "<section><p class='note'>No results were produced.</p></section>"

  paste0(
"<!doctype html><html lang='en'><head><meta charset='utf-8'>",
"<meta name='viewport' content='width=device-width, initial-scale=1'>",
"<title>", esc(ttl), " — Results</title><style>",
":root{--brand:#2454E8;--ink:#0F1B2D;--muted:#5B6B82;--line:#DCE3EE;--tint:#F3F6FB;}",
"*{box-sizing:border-box;}",
"body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;",
"color:var(--ink);max-width:900px;margin:0 auto;padding:40px 28px 64px;line-height:1.6;background:#fff;}",
"header{border-bottom:2px solid var(--line);padding-bottom:16px;margin-bottom:8px;}",
"h1{font-size:1.7rem;font-weight:800;letter-spacing:-.02em;margin:0 0 4px;}",
".sub{color:var(--muted);font-size:.9rem;}",
"section{margin-top:34px;}",
"h2{font-size:1.15rem;font-weight:700;color:var(--ink);border-bottom:1px solid var(--line);",
"padding-bottom:8px;margin-bottom:14px;}",
"table.rt{border-collapse:collapse;width:100%;font-size:.92rem;}",
"table.rt th{text-align:left;color:var(--muted);font-size:.8rem;text-transform:uppercase;",
"letter-spacing:.04em;border-bottom:2px solid var(--line);padding:8px 10px;}",
"table.rt td{padding:8px 10px;border-bottom:1px solid var(--line);}",
"table.rt tbody tr:nth-child(even){background:var(--tint);}",
"img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:8px;}",
"pre{background:var(--tint);border:1px solid var(--line);border-radius:8px;padding:14px 16px;",
"white-space:pre-wrap;font-size:.86rem;overflow-x:auto;}",
".cap{color:var(--muted);font-size:.85rem;font-style:italic;margin-top:8px;}",
".note{color:var(--muted);font-size:.9rem;}",
".privacy{background:#FDF1E4;border:1px solid #F6DDBC;color:#9A5B0A;border-radius:10px;",
"padding:14px 16px;margin-top:24px;}",
".privacy ul{margin:8px 0 0;padding-left:20px;}",
"footer{margin-top:48px;padding-top:16px;border-top:1px solid var(--line);color:var(--muted);",
"font-size:.8rem;}",
"@media print{body{max-width:none;}section{break-inside:avoid;}}",
"</style></head><body>",
"<header><h1>", esc(ttl), "</h1>",
"<div class='sub'>Federated Statistics — ", esc(paste(meta_bits, collapse = " · ")), "</div></header>",
paste(sections, collapse = "\n"),
"<footer>Only aggregate statistics leave each site; no individual patient data is transferred. ",
"Cells below the site privacy threshold are withheld.</footer>",
"</body></html>")
}

# ---------------------------------------------------------------
# Saving results as Excel: each table output becomes its own worksheet,
# plus a "Study info" sheet (name / sites / date / privacy note) and a
# "Notes" sheet for text outputs. Plots can't live in a spreadsheet — they
# stay in the HTML report. Returns a named list of data.frames for
# writexl::write_xlsx(). Pure / testable.
# ---------------------------------------------------------------
# A small key/value data.frame describing the run — shared by the Excel
# "Study info" sheet and the CSV bundle's study_info.csv.
.results_info_df <- function(title, n_sites, warnings, now) {
  priv <- grep("Privacy suppression", warnings, value = TRUE)
  data.frame(
    Field = c("Study", "Sites", "Generated", if (length(priv)) "Privacy note"),
    Value = c(if (is.null(title) || !nzchar(title)) "Federated Analysis" else title,
              if (is.na(n_sites)) "" else as.character(n_sites),
              format(now, "%Y-%m-%d %H:%M"),
              if (length(priv)) paste(priv, collapse = " | ")),
    stringsAsFactors = FALSE)
}

# Excel sheet names: <=31 chars, none of []:*?/\, and unique in the book.
.xlsx_sheet_name <- function(name, used) {
  s <- gsub("[][:*?/\\\\]", " ", name, perl = TRUE)
  s <- trimws(gsub("[[:cntrl:]]", " ", s))
  if (!nzchar(s)) s <- "Sheet"
  s <- substr(s, 1L, 31L)
  base <- s; k <- 1L
  while (s %in% used) {
    suf <- sprintf(" (%d)", k)
    s <- paste0(substr(base, 1L, 31L - nchar(suf)), suf); k <- k + 1L
  }
  s
}

# Filesystem-safe name for one CSV in the bundle: strip \/:*?"<>|, cap
# length, keep unique.
.safe_filename <- function(name, ext, used) {
  s <- gsub("[\\\\/:*?\"<>|]", " ", name)
  s <- gsub("\\s+", "_", trimws(gsub("[[:cntrl:]]", " ", s)))
  if (!nzchar(s)) s <- "table"
  s <- substr(s, 1L, 60L)
  fn <- paste0(s, ext); base <- s; k <- 1L
  while (fn %in% used) { fn <- paste0(base, "_", k, ext); k <- k + 1L }
  fn
}

build_results_workbook <- function(outputs = NULL, title = NULL, val_txt = NULL,
                                    warnings = character(0), n_sites = NA_integer_,
                                    now = Sys.time()) {
  sheets <- list(); used <- character(0)
  add <- function(nm, df) {
    s <- .xlsx_sheet_name(nm, used); used <<- c(used, s); sheets[[s]] <<- df
  }

  add("Study info", .results_info_df(title, n_sites, warnings, now))

  notes <- list()
  if (!is.null(outputs)) for (out in outputs) {
    if (identical(out$type, "table")) {
      add(out$name, as.data.frame(out$value, stringsAsFactors = FALSE))
    } else if (identical(out$type, "text")) {
      txt <- if (is.character(out$value)) paste(out$value, collapse = "\n")
             else paste(utils::capture.output(print(out$value)), collapse = "\n")
      notes[[length(notes) + 1L]] <-
        data.frame(Output = out$name, Content = txt, stringsAsFactors = FALSE)
    }
  }
  if (length(notes)) add("Notes", do.call(rbind, notes))

  if (!is.null(val_txt) && nzchar(val_txt))
    add("Validation",
        data.frame(Validation = strsplit(val_txt, "\n", fixed = TRUE)[[1]],
                   stringsAsFactors = FALSE))

  sheets
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* ── Palette: professional / trustworthy / tech ──────────────── */
    :root {
      --brand:      #2454E8;
      --brand-dark: #1C41C2;
      --brand-deep: #17318F;
      --ink:        #0F1B2D;
      --ink-muted:  #5B6B82;
      --line:       #DCE3EE;
      --tint:       #F3F6FB;
      --ok-bg:#EAF6EF;   --ok-fg:#146C43;   --ok-dot:#1FAA59;   --ok-line:#CDEBD9;
      --warn-bg:#FDF1E4; --warn-fg:#9A5B0A; --warn-dot:#E08A1E; --warn-line:#F6DDBC;
      --bad-bg:#FDECEC;  --bad-fg:#991B1B;  --bad-dot:#DC2626;  --bad-line:#F6D0D0;
      --muted-bg:#F0F2F6;--muted-fg:#6B7686;--muted-dot:#A5AEBC;--muted-line:#E2E6ED;
    }
    html  { font-size:16px; }
    body  { font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
            background:#FCFDFF; color:var(--ink); font-size:1rem; line-height:1.6; }
    .well { background-color:#FCFDFF; border:1px solid var(--line); border-radius:14px;
            box-shadow:0 1px 3px rgba(15,27,45,.04); padding:20px; }
    h2    { color:var(--ink); font-weight:800; font-size:2.1rem; letter-spacing:-.02em;
            padding-bottom:18px; margin-bottom:22px; border-bottom:1px solid var(--line); }
    .step { font-weight:800; font-size:0.88rem; margin:18px 0 6px 0;
            color:var(--brand-deep); text-transform:uppercase; letter-spacing:.07em; }
    .meta-title { font-size: 1.05rem; font-weight: 700; color: var(--ink);
                  margin: 6px 0 2px 0; }
    .meta-vars  { font-size: 0.85rem; color: var(--ink-muted); margin-bottom: 6px;
                  word-break: break-word; }
    pre  { font-size: 0.88rem; background: #F8FAFC; border: 1px solid var(--line);
           border-radius: 12px; padding: 16px; white-space: pre-wrap;
           max-height: 480px; overflow-y: auto; }
    .tbl-caption { font-size: 0.9rem; color: var(--ink-muted); font-style: italic;
                   margin: 6px 0 16px 0; }
    .welcome { color: var(--ink-muted); padding: 40px 20px; text-align: center;
               font-size: .95rem; }
    .welcome h3, .welcome h4 { color: var(--ink); border:none; }
    .note { font-size: 0.85rem; color: var(--ink-muted); margin-top: 12px; line-height:1.5; }
    .btn, button.btn { border-radius:9px; font-weight:600; transition:all .15s ease; }
    .btn-primary,
    .btn-primary:active,
    .btn-primary.active { background-color: var(--brand) !important;
                          border-color: var(--brand-dark) !important;
                          box-shadow:0 2px 8px rgba(36,84,232,.25); }
    .btn-primary:hover,
    .btn-primary:focus  { background-color: var(--brand-dark) !important;
                          border-color: var(--brand-deep) !important;
                          box-shadow:0 4px 14px rgba(36,84,232,.35);
                          transform:translateY(-1px); }
    .btn-success { background-color:#16A34A !important; border-color:#15803D !important; }
    .btn-success:hover { background-color:#15803D !important; transform:translateY(-1px); }
    label { font-weight:600; font-size:.92rem; color:var(--ink-muted); }
    /* ── Sites table ───────────────────────────────────────────── */
    .sites-tbl { width:100%; font-size:0.92rem; border-collapse:collapse; table-layout:fixed; }
    .sites-tbl th { text-align:left; color:var(--ink-muted); font-size:0.82rem;
                    text-transform:uppercase; letter-spacing:.04em;
                    border-bottom:2px solid var(--line); padding:8px 6px; }
    .sites-tbl td { padding:9px 6px; border-bottom:1px solid var(--line);
                    vertical-align:middle; overflow-wrap:anywhere; }
    .site-name    { font-weight:700; white-space:nowrap; }
    .site-addr-sub { font-size:.72rem; color:var(--ink-muted); margin-top:2px;
                     font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                     overflow-wrap:anywhere; }
    .sites-tbl th:nth-child(1), .sites-tbl td:nth-child(1) { width:44%; }
    .sites-tbl th:nth-child(2), .sites-tbl td:nth-child(2) { width:19%; }
    .sites-tbl th:nth-child(3), .sites-tbl td:nth-child(3) { width:22%; }
    .sites-tbl th:nth-child(4), .sites-tbl td:nth-child(4) { width:15%; }
    .sbadge { display:inline-flex; align-items:center; gap:6px;
              padding:3px 12px 3px 9px; border-radius:20px; border:1px solid transparent;
              font-size:0.78rem; font-weight:700; }
    .sbadge::before { content:''; width:7px; height:7px; border-radius:50%; flex:0 0 auto; }
    .sb-ok    { background:var(--ok-bg);    color:var(--ok-fg);    border-color:var(--ok-line); }
    .sb-ok::before    { background:var(--ok-dot); }
    .sb-info  { background:var(--warn-bg);  color:var(--warn-fg);  border-color:var(--warn-line); }
    .sb-info::before  { background:var(--warn-dot); }
    .sb-warn  { background:var(--bad-bg);   color:var(--bad-fg);   border-color:var(--bad-line); }
    .sb-warn::before  { background:var(--bad-dot); }
    .sb-muted { background:var(--muted-bg); color:var(--muted-fg); border-color:var(--muted-line); }
    .sb-muted::before { background:var(--muted-dot); }
    .coord-addr { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                  font-size:0.9rem; color:var(--brand-deep);
                  background:var(--tint); border:1px solid #C7D6F8;
                  border-radius:10px; padding:8px 12px; word-break:break-all; }
    .fp { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
          font-size:0.85rem; color:var(--ink-muted); }
    .inv-box { width:100%; height:130px;
               font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
               font-size:0.82rem; word-break:break-all; border-radius:10px; border-color:var(--line);
               padding:12px; }
    /* ── Two-phase workspace: big centered setup, then compact + results ─
       Same DOM/inputs the whole time (nothing is ever destroyed/recreated
       on transition — only #workspace's class changes, server-side, once
       Validate/Run actually produce something). phase-results needs no
       rules: Bootstrap's own col-sm-5/col-sm-7 (from sidebarPanel(width=5)/
       mainPanel(width=7)) are already exactly that look. ─────────────── */
    #workspace.phase-setup .col-sm-5 { float:none; width:100%; max-width:760px; margin:0 auto; }
    #workspace.phase-setup .col-sm-7 { display:none; }
    /* ── Result tabs: modern underline style ──────────────────────── */
    .nav-tabs { border-bottom:2px solid var(--line); }
    .nav-tabs > li > a { border:none !important; background:transparent !important;
                         color:var(--ink-muted); font-weight:600; font-size:.92rem;
                         padding:11px 20px; border-radius:8px 8px 0 0; margin-right:2px; }
    .nav-tabs > li > a:hover { color:var(--brand-dark); background:var(--tint) !important; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus { color:var(--brand); background:transparent !important;
                                      border:none !important; box-shadow:inset 0 -3px 0 var(--brand); }
    /* ── Persistent privacy-suppression banner (results view) ─────── */
    .privacy-banner { background:var(--warn-bg); color:var(--warn-fg);
                      border:1px solid var(--warn-line); border-radius:10px;
                      padding:14px 16px; margin-bottom:12px; font-size:.9rem; line-height:1.5; }
    .privacy-banner ul { margin:8px 0 0; padding-left:20px; }
    .privacy-banner li { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                         font-size:.82rem; margin:2px 0; }
  "))),

  tags$script(HTML("
    function fedCopy(id){
      var el = document.getElementById(id);
      if(!el) return;
      el.select(); el.setSelectionRange(0, 99999);
      navigator.clipboard.writeText(el.value);
    }
    Shiny.addCustomMessageHandler('setPhase', function(phase){
      var el = document.getElementById('workspace');
      if (el) el.className = 'phase-' + phase;
    });
  ")),

  uiOutput("app_title_ui"),

  div(id = "workspace", class = "phase-setup", sidebarLayout(
    sidebarPanel(
      width = 5,

      # ---- Study (set once, independent of any script — reused by every
      # invite so all sites always share the same name automatically) ----
      div(class = "step", "Study"),
      textInput("study_name", NULL, value = "Federated study", width = "100%"),

      hr(),

      # ---- Analysis (no required order relative to Sites below — invite
      # people whenever, load/swap analyses whenever). A dropdown of the
      # bundled analyses plus a Browse for your own — both always available,
      # like the site's data-file picker, so neither is an "error path". ----
      div(class = "step", "Analysis"),
      if (length(ANALYSIS_TEMPLATES) > 0)
        selectInput("script_choice", NULL, width = "100%",
                    choices = c("Choose an analysis…" = "", ANALYSIS_TEMPLATES)),
      fileInput("script_file", "Or load your own script", accept = ".R",
                buttonLabel = "Browse…", placeholder = "No script selected"),
      uiOutput("script_meta_ui"),

      hr(),

      # ---- Sites ----
      div(class = "step", "Sites"),
      uiOutput("registrar_status_ui"),
      div(style = "margin:8px 0;",
          actionButton("btn_invite", "Invite a site",
                       class = "btn-primary btn-sm")),
      uiOutput("sites_table_ui"),

      hr(),
      uiOutput("readiness_note"),

      actionButton("btn_ping",     "Ping sites",
                   class = "btn-default btn-block"),
      br(),
      actionButton("btn_validate", "Validate data",
                   class = "btn-primary btn-block"),
      br(),
      actionButton("btn_run",      "Run analysis",
                   class = "btn-success btn-block"),

      div(class = "note", style = "margin-top:8px;",
          strong("Ping"), " checks the sites answer. ",
          strong("Validate"), " checks each site's data is ready. ",
          strong("Run"), " computes the results."),

      hr(),
      verbatimTextOutput("ping_out"),

      div(class = "note",
          "Only aggregate statistics leave each site.",
          br(), "No individual patient data is transferred.",
          br(),
          actionLink("show_help", "How this works", style = "font-size:.85rem;"))
    ),

    mainPanel(
      width = 7,
      uiOutput("results_panel")
    )
  ))
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    meta        = NULL,   # list(title, vars_spec)
    script_path = NULL,   # path Run sources (bundled template or uploaded file)
    outputs     = NULL,   # list of register_output() entries
    val_txt     = NULL,   # text from standalone Validate
    console_log = NULL,   # captured cat()/print() output from analysis script
    run_warnings = character(0),  # warnings captured during the last run (privacy etc.)
    reg         = reg_load(REG_FILE),  # registered-sites registry (polled)
    ping_status = list(),  # url -> list(ok, checked_at) from the last ping
    pending_revoke_sid = NULL, # sid awaiting confirm in the Revoke dialog
    phase       = "setup" # "setup" (big, centered) -> "results" (compact),
                          # one-way for the rest of the session
  )

  # ---- Phase transition: big centered setup -> compact + results, the
  # first time Validate/Run actually produce something. One-way — loading
  # a different script later clears rv$outputs/val_txt but must not dump
  # the operator back into the setup screen; they're already working.
  observe({
    if (identical(rv$phase, "setup") && (!is.null(rv$val_txt) || !is.null(rv$outputs))) {
      rv$phase <- "results"
      session$sendCustomMessage("setPhase", "results")
    }
  })

  # ---- Poll the registry file + drain registrar logs ------------
  poll <- reactiveTimer(1500)
  observe({
    poll()
    rv$reg <- reg_load(REG_FILE)
    # Drain the registrar's pipes so its output buffer never blocks.
    if (!is.null(registrar_proc) && registrar_proc$is_alive()) {
      invisible(tryCatch(registrar_proc$read_output_lines(), error = function(e) NULL))
      invisible(tryCatch(registrar_proc$read_error_lines(),  error = function(e) NULL))
    }
  })

  # ---- Active sites for analysis (registry consumed + manual) ---
  active_sites <- reactive({
    sites <- list()
    reg <- rv$reg
    if (!is.null(reg)) {
      for (sid in names(reg$sites)) {
        r <- reg$sites[[sid]]
        if (identical(r$invite_state, "consumed") && !is.null(r$site_addr))
          sites[[length(sites) + 1L]] <-
            list(name = r$name, url = r$site_addr, token = r$token)
      }
    }
    sites
  })

  # ---- Dynamic app title from script ----------------------------
  output$app_title_ui <- renderUI({
    title <- if (!is.null(rv$meta)) rv$meta$title
             else "Federated Analysis — Coordinator"
    titlePanel(title)
  })

  # ---- Load an analysis: extract metadata (shared by both pickers) ----
  # rv$script_path is the file the Run button actually sources — set to a
  # bundled template (dropdown) or an uploaded file (Browse), whichever the
  # operator touched last.
  load_analysis <- function(path) {
    rv$script_path <- path
    rv$meta <- tryCatch(
      .extract_meta(path),
      error = function(e) {
        showNotification(paste("Could not read analysis:", conditionMessage(e)),
                         type = "error"); NULL
      })
    rv$outputs      <- NULL
    rv$val_txt      <- NULL
    rv$console_log  <- NULL
    rv$run_warnings <- character(0)
  }

  observeEvent(input$script_file, {           # Browse: your own script
    req(input$script_file)
    load_analysis(input$script_file$datapath)
  })

  observeEvent(input$script_choice, {         # Dropdown: a bundled analysis
    if (nzchar(input$script_choice)) load_analysis(input$script_choice)
  }, ignoreInit = TRUE)

  # ---- "How this works" help modal ------------------------------
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "How this works",
      tags$ol(
        tags$li(strong("Name your study"), " and ", strong("choose an analysis"),
                " (a built-in one, or load your own script)."),
        tags$li(strong("Invite each hospital site."), " Send them the invite link; ",
                "they join and start their own server on their machine."),
        tags$li(strong("Ping"), " to check they answer, ", strong("Validate"),
                " to check their data is ready, then ", strong("Run"),
                " to compute the pooled results."),
        tags$li(strong("Save the results"), " as Excel, CSV, or an HTML report.")),
      p(class = "note",
        "Only aggregate statistics ever leave each hospital — no patient records are ",
        "transferred, and subgroups too small to be safe are withheld automatically."),
      easyClose = TRUE, footer = modalButton("Got it")))
  })

  output$script_meta_ui <- renderUI({
    m <- rv$meta
    if (is.null(m)) return(NULL)
    vs_names <- if (!is.null(m$vars_spec)) names(m$vars_spec) else character(0)
    tagList(
      div(class = "meta-title", m$title),
      if (length(vs_names) > 0)
        div(class = "meta-vars",
            strong("Variables: "), paste(vs_names, collapse = ", "))
    )
  })

  # ---- Registrar status + coordinator address -------------------
  output$registrar_status_ui <- renderUI({
    poll()
    alive <- !is.null(registrar_proc) && registrar_proc$is_alive()
    if (!alive)
      return(div(class = "sbadge sb-warn", style = "margin-bottom:4px;",
                 "Registrar not running"))
    tagList(
      if (nzchar(COORD_ADDR))
        div(class = "coord-addr", title = "Sites register here automatically",
            paste0("Registrar: ", COORD_ADDR))
      else
        div(class = "sbadge sb-warn",
            "Tailscale not detected — invites will not be reachable remotely."),
      div(class = "fp", title = "Read this to a site operator to verify your key",
          paste0("Key fingerprint: ", COORD_FP))
    )
  })

  # ---- Sites table ----------------------------------------------
  output$sites_table_ui <- renderUI({
    reg <- rv$reg
    badge <- function(state, pending) {
      if (!is.null(pending)) return(span(class = "sbadge sb-warn", "Needs approval"))
      switch(state,
        issued   = span(class = "sbadge sb-info",  "Invited"),
        in_use   = span(class = "sbadge sb-info",  "Registering"),
        consumed = span(class = "sbadge sb-ok",    "Registered"),
        expired  = span(class = "sbadge sb-muted", "Expired"),
        span(class = "sbadge sb-muted", state))
    }
    act_btn <- function(label, cls, inp, val)
      tags$button(label, class = paste("btn btn-xs", cls),
        style = "margin:0 1px;",
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority:'event'})",
                          inp, jsonlite::toJSON(val, auto_unbox = TRUE)))

    # Live reachability badge from the last Ping — additive to the
    # invite-state badge above (that reflects invite lifecycle, this
    # reflects "did it actually answer recently"). Logic lives in the pure
    # ping_badge_state() so it's unit-testable outside Shiny.
    ping_badge <- function(url) {
      state <- ping_badge_state(rv$ping_status[[url]])
      if (is.null(state)) return(NULL)
      switch(state,
        connected = span(class = "sbadge sb-ok",   "Connected"),
        stale     = span(class = "sbadge sb-info", "Stale"),
        offline   = span(class = "sbadge sb-warn", "Offline"))
    }

    rows <- list()
    if (!is.null(reg)) for (sid in names(reg$sites)) {
      r <- reg$sites[[sid]]
      actions <- tagList(
        if (!is.null(r$pending))
          act_btn("Approve", "btn-success", "approve_sid", sid),
        act_btn("Revoke", "btn-danger", "revoke_sid", sid))
      # Name + address stacked in one cell (address as a small subtitle line)
      # instead of side-by-side columns — a full "http://100.x.x.x:8000" URL
      # otherwise squeezes the Name column so tight that even short names
      # wrap mid-word.
      rows[[length(rows) + 1L]] <- tags$tr(
        tags$td(
          div(class = "site-name", if (nzchar(r$name)) r$name else "(unnamed)"),
          if (!is.null(r$site_addr)) div(class = "site-addr-sub", r$site_addr)),
        tags$td(badge(r$invite_state, r$pending),
               { lbl <- invite_expiry_label(r$exp, r$invite_state)
                 if (!is.null(lbl)) div(class = "site-addr-sub", lbl) }),
        tags$td(if (!is.null(r$site_addr)) ping_badge(r$site_addr)),
        tags$td(actions))
    }
    if (!length(rows))
      return(div(class = "note", "No sites yet. Click “Invite a site”."))

    tags$table(class = "sites-tbl",
      tags$thead(tags$tr(tags$th("Site"),
                         tags$th("Status"), tags$th("Reachability"), tags$th(""))),
      tags$tbody(rows))
  })

  # ---- Invite a site --------------------------------------------
  observeEvent(input$btn_invite, {
    showModal(modalDialog(
      title = "Invite a site",
      p(class = "note", style = "margin-top:0;",
        "Study: ", strong(trimws(input$study_name)),
        " — change it in the sidebar if that's wrong."),
      textInput("inv_name", "Site name (label)", placeholder = "e.g. Karolinska"),
      if (!nzchar(COORD_ADDR))
        div(class = "sbadge sb-warn",
            "Tailscale not detected — this invite will not be reachable by remote sites."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("inv_make", "Create invite", class = "btn-primary")),
      easyClose = TRUE))
  })

  observeEvent(input$inv_make, {
    name  <- trimws(input$inv_name)
    study <- trimws(input$study_name)

    if (!nzchar(name)) {
      showNotification("Enter a site name.", type = "warning"); return()
    }
    if (site_name_exists(rv$reg, name)) {
      showNotification(
        sprintf('A site named "%s" already exists — pick a different name, or revoke the existing one first.', name),
        type = "error", duration = 8)
      return()
    }
    if (!nzchar(study)) study <- "Federated study"

    sid   <- fed_sid()
    token <- fed_token()
    exp   <- as.integer(Sys.time()) + INVITE_TTL_DAYS * 86400L

    invite <- fed_invite_create(
      study = study, coord = COORD_ADDR, sid = sid, token = token,
      private_key = COORD_KEY$private, name = name, ttl_days = INVITE_TTL_DAYS)

    # Store the invite text itself (not just the registry state) so the
    # registrar can serve it back at a short link.
    reg_modify(REG_FILE, function(reg)
      reg_add_invite(reg, sid, name, study, token, exp, invite = invite))
    rv$reg <- reg_load(REG_FILE)

    short_link <- if (nzchar(COORD_ADDR)) sprintf("http://%s/i/%s", COORD_ADDR, sid) else ""
    # /get (a real page with a visible download button), not /kit directly —
    # a silent no-page download is unreliable/invisible across browsers.
    kit_link   <- if (nzchar(COORD_ADDR)) sprintf("http://%s/get", COORD_ADDR) else ""
    onboarding_msg <- if (nzchar(short_link) && nzchar(kit_link))
      build_onboarding_message(study, kit_link, short_link) else ""
    mailto_subject <- sprintf('Join the "%s" federated study', study)
    invite_mailto  <- if (nzchar(short_link))
      build_mailto_link(mailto_subject,
                        sprintf("Here's your invite to join the study:\n\n%s", short_link)) else ""
    onboarding_mailto <- if (nzchar(onboarding_msg))
      build_mailto_link(mailto_subject, onboarding_msg) else ""

    removeModal()
    showModal(modalDialog(
      title = "Invite created",
      p("Send either of these to the site operator — both work. It expires in ",
        strong(paste0(INVITE_TTL_DAYS, " day(s)")), "."),
      if (nzchar(short_link)) tagList(
        div(class = "sec-lbl", "Short link (easiest to share)"),
        tags$input(id = "invite_link", class = "form-control", readonly = NA,
                   value = short_link,
                   style = "font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace; font-size:.92rem;"),
        div(style = "margin-top:6px; margin-bottom:14px;",
            tags$button("Copy link", class = "btn btn-primary btn-sm",
                        onclick = "fedCopy('invite_link')"),
            if (nzchar(invite_mailto))
              tags$a("Open in email", href = invite_mailto,
                     class = "btn btn-default btn-sm", style = "margin-left:6px;"))
      ),
      div(class = "sec-lbl", "Full invite text"),
      tags$textarea(id = "invite_str", class = "inv-box", readonly = NA, invite),
      div(style = "margin-top:6px;",
          tags$button("Copy text", class = "btn btn-default btn-sm",
                      onclick = "fedCopy('invite_str')")),
      if (nzchar(onboarding_msg)) tags$details(
        style = "margin-top:16px;",
        tags$summary(style = "cursor:pointer; font-weight:600; color:var(--brand-deep);",
                     "First time? This operator doesn't have the software yet"),
        div(style = "margin-top:10px;",
            p(class = "note", style = "margin-top:0;",
              "One message covering Tailscale setup, the software, and this invite — ",
              "send it instead of the invite alone."),
            div(class = "sec-lbl", "Site kit link"),
            p(class = "note", style = "margin-top:0;",
              "Copy this and send it — it's for the operator to open, not you. It only ",
              "works once they're on the tailnet."),
            tags$input(id = "kit_link_input", class = "form-control", readonly = NA,
                       value = kit_link,
                       style = "font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace; font-size:.92rem;"),
            div(style = "margin-top:6px; margin-bottom:14px;",
                tags$button("Copy kit link", class = "btn btn-default btn-sm",
                           onclick = "fedCopy('kit_link_input')")),
            div(class = "sec-lbl", style = "margin-top:14px;", "Full onboarding message"),
            tags$textarea(id = "onboard_msg", class = "inv-box", readonly = NA,
                          style = "height:220px;", onboarding_msg),
            div(style = "margin-top:6px;",
                tags$button("Copy onboarding message", class = "btn btn-primary btn-sm",
                            onclick = "fedCopy('onboard_msg')"),
                if (nzchar(onboarding_mailto))
                  tags$a("Open in email", href = onboarding_mailto,
                         class = "btn btn-default btn-sm", style = "margin-left:6px;")))
      ),
      div(class = "fp", style = "margin-top:10px;",
          "Your key fingerprint (read it to the operator to verify): ",
          tags$b(COORD_FP)),
      footer = modalButton("Done"), easyClose = TRUE, size = "l"))
  })

  # ---- Approve a held registration ------------------------------
  observeEvent(input$approve_sid, {
    sid <- input$approve_sid
    reg_modify(REG_FILE, function(reg) reg_approve_pending(reg, sid))
    rv$reg <- reg_load(REG_FILE)
    showNotification("Registration approved.", type = "message")
  })

  # ---- Revoke and Remove (confirm first — not reversible: the token goes
  # on the permanent revocation list and the row disappears) --------
  observeEvent(input$revoke_sid, {
    sid <- input$revoke_sid
    r   <- rv$reg$sites[[sid]]
    label <- if (!is.null(r) && nzchar(r$name)) r$name else "(unnamed)"
    showModal(modalDialog(
      title = "Revoke this site?",
      p("This removes ", strong(label), " and permanently revokes its token. ",
        "This can't be undone — if they need to rejoin, you'll have to send a new invite."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("revoke_confirm", "Revoke", class = "btn-danger")),
      easyClose = TRUE))
    rv$pending_revoke_sid <- sid
  })

  observeEvent(input$revoke_confirm, {
    sid <- rv$pending_revoke_sid
    removeModal()
    if (is.null(sid)) return()
    reg_modify(REG_FILE, function(reg) reg_revoke_remove(reg, sid))
    rv$reg <- reg_load(REG_FILE)
    rv$pending_revoke_sid <- NULL
    showNotification("Site revoked and removed.", type = "message")
  })

  # ---- Ping -----------------------------------------------------
  # Async (future/promises): sites are pinged in parallel, in the
  # background, so this session's own reactivity (other buttons, sidebar)
  # doesn't freeze while pings are in flight. Wall-clock time is
  # max(latency) across sites instead of sum(latency).
  observeEvent(input$btn_ping, {
    sites <- active_sites()
    if (!length(sites)) {
      showNotification("No sites to ping. Invite or add a site first.",
                       type = "warning")
      return()
    }
    progress <- shiny::Progress$new()
    progress$set(message = "Pinging sites…")

    ping_sites_async(sites) %...>% (function(lines) {
      now <- Sys.time()
      status <- rv$ping_status
      for (i in seq_along(sites))
        status[[sites[[i]]$url]] <- list(ok = grepl("\\[OK\\]", lines[[i]]),
                                         checked_at = now)
      rv$ping_status <- status
      output$ping_out <- renderText(paste(lines, collapse = "\n"))
      progress$close()
    }) %...!% (function(e) {
      showNotification(paste("Ping failed:", conditionMessage(e)), type = "error")
      progress$close()
    })
  })

  # ---- Validate data --------------------------------------------
  observeEvent(input$btn_validate, {
    sites <- active_sites()
    if (!length(sites)) {
      showNotification("No sites yet. Invite or add a site first.", type = "warning"); return()
    }
    if (is.null(rv$meta) || is.null(rv$meta$vars_spec)) {
      showNotification("Load an analysis script first.", type = "warning"); return()
    }
    ss <- lapply(sites, function(s) create_remote_server(s$url, s$token, label = s$name))

    withProgress(message = "Validating sites…", {
      v <- tryCatch(
        fed_validate(ss, rv$meta$vars_spec, formula = NULL, min_n = 20L),
        error = function(e) list(ok = FALSE,
          errors = conditionMessage(e), warnings = character(0),
          site_reports = list(), heterogeneity = list())
      )
    })

    rv$val_txt  <- cap_print(print_validation_report(v))
    rv$outputs  <- NULL   # clear old results so validation tab shows

    if (v$ok)
      showNotification("Validation passed. Ready to run analysis.",
                       type = "message")
    else
      showNotification(
        sprintf("%d error(s) found — review the Validation tab.",
                length(v$errors)), type = "error", duration = 8)
  })

  # ---- Run analysis ---------------------------------------------
  observeEvent(input$btn_run, {
    sites <- active_sites()
    if (!length(sites)) {
      showNotification("No sites yet. Invite or add a site first.", type = "warning"); return()
    }
    if (is.null(rv$meta) || is.null(rv$script_path)) {
      showNotification("Choose an analysis first.", type = "warning"); return()
    }
    ss     <- lapply(sites, function(s) create_remote_server(s$url, s$token, label = s$name))
    script <- rv$script_path

    # capture.output() only grabs printed output; warnings go to stderr and
    # would be lost. Collect them via a calling handler so privacy-suppression
    # notices (raised by fed_group_numeric et al.) reach the operator.
    warn_msgs <- character(0)

    withProgress(
      message = paste("Running:", rv$meta$title), value = 0, {

      incProgress(0.05, detail = "Preparing…")
      clear_outputs()
      env         <- new.env(parent = globalenv())
      env$servers <- ss

      incProgress(0.10, detail = "Sourcing analysis script…")
      captured <- tryCatch(
        withCallingHandlers(
          capture.output(source(script, local = env)),
          warning = function(w) {
            warn_msgs <<- c(warn_msgs, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          showNotification(paste("Script error:", conditionMessage(e)),
                           type = "error", duration = 15)
          character(0)
        }
      )
      log_parts <- captured
      if (length(warn_msgs))
        log_parts <- c(log_parts, "", "──────── Warnings ────────", warn_msgs)
      rv$console_log  <- if (length(log_parts)) paste(log_parts, collapse = "\n") else ""
      rv$run_warnings <- warn_msgs

      incProgress(1, detail = "Done")
    })

    # Privacy suppression changes what the results mean (some small subgroups
    # were withheld), so surface it prominently — a sticky warning, not buried
    # in the log.
    if (any(grepl("Privacy suppression", warn_msgs)))
      showNotification(
        HTML(paste0("<b>Privacy protection applied.</b> One or more small subgroups ",
                    "(fewer patients than the privacy threshold) were hidden to protect ",
                    "individual patients. Affected pooled figures exclude the withheld ",
                    "site(s) — see the Console log for details.")),
        type = "warning", duration = NULL)

    result <- get_outputs()
    if (length(result) == 0) {
      showNotification(
        "No outputs were registered. Check the script calls register_output().",
        type = "warning")
    } else {
      rv$outputs <- result
      showNotification(
        sprintf("Done — %d output(s) ready.", length(result)),
        type = "message")
    }
  })

  # ---- Main panel -----------------------------------------------
  # Readiness note (setup phase only — see the phase-transition observe()
  # above). Once in "results" phase this whole sidebar area has already
  # done its job, so it's hidden rather than left showing a stale "Ready".
  output$readiness_note <- renderUI({
    if (!identical(rv$phase, "setup")) return(NULL)
    div(class = "note", style = "margin-top:8px;",
       build_readiness_note(!is.null(rv$meta), length(active_sites())))
  })

  output$results_panel <- renderUI({
    # mainPanel is hidden (display:none) until rv$phase == "results", which
    # itself requires rv$meta to already be set (Validate/Run both check
    # that first) — so by the time this is ever visible, a script is
    # guaranteed loaded. No "no script yet" branch needed.

    # Build tab list: always include Status tab, add output tabs if available
    all_tabs <- list()

    # Status tab: shows validation output or a ready-prompt
    status_body <- if (!is.null(rv$val_txt)) {
      tagList(h5("Validation report"), verbatimTextOutput("val_display"))
    } else if (is.null(rv$outputs)) {
      div(class = "welcome",
          h4(rv$meta$title),
          p("Script loaded. Click Validate or Run Analysis."))
    } else {
      div(class = "welcome",
          p("Analysis complete. See output tabs above."))
    }
    all_tabs[[1]] <- tabPanel("Status", br(), status_body)

    # Dynamic output tabs
    if (!is.null(rv$outputs)) {
      for (i in seq_along(rv$outputs)) {
        out <- rv$outputs[[i]]
        id  <- paste0("dyn_", i)
        inner <- switch(out$type,
          "table" = tableOutput(id),
          "plot"  = plotOutput(id, height = "440px"),
          "text"  = verbatimTextOutput(id)
        )
        all_tabs[[length(all_tabs) + 1L]] <- tabPanel(
          out$name,
          br(),
          inner,
          if (!is.null(out$caption))
            div(class = "tbl-caption", out$caption)
        )
      }
    }

    # Console tab: cat()/print() output captured from the analysis script
    if (!is.null(rv$console_log)) {
      all_tabs[[length(all_tabs) + 1L]] <- tabPanel(
        "Console",
        br(),
        verbatimTextOutput("console_out")
      )
    }

    tabs <- do.call(tabsetPanel, c(list(id = "dyn_tabs"), all_tabs))

    # Save-results bar: a single self-contained HTML report (all tables,
    # plots and text) the coordinator can keep, print to PDF, or share.
    # Shown whenever there is something to save.
    have_results <- !is.null(rv$outputs) || !is.null(rv$val_txt)
    if (!have_results) return(tabs)
    has_table <- !is.null(rv$outputs) &&
      any(vapply(rv$outputs, function(o) identical(o$type, "table"), logical(1)))

    # Persistent privacy banner: stays visible above the results the whole
    # time they're shown (not just a dismissable toast at run time), so the
    # fact that small subgroups were withheld can't be missed or forgotten —
    # matching the notice embedded in every exported file.
    priv_msgs <- grep("Privacy suppression", rv$run_warnings, value = TRUE)
    privacy_banner <- if (length(priv_msgs) > 0)
      div(class = "privacy-banner",
          strong("Privacy protection applied. "),
          "Small subgroups (fewer patients than the site privacy threshold) were ",
          "withheld to protect individual patients; affected pooled figures exclude ",
          "the withheld site(s).",
          tags$ul(lapply(priv_msgs, tags$li)))

    tagList(
      privacy_banner,
      div(style = "display:flex; justify-content:flex-end; gap:8px; margin-bottom:10px;",
          if (has_table)
            downloadButton("download_csv", "CSV", class = "btn-default btn-sm"),
          if (has_table)
            downloadButton("download_xlsx", "Excel", class = "btn-default btn-sm"),
          downloadButton("download_report", "Report (HTML)",
                         class = "btn-primary btn-sm")),
      tabs
    )
  })

  # val_display is always registered; shown inside the Status tab
  output$val_display <- renderText({
    if (is.null(rv$val_txt)) "" else rv$val_txt
  })

  # console_out: captured cat()/print() from the analysis script
  output$console_out <- renderText({
    log <- rv$console_log
    if (is.null(log) || !nzchar(log)) "(no printed output)" else log
  })

  # ---- Save results: one self-contained HTML report ---------------
  output$download_report <- downloadHandler(
    filename = function() {
      ttl  <- if (!is.null(rv$meta)) rv$meta$title else "federated-analysis"
      slug <- gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", trimws(ttl)))
      sprintf("%s_%s.html", if (nzchar(slug)) slug else "results",
              format(Sys.time(), "%Y-%m-%d_%H%M"))
    },
    content = function(file) {
      html <- build_results_html(
        title       = if (!is.null(rv$meta)) rv$meta$title else NULL,
        outputs     = rv$outputs,
        val_txt     = rv$val_txt,
        console_log = rv$console_log,
        warnings    = rv$run_warnings,
        n_sites     = length(active_sites())
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )

  # ---- Save results: an Excel workbook (a sheet per table) --------
  output$download_xlsx <- downloadHandler(
    filename = function() {
      ttl  <- if (!is.null(rv$meta)) rv$meta$title else "federated-analysis"
      slug <- gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", trimws(ttl)))
      sprintf("%s_%s.xlsx", if (nzchar(slug)) slug else "results",
              format(Sys.time(), "%Y-%m-%d_%H%M"))
    },
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE))
        stop("The 'writexl' package is needed for Excel export. Install it with install.packages('writexl').")
      wb <- build_results_workbook(
        outputs  = rv$outputs,
        title    = if (!is.null(rv$meta)) rv$meta$title else NULL,
        val_txt  = rv$val_txt,
        warnings = rv$run_warnings,
        n_sites  = length(active_sites())
      )
      writexl::write_xlsx(wb, path = file)
    }
  )

  # ---- Save results: CSV (one table -> .csv; several -> .zip) -----
  .result_tables <- reactive({
    if (is.null(rv$outputs)) list()
    else Filter(function(o) identical(o$type, "table"), rv$outputs)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      ttl  <- if (!is.null(rv$meta)) rv$meta$title else "federated-analysis"
      slug <- gsub("(^-|-$)", "", gsub("[^A-Za-z0-9]+", "-", trimws(ttl)))
      if (!nzchar(slug)) slug <- "results"
      stamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      ext   <- if (length(.result_tables()) <= 1L) "csv" else "zip"
      sprintf("%s_%s.%s", slug, stamp, ext)
    },
    content = function(file) {
      tabs <- .result_tables()
      if (length(tabs) == 1L) {
        # single table: a plain CSV, ready to import anywhere
        utils::write.csv(as.data.frame(tabs[[1]]$value), file,
                         row.names = FALSE, fileEncoding = "UTF-8")
        return(invisible())
      }
      # several tables: one CSV each + study_info.csv, bundled into a zip
      if (!requireNamespace("zip", quietly = TRUE))
        stop("The 'zip' package is needed to bundle multiple CSVs. Install it with install.packages('zip').")
      td <- tempfile("csvs"); dir.create(td)
      on.exit(unlink(td, recursive = TRUE), add = TRUE)
      used <- character(0)
      for (o in tabs) {
        fn <- .safe_filename(o$name, ".csv", used); used <- c(used, fn)
        utils::write.csv(as.data.frame(o$value), file.path(td, fn),
                         row.names = FALSE, fileEncoding = "UTF-8")
      }
      utils::write.csv(
        .results_info_df(if (!is.null(rv$meta)) rv$meta$title else NULL,
                         length(active_sites()), rv$run_warnings, Sys.time()),
        file.path(td, "study_info.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      zip::zip(zipfile = file, files = list.files(td), root = td)
    }
  )

  # ---- Register a renderer for every dynamic output ---------------
  observeEvent(rv$outputs, {
    outputs <- rv$outputs
    if (is.null(outputs)) return()

    for (i in seq_along(outputs)) {
      local({
        out <- outputs[[i]]
        id  <- paste0("dyn_", i)

        if (out$type == "table") {

          output[[id]] <- renderTable(
            out$value,
            striped = TRUE, hover = TRUE, na = ""
          )

        } else if (out$type == "plot") {

          fn <- out$value   # capture the closure
          output[[id]] <- renderPlot({
            if (is.function(fn))        fn()
            else if (inherits(fn, "ggplot")) print(fn)
            else { plot.new(); text(0.5, 0.5, "Cannot render plot.", cex = 1.2) }
          }, height = 440)

        } else {   # "text"

          txt <- out$value
          output[[id]] <- renderText(
            if (is.character(txt)) txt
            else cap_print(print(txt))
          )

        }
      })
    }

    # Switch to the first output tab automatically
    if (length(outputs) > 0)
      updateTabsetPanel(session, "dyn_tabs",
                        selected = outputs[[1]]$name)

  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
