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

# Pure function (no Shiny dependency): given which of the three pipeline
# stages are complete, returns "done" / "active" / "pending" for each —
# "active" is the first not-yet-done stage, matching a standard wizard
# stepper (only one step is ever the current focus).
pipeline_step_states <- function(done) {
  first_pending <- which(!done)[1]
  vapply(seq_along(done), function(i) {
    if (isTRUE(done[i])) "done"
    else if (!is.na(first_pending) && i == first_pending) "active"
    else "pending"
  }, character(1))
}

# Top-of-main-panel progress stepper reflecting real pipeline state (not
# static instructions — the sidebar's own step labels already cover that).
build_stepper <- function(step_script, n_sites, step_results) {
  states <- pipeline_step_states(c(step_script, n_sites > 0, step_results))
  labels <- c("Script", if (n_sites > 0) sprintf("Sites (%d)", n_sites) else "Sites", "Results")
  items  <- lapply(seq_along(states), function(i)
    tagList(
      if (i > 1) div(class = "step-line"),
      div(class = paste("step-item", paste0("step-", states[i])),
          div(class = "step-num", if (states[i] == "done") "✓" else i),
          div(class = "step-label", labels[i]))
    ))
  div(class = "stepper", items)
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* ── Palette: professional / trustworthy / tech ──────────────── */
    :root {
      --brand:      #2563EB;
      --brand-dark: #1D4ED8;
      --brand-deep: #1E40AF;
      --ink:        #0F172A;
      --ink-muted:  #475569;
      --line:       #E2E8F0;
      --tint:       #EFF6FF;
      --ok-bg:#DCFCE7; --ok-fg:#166534;
      --warn-bg:#FEF3C7; --warn-fg:#92400E;
      --bad-bg:#FEE2E2; --bad-fg:#991B1B;
      --muted-bg:#F1F5F9; --muted-fg:#64748B;
    }
    html  { font-size:16px; }
    body  { font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
            color:var(--ink); font-size:1rem; line-height:1.6; }
    .well { background-color:#fff; border:1px solid var(--line); border-radius:14px;
            box-shadow:0 6px 20px rgba(15,23,42,.05); padding:20px; }
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
                          box-shadow:0 2px 8px rgba(37,99,235,.25); }
    .btn-primary:hover,
    .btn-primary:focus  { background-color: var(--brand-dark) !important;
                          border-color: var(--brand-deep) !important;
                          box-shadow:0 4px 14px rgba(37,99,235,.35);
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
    .sbadge { display:inline-block; padding:3px 12px; border-radius:20px;
              font-size:0.78rem; font-weight:700; }
    .sb-ok    { background:var(--ok-bg);    color:var(--ok-fg); }
    .sb-info  { background:var(--warn-bg);  color:var(--warn-fg); }
    .sb-warn  { background:var(--bad-bg);   color:var(--bad-fg); }
    .sb-muted { background:var(--muted-bg); color:var(--muted-fg); }
    .coord-addr { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                  font-size:0.9rem; color:var(--brand-deep);
                  background:var(--tint); border:1px solid #BFDBFE;
                  border-radius:10px; padding:8px 12px; word-break:break-all;
                  box-shadow:0 2px 8px rgba(37,99,235,.06); }
    .fp { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
          font-size:0.85rem; color:var(--ink-muted); }
    .inv-box { width:100%; height:130px;
               font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
               font-size:0.82rem; word-break:break-all; border-radius:10px; border-color:var(--line);
               padding:12px; }
    /* ── Pipeline stepper (top of main panel) ─────────────────────── */
    .stepper { display:flex; align-items:flex-start; margin-bottom:30px; }
    .step-item { display:flex; flex-direction:column; align-items:center; gap:7px; }
    .step-num { width:34px; height:34px; border-radius:50%; display:flex;
                align-items:center; justify-content:center; font-weight:700;
                font-size:.92rem; background:var(--muted-bg); color:var(--muted-fg);
                border:2px solid var(--line); transition:all .2s ease; }
    .step-label { font-size:.82rem; color:var(--ink-muted); font-weight:600; white-space:nowrap; }
    .step-line  { flex:1; height:2px; background:var(--line); margin:17px 10px 0; }
    .step-active .step-num   { background:var(--brand); color:#fff; border-color:var(--brand);
                                box-shadow:0 0 0 5px var(--tint); }
    .step-active .step-label { color:var(--brand-deep); }
    .step-done .step-num     { background:var(--ok-bg); color:var(--ok-fg); border-color:#86EFAC; }
    .step-done .step-label   { color:var(--ok-fg); }
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
  "))),

  tags$script(HTML("
    function fedCopy(id){
      var el = document.getElementById(id);
      if(!el) return;
      el.select(); el.setSelectionRange(0, 99999);
      navigator.clipboard.writeText(el.value);
    }
  ")),

  uiOutput("app_title_ui"),

  sidebarLayout(
    sidebarPanel(
      width = 5,

      # ---- Step 1: analysis script ----
      div(class = "step", "① Analysis script"),
      fileInput("script_file", NULL, accept = ".R",
                buttonLabel = "Browse…", placeholder = "No script selected"),
      uiOutput("script_meta_ui"),

      # ---- Step 2: sites ----
      div(class = "step", "② Sites"),
      uiOutput("registrar_status_ui"),
      div(style = "margin:8px 0;",
          actionButton("btn_invite", "Invite a site",
                       class = "btn-primary btn-sm")),
      uiOutput("sites_table_ui"),

      br(),
      actionButton("btn_ping",     "Ping sites",
                   class = "btn-default btn-block"),
      br(),
      actionButton("btn_validate", "Validate data",
                   class = "btn-primary btn-block"),
      br(),
      actionButton("btn_run",      "Run analysis",
                   class = "btn-success btn-block"),

      hr(),
      verbatimTextOutput("ping_out"),

      div(class = "note",
          "Only aggregate statistics leave each site.",
          br(), "No individual patient data is transferred.")
    ),

    mainPanel(
      width = 7,
      uiOutput("results_panel")
    )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    meta        = NULL,   # list(title, vars_spec)
    outputs     = NULL,   # list of register_output() entries
    val_txt     = NULL,   # text from standalone Validate
    console_log = NULL,   # captured cat()/print() output from analysis script
    reg         = reg_load(REG_FILE),  # registered-sites registry (polled)
    ping_status = list()   # url -> list(ok, checked_at) from the last ping
  )

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

  # ---- Load script: extract metadata ----------------------------
  observeEvent(input$script_file, {
    req(input$script_file)
    rv$meta    <- tryCatch(
      .extract_meta(input$script_file$datapath),
      error = function(e) {
        showNotification(paste("Could not read script:", e$message),
                         type = "error"); NULL
      })
    rv$outputs     <- NULL
    rv$val_txt     <- NULL
    rv$console_log <- NULL
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
        tags$td(badge(r$invite_state, r$pending)),
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
      textInput("inv_study", "Study name",
                value = if (!is.null(rv$meta)) rv$meta$title else "Federated study"),
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
    study <- trimws(input$inv_study)
    sid   <- fed_sid()
    token <- fed_token()
    exp   <- as.integer(Sys.time()) + INVITE_TTL_DAYS * 86400L
    reg_modify(REG_FILE, function(reg)
      reg_add_invite(reg, sid, name, study, token, exp))
    rv$reg <- reg_load(REG_FILE)

    invite <- fed_invite_create(
      study = study, coord = COORD_ADDR, sid = sid, token = token,
      private_key = COORD_KEY$private, name = name, ttl_days = INVITE_TTL_DAYS)

    removeModal()
    showModal(modalDialog(
      title = "Invite created",
      p("Send this invite to the site operator. It expires in ",
        strong(paste0(INVITE_TTL_DAYS, " day(s)")), "."),
      tags$textarea(id = "invite_str", class = "inv-box", readonly = NA, invite),
      div(style = "margin-top:6px;",
          tags$button("Copy", class = "btn btn-primary btn-sm",
                      onclick = "fedCopy('invite_str')")),
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

  # ---- Revoke and Remove ----------------------------------------
  observeEvent(input$revoke_sid, {
    sid <- input$revoke_sid
    reg_modify(REG_FILE, function(reg) reg_revoke_remove(reg, sid))
    rv$reg <- reg_load(REG_FILE)
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
    ss <- lapply(sites, function(s) create_remote_server(s$url, s$token))

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
    if (is.null(rv$meta)) {
      showNotification("Load an analysis script first.", type = "warning"); return()
    }
    ss     <- lapply(sites, function(s) create_remote_server(s$url, s$token))
    script <- input$script_file$datapath

    withProgress(
      message = paste("Running:", rv$meta$title), value = 0, {

      incProgress(0.05, detail = "Preparing…")
      clear_outputs()
      env         <- new.env(parent = globalenv())
      env$servers <- ss

      incProgress(0.10, detail = "Sourcing analysis script…")
      captured <- tryCatch(
        capture.output(source(script, local = env)),
        error = function(e) {
          showNotification(paste("Script error:", conditionMessage(e)),
                           type = "error", duration = 15)
          character(0)
        }
      )
      rv$console_log <- if (length(captured)) paste(captured, collapse = "\n") else ""

      incProgress(1, detail = "Done")
    })

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
  output$results_panel <- renderUI({

    stepper <- build_stepper(!is.null(rv$meta), length(active_sites()), !is.null(rv$outputs))

    # No script loaded yet — the sidebar's own numbered steps already say
    # what to do; this just needs to say results aren't here yet.
    if (is.null(rv$meta)) {
      return(tagList(stepper, div(class = "welcome",
        p("Results will appear here once you load an analysis script."))))
    }

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

    tagList(stepper, do.call(tabsetPanel, c(list(id = "dyn_tabs"), all_tabs)))
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
