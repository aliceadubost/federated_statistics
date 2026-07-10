# engine/site/site_app.R
# =====================================================================
# Federated Statistics — Site Server GUI
#
# Launched by the Start Site launchers in Run/Mac, Run/Linux, Run/Windows.
# Runs the plumber API (api_server.R) as a background subprocess and
# streams its output into the browser log panel.
#
# Paste a coordinator invite and click Join. The invite is verified
# (Ed25519 signature, expiry), the coordinator's key is pinned (TOFU), a
# per-site key and config are saved, and after the server starts the site
# registers itself back to the coordinator automatically. Joining via
# invite is required — there is no shared-token / manual-address path.
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(processx)
  library(jsonlite)
})

# shiny::runApp() sets cwd to the app directory (engine/site/).
# Navigate up two levels to reach the project root.
.app_root    <- normalizePath(file.path(getwd(), "..", ".."))
.api_script  <- file.path(.app_root, "engine", "site", "api_server.R")
.config_file <- file.path(.app_root, "engine", "site", "site_config.json")
.fedstats_ok <- requireNamespace("fedstats", quietly = TRUE) && file.exists(.api_script)

# ---- Auto-detect CSV files ------------------------------------------
# A browser file-input dialog can't be told what folder to open (that's a
# browser security restriction — no site can override it, for anyone). So
# instead: any CSV dropped in <project root>/data/ is auto-detected and
# offered as a dropdown, skipping the OS file dialog entirely. Create the
# folder so it's a visible, obvious drop target from a fresh checkout.
.data_dir <- file.path(.app_root, "data")
if (!dir.exists(.data_dir))
  dir.create(.data_dir, recursive = TRUE, showWarnings = FALSE)

.scan_csvs <- function() {
  found <- list.files(.data_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(found)) return(found)
  list.files(.app_root, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
}

# ---- Tailscale IP ---------------------------------------------------
# A function, not a one-shot constant: if Tailscale connects *after* this
# app is already open, a static value computed once at startup would never
# update — the operator would see "not detected" indefinitely and need to
# restart the app. server() below polls this on a timer instead.
.get_ts_ip <- function() {
  tryCatch({
    cmd <- if (.Platform$OS.type == "windows") "tailscale ip -4"
           else "tailscale ip -4 2>/dev/null"
    ip  <- trimws(system(cmd, intern = TRUE, ignore.stderr = TRUE))
    if (length(ip) && nzchar(ip[1])) ip[1] else ""
  }, error = function(e) "")
}

# ---- Automatic port selection ---------------------------------------
# Nobody who doesn't already know what a port is should have to pick one.
# Try 8000 upward and use the first free one. httpuv::startServer() binds
# synchronously and throws immediately on failure (no waiting for a client
# to connect, unlike a plain socketConnection(server = TRUE)) — it's
# already a transitive dependency via shiny, so this adds nothing new.
.port_is_free <- function(port) {
  tryCatch({
    srv <- httpuv::startServer("0.0.0.0", port, list())
    httpuv::stopServer(srv)
    TRUE
  }, error = function(e) FALSE)
}
.find_free_port <- function(start = 8000L, tries = 50L) {
  for (p in start:(start + tries - 1L)) if (.port_is_free(p)) return(p)
  NA_integer_
}

# ---- Site config persistence ----------------------------------------
.load_config <- function() {
  if (!file.exists(.config_file)) return(NULL)
  cfg <- tryCatch(jsonlite::fromJSON(.config_file, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(cfg) || is.null(cfg$sid) || is.null(cfg$token)) return(NULL)
  cfg
}

.save_config <- function(cfg) {
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE), .config_file)
  if (.fedstats_ok) try(fedstats::fed_harden_file(.config_file), silent = TRUE)
}

# Register this site back to its coordinator. Returns a status string.
.do_register <- function(cfg, port, ts_ip) {
  if (!.fedstats_ok) return("fedstats not installed — cannot register.")
  addr <- if (nzchar(ts_ip)) sprintf("http://%s:%d", ts_ip, port)
          else sprintf("http://localhost:%d", port)
  ts  <- as.integer(Sys.time())
  msg <- fedstats::fed_register_message(cfg$sid, addr, cfg$site_pub, ts)
  sig <- fedstats::fed_sign(msg, cfg$site_priv)
  body <- jsonlite::toJSON(list(sid = cfg$sid, site_addr = addr,
                                site_pk = cfg$site_pub, ts = ts, sig = sig),
                           auto_unbox = TRUE)
  url <- paste0("http://", cfg$coord, "/register")
  tryCatch({
    r <- httr::POST(url,
                    httr::add_headers(Authorization = paste("Bearer", cfg$token),
                                      `Content-Type` = "application/json"),
                    body = body, encode = "raw", httr::timeout(8))
    sc <- httr::status_code(r)
    if (sc == 200)      "Registered with coordinator."
    else if (sc == 409) "Waiting for coordinator to approve this registration (new address/host)."
    else if (sc == 403) "Coordinator rejected: invite expired or revoked."
    else                sprintf("Coordinator rejected registration (HTTP %d).", sc)
  }, error = function(e)
    sprintf("Could not reach coordinator at %s — is it running?", cfg$coord))
}

# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* ── Palette: professional / trustworthy / tech ──────────────── */
    :root {
      --brand:      #2563EB;   /* primary action / accent */
      --brand-dark: #1D4ED8;   /* hover / active */
      --brand-deep: #1E40AF;   /* text-on-tint accent */
      --ink:        #0F172A;   /* primary text */
      --ink-muted:  #475569;   /* secondary text */
      --line:       #E2E8F0;   /* borders */
      --tint:       #EFF6FF;   /* light surfaces */
      --ok-bg:#DCFCE7; --ok-fg:#166534;
      --warn-bg:#FEF3C7; --warn-fg:#92400E;
      --bad-bg:#FEE2E2; --bad-fg:#991B1B;
    }
    html { font-size:16px; }
    body { font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
           color:var(--ink); max-width:920px; margin:0 auto; padding:36px 32px 48px;
           font-size:1rem; line-height:1.6; }
    .app-header { display:flex; align-items:center; gap:12px;
                  padding-bottom:20px; margin-bottom:24px; border-bottom:1px solid var(--line); }
    .app-dot { width:12px; height:12px; border-radius:50%; flex:0 0 auto;
               background:linear-gradient(135deg,var(--brand),var(--brand-deep));
               box-shadow:0 0 0 4px var(--tint); }
    h3   { margin:0; color:var(--ink); font-weight:800; font-size:1.9rem; letter-spacing:-.02em; }
    .bdg { display:inline-block; padding:5px 14px; border-radius:20px;
           font-size:.8rem; font-weight:700; margin-left:12px; vertical-align:middle;
           letter-spacing:.01em; }
    .bdg-stopped  { background:var(--bad-bg);  color:var(--bad-fg); }
    .bdg-starting { background:var(--warn-bg); color:var(--warn-fg); }
    .bdg-running  { background:var(--ok-bg);   color:var(--ok-fg); }
    .addr { background:var(--tint); border:1px solid #BFDBFE; border-radius:14px;
            padding:18px 22px; margin:16px 0;
            box-shadow:0 4px 14px rgba(37,99,235,.08); }
    .addr-lbl { font-size:.78rem; font-weight:700; color:var(--brand-deep);
                text-transform:uppercase; letter-spacing:.07em; }
    .addr-url { font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                font-size:1.35rem; color:var(--brand-deep); font-weight:700; margin-top:6px; }
    .privacy  { background:#F0FDF4; border-left:4px solid #22C55E;
                border-radius:0 10px 10px 0; padding:14px 18px; margin:16px 0;
                font-size:.95rem; color:var(--ok-fg); box-shadow:0 2px 8px rgba(22,101,52,.06); }
    .warn-box { background:#FEF2F2; border-left:4px solid #F87171;
                border-radius:0 10px 10px 0; padding:14px 18px; margin:16px 0;
                font-size:.95rem; color:var(--bad-fg); box-shadow:0 2px 8px rgba(153,27,27,.06); }
    .join-box { background:#fff; border:1px solid var(--line); border-radius:14px;
                padding:22px 24px; margin:14px 0;
                box-shadow:0 6px 20px rgba(15,23,42,.06); }
    .join-status { font-size:.95rem; color:var(--brand-deep); margin-top:8px; }
    .sec-lbl  { font-weight:800; font-size:.88rem; color:var(--ink-muted);
                text-transform:uppercase; letter-spacing:.08em; margin:28px 0 10px 0; }
    #invite { width:100%; height:96px; font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
              font-size:.85rem; word-break:break-all; border-radius:10px; border-color:var(--line);
              padding:12px; }
    #server_log { height:290px; overflow-y:auto; background:#0F172A; color:#E2E8F0;
                  font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
                  font-size:.85rem; border-radius:14px;
                  padding:16px 20px; white-space:pre-wrap; border:none;
                  box-shadow:0 6px 20px rgba(15,23,42,.15); }
    hr { border-color:var(--line); margin:22px 0; }
    .btn, button.btn { border-radius:9px; font-weight:600; padding:9px 20px;
                       font-size:1rem; transition:all .15s ease; }
    .btn-sm { padding:8px 18px; font-size:.92rem; }
    /* fileInput's Browse… button sits in a Bootstrap input-group next to
       the placeholder text box, which keeps its default (smaller) height
       — the bigger padding above made Browse taller than its pair. */
    .btn-file { padding:6px 14px !important; font-size:.92rem !important;
               border-radius:9px 0 0 9px !important; }
    .input-group .form-control { border-radius:0 9px 9px 0; }
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
    label { font-weight:600; font-size:.92rem; color:var(--ink-muted); }
    .selectize-input, select.form-control { border-radius:9px !important; }
  "))),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('scrollLog', function(x) {
      setTimeout(function() {
        var el = document.getElementById('server_log');
        if (el) el.scrollTop = el.scrollHeight;
      }, 60);
    });
  ")),

  div(class = "app-header",
      div(class = "app-dot"),
      tags$h3("Federated Site", uiOutput("status_badge", inline = TRUE))),

  uiOutput("warn_ui"),

  # ---- Join a study (invite flow) --------------------------------
  div(class = "sec-lbl", "Join a study"),
  uiOutput("joined_ui"),
  div(class = "join-box",
      tags$textarea(id = "invite", placeholder = "Paste your invite or invite link here"),
      div(style = "margin-top:8px;",
          actionButton("btn_join", "Join", class = "btn btn-primary btn-sm")),
      uiOutput("join_status_ui")),

  uiOutput("address_ui"),

  hr(),
  div(class = "sec-lbl", "Configuration"),

  fluidRow(
    column(12, uiOutput("file_ui"))
  ),

  fluidRow(
    column(12, uiOutput("action_btn_ui"), style = "margin-top:10px;")
  ),

  hr(),
  div(class = "sec-lbl", "Log"),
  verbatimTextOutput("server_log")
)

# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive so Refresh can re-scan data/ without restarting the app.
  csv_files_rv <- reactiveVal(.scan_csvs())

  rv <- reactiveValues(
    proc         = NULL,
    log          = character(0),
    status       = "stopped",   # "stopped" | "starting" | "running"
    config       = .load_config(),
    join_status  = NULL,        # text shown under the Join box
    need_register = FALSE,      # register once the server reaches "running"
    ts_ip        = .get_ts_ip(),# live Tailscale IP, refreshed below
    port         = NA_integer_  # chosen automatically at Start Server time
  )

  # Re-check Tailscale every few seconds so connecting *after* this app is
  # already open is picked up without restarting it.
  ts_timer <- reactiveTimer(3000)
  observe({
    ts_timer()
    rv$ts_ip <- .get_ts_ip()
  })

  timer <- reactiveTimer(500)

  # ---- Poll subprocess output every 500 ms -----------------------
  observe({
    timer()
    proc <- isolate(rv$proc)
    if (is.null(proc)) return()

    if (proc$is_alive()) {
      new_lines <- c(proc$read_output_lines(), proc$read_error_lines())
      if (length(new_lines)) {
        rv$log <- c(rv$log, new_lines)
        session$sendCustomMessage("scrollLog", list())
      }
      if (isolate(rv$status) == "starting") {
        # Confirm the server is actually answering /health before declaring
        # it "running" — a log line saying "Listening on port N" only means
        # api_server.R printed that *before* attempting to bind; it doesn't
        # mean the bind succeeded (a busy port fails right after that line).
        # Trusting the log alone previously caused a false "Registered with
        # coordinator" for a server that then crashed on bind.
        port  <- isolate(rv$port)
        token <- if (!is.null(isolate(rv$config))) isolate(rv$config)$token else ""
        healthy <- !is.na(port) && tryCatch({
          hdrs <- if (nzchar(token))
            httr::add_headers(Authorization = paste("Bearer", token)) else NULL
          r <- httr::GET(sprintf("http://127.0.0.1:%d/health", port), hdrs, httr::timeout(1))
          httr::status_code(r) == 200L
        }, error = function(e) FALSE)

        if (healthy) {
          rv$status <- "running"
          # Auto-register once the server is confirmed up, if we joined via an invite.
          if (isolate(rv$need_register) && !is.null(isolate(rv$config))) {
            msg <- .do_register(isolate(rv$config), port, isolate(rv$ts_ip))
            rv$join_status <- msg
            rv$log <- c(rv$log, paste0("\n--- ", msg, " ---"))
            rv$need_register <- FALSE
          }
        }
      }
    } else {
      code    <- tryCatch(proc$get_exit_status(), error = function(e) NULL)
      new_err <- proc$read_error_lines()
      rv$log  <- c(rv$log, new_err,
                   sprintf("\n--- server exited (code %s) ---",
                           if (is.null(code)) "?" else as.character(code)))
      rv$status <- "stopped"
      rv$proc   <- NULL
      session$sendCustomMessage("scrollLog", list())
    }
  })

  # ---- Warnings --------------------------------------------------
  output$warn_ui <- renderUI({
    if (!.fedstats_ok)
      div(class = "warn-box",
          strong("Setup required"),
          br(),
          "The fedstats package is not installed",
          br(),
          "Run this in R to fix it",
          br(),
          tags$code('devtools::install("fedstats")'),
          br(),
          "then restart this window")
  })

  # ---- Joined-study banner ---------------------------------------
  output$joined_ui <- renderUI({
    cfg <- rv$config
    if (is.null(cfg)) return(NULL)
    fp <- if (.fedstats_ok && !is.null(cfg$coord_pk))
            fedstats::fed_fingerprint(cfg$coord_pk) else "?"
    div(class = "privacy",
        strong(paste0("Joined study: ", cfg$study)),
        br(),
        sprintf("Coordinator: %s   ·   key fingerprint: %s", cfg$coord, fp))
  })

  output$join_status_ui <- renderUI({
    if (is.null(rv$join_status)) return(NULL)
    div(class = "join-status", rv$join_status)
  })

  # ---- Join: parse + verify + pin + save -------------------------
  observeEvent(input$btn_join, {
    if (!.fedstats_ok) {
      showNotification("fedstats package not installed.", type = "error"); return()
    }
    raw <- trimws(input$invite)
    if (!nzchar(raw)) {
      showNotification("Paste an invite first.", type = "warning"); return()
    }

    # A short link (http://coord:8731/i/<sid>) instead of the full invite
    # text is also accepted — resolve it to the real invite first.
    if (grepl("^https?://", raw, ignore.case = TRUE)) {
      raw <- tryCatch({
        resp <- httr::GET(raw, httr::timeout(8))
        if (httr::status_code(resp) != 200)
          stop("That link didn't return an invite — it may have expired.")
        inv <- httr::content(resp, as = "parsed")$invite
        if (is.null(inv) || !nzchar(inv)) stop("That link didn't return an invite.")
        inv
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NA_character_
      })
      if (is.na(raw)) return()
    }

    pr <- fedstats::fed_invite_parse(raw)
    if (!isTRUE(pr$ok)) {
      showNotification(pr$reason, type = "error", duration = 10); return()
    }

    p  <- pr$payload
    fp <- fedstats::fed_fingerprint(pr$pk)
    key_changed <- !is.null(rv$config) && !is.null(rv$config$coord_pk) &&
                   !identical(rv$config$coord_pk, pr$pk)

    showModal(modalDialog(
      title = "Join this study?",
      p(strong("Intended site: "),
        if (nzchar(p$name)) p$name else "(no label set)",
        br(),
        tags$span(class = "note",
                  "This invite was issued for the site above. ",
                  "If that is not you, do not join — you were sent the wrong invite.")),
      p(strong("Study: "), p$study),
      p(strong("Coordinator: "), p$coord),
      p(strong("Key fingerprint: "), tags$code(fp)),
      p(class = "note",
        "If the coordinator told you a fingerprint out of band, check it matches."),
      if (key_changed)
        div(class = "warn-box",
            strong("WARNING: the coordinator's key has changed"),
            br(),
            "This invite is signed by a different key than the one you joined ",
            "with before. Only continue if you expected this."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("join_confirm", "Join", class = "btn-primary")),
      easyClose = TRUE))
  })

  observeEvent(input$join_confirm, {
    removeModal()
    pr <- fedstats::fed_invite_parse(trimws(input$invite))
    if (!isTRUE(pr$ok)) {
      showNotification(pr$reason, type = "error"); return()
    }
    p <- pr$payload

    # Reuse the site's existing keypair across re-joins; mint on first join.
    old <- rv$config
    if (!is.null(old) && !is.null(old$site_priv) && !is.null(old$site_pub)) {
      site_priv <- old$site_priv; site_pub <- old$site_pub
    } else {
      kp <- fedstats::fed_keypair(); site_priv <- kp$private; site_pub <- kp$public
    }

    cfg <- list(study = p$study, coord = p$coord, sid = p$sid, token = p$tok,
                coord_pk = pr$pk, site_priv = site_priv, site_pub = site_pub)
    .save_config(cfg)
    rv$config <- cfg

    updateTextAreaInput(session, "invite", value = "")
    rv$join_status <- "Joined. Select your data file (if needed) and click Start Server."
    showNotification(paste0("Joined study '", cfg$study,
                            "'. Start the server to register."),
                     type = "message", duration = 8)
  })

  # ---- Status badge ----------------------------------------------
  output$status_badge <- renderUI({
    lbl <- switch(rv$status,
      stopped  = "Stopped",
      starting = "Starting…",
      running  = "Running"
    )
    span(class = paste0("bdg bdg-", rv$status), lbl)
  })

  # ---- Address display (only shown while running) ----------------
  output$address_ui <- renderUI({
    if (rv$status == "stopped") return(NULL)
    port <- isolate(rv$port)
    if (nzchar(rv$ts_ip)) {
      div(class = "addr",
          div(class = "addr-lbl", "Your address (the coordinator reaches you here)"),
          div(class = "addr-url", sprintf("http://%s:%d", rv$ts_ip, port)))
    } else {
      div(class = "addr",
          div(class = "addr-lbl", "Local address (Tailscale not detected)"),
          div(class = "addr-url", sprintf("http://localhost:%d", port)),
          div(style = "font-size:.82em; color:#666; margin-top:4px;",
              "The coordinator must be on the same machine or local network."))
    }
  })

  # ---- File selector: the data/ folder dropdown and Browse are always
  # both available (not either/or) — picking a CSV from elsewhere isn't an
  # error case, it's just as normal a path as the quick-access folder.
  # Refresh re-scans data/ without restarting the app, e.g. after dropping
  # in a file for a different study you want to try next.
  observeEvent(input$btn_refresh_files, {
    csv_files_rv(.scan_csvs())
  })

  output$file_ui <- renderUI({
    files <- csv_files_rv()
    tagList(
      div(style = "display:flex; justify-content:space-between; align-items:baseline;",
          tags$span("Data file", style = "font-weight:600; font-size:.92rem; color:var(--ink-muted);"),
          actionLink("btn_refresh_files", "↻ Refresh", style = "font-size:.82rem;")),
      if (length(files) > 0) {
        selectInput("data_file", NULL, choices = setNames(files, basename(files)))
      } else {
        div(class = "note", style = "margin-top:2px; margin-bottom:8px;",
            "No files here yet — browse below to pick one, or drop CSVs in the data folder for quick access next time.")
      },
      div(style = "margin-top:6px;",
          fileInput("data_file_upload", "Or choose a file from anywhere",
                    accept = ".csv", buttonLabel = "Browse…",
                    placeholder = "No file selected"))
    )
  })

  # ---- Start / Stop button ---------------------------------------
  output$action_btn_ui <- renderUI({
    if (rv$status == "stopped") {
      actionButton("btn_start", "Start Server",
                   class = "btn btn-primary",
                   style = "min-width:140px; font-size:1em;")
    } else {
      actionButton("btn_stop", "Stop Server",
                   class = "btn btn-danger",
                   style = "min-width:140px; font-size:1em;")
    }
  })

  # ---- Start server ----------------------------------------------
  observeEvent(input$btn_start, {
    if (!.fedstats_ok) {
      showNotification("fedstats package not installed",
                       type = "error"); return()
    }
    if (is.null(rv$config)) {
      showNotification("Join a study first (paste your invite above).",
                       type = "warning"); return()
    }

    # Browse takes priority when used (an explicit override); otherwise
    # fall back to whatever's picked from the data/ folder dropdown.
    data_path <- if (!is.null(input$data_file_upload)) {
      input$data_file_upload$datapath
    } else if (!is.null(input$data_file) && nzchar(input$data_file)) {
      input$data_file
    } else {
      NA_character_
    }

    if (is.na(data_path) || !nzchar(data_path) || !file.exists(data_path)) {
      showNotification("Select a data file first.",
                       type = "error"); return()
    }

    port <- .find_free_port()
    if (is.na(port)) {
      showNotification("No free port found — close other programs and try again.",
                       type = "error"); return()
    }
    rv$port <- port
    token   <- rv$config$token

    env_vars                  <- Sys.getenv()
    env_vars["FED_DATA_FILE"] <- data_path
    env_vars["FED_PORT"]      <- as.character(port)
    env_vars["FED_TOKEN"]     <- token

    rv$log    <- character(0)
    rv$status <- "starting"
    rv$need_register <- TRUE  # joining via invite is now required to reach here

    rv$proc <- tryCatch(
      processx::process$new(
        "Rscript", args = .api_script,
        wd     = .app_root,
        env    = env_vars,
        stdout = "|",
        stderr = "|"
      ),
      error = function(e) {
        rv$status <- "stopped"
        showNotification(paste("Failed to start server:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
  })

  # ---- Stop server -----------------------------------------------
  observeEvent(input$btn_stop, {
    proc <- rv$proc
    if (!is.null(proc) && proc$is_alive()) {
      proc$kill()
      rv$log <- c(rv$log, "\n--- server stopped by user ---")
    }
    rv$status <- "stopped"
    rv$proc   <- NULL
    session$sendCustomMessage("scrollLog", list())
  })

  # ---- Log display -----------------------------------------------
  output$server_log <- renderText({
    if (!length(rv$log))
      return("Log will appear here once the server starts")
    paste(rv$log, collapse = "\n")
  })

  # ---- Kill subprocess on browser close --------------------------
  session$onSessionEnded(function() {
    proc <- isolate(rv$proc)
    if (!is.null(proc) && proc$is_alive()) proc$kill()
  })
}

shinyApp(ui, server)
