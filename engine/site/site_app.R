# engine/site/site_app.R
# =====================================================================
# Federated Statistics — Site Server GUI
#
# Launched by the Start Site launchers in Run/Mac, Run/Linux, Run/Windows.
# Runs the plumber API (api_server.R) as a background subprocess and
# streams its output into the browser log panel.
#
# Phase 2: paste a coordinator invite and click Join. The invite is
# verified (Ed25519 signature, expiry), the coordinator's key is pinned
# (TOFU), a per-site key and config are saved, and after the server
# starts the site registers itself back to the coordinator automatically.
# The old "type a shared token and Start" flow still works unchanged.
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
.csv_files <- .scan_csvs()

# ---- Tailscale IP ---------------------------------------------------
.ts_ip <- tryCatch({
  cmd <- if (.Platform$OS.type == "windows") "tailscale ip -4"
         else "tailscale ip -4 2>/dev/null"
  ip  <- trimws(system(cmd, intern = TRUE, ignore.stderr = TRUE))
  if (length(ip) && nzchar(ip[1])) ip[1] else ""
}, error = function(e) "")

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
.do_register <- function(cfg, port) {
  if (!.fedstats_ok) return("fedstats not installed — cannot register.")
  addr <- if (nzchar(.ts_ip)) sprintf("http://%s:%d", .ts_ip, port)
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
    /* ── Karolinska colour palette ─────────────────────────────── */
    body { font-family:'Helvetica Neue',Arial,sans-serif;
           max-width:860px; margin:0 auto; padding:24px; }
    h3   { margin-bottom:0; color:#6A0DAD; }
    .bdg { display:inline-block; padding:3px 12px; border-radius:10px;
           font-size:.82em; font-weight:bold; margin-left:10px; vertical-align:middle; }
    .bdg-stopped  { background:#fed7d7; color:#9b2c2c; }
    .bdg-starting { background:#fefcbf; color:#744210; }
    .bdg-running  { background:#e9d5ff; color:#4a0080; }
    .addr { background:#f3e8ff; border:1px solid #c084fc; border-radius:6px;
            padding:10px 16px; margin:12px 0; }
    .addr-lbl { font-size:.75em; font-weight:bold; color:#6A0DAD;
                text-transform:uppercase; letter-spacing:.05em; }
    .addr-url { font-family:monospace; font-size:1.18em; color:#4a0080;
                font-weight:bold; margin-top:3px; }
    .privacy  { background:#f0fff4; border-left:3px solid #38a169;
                padding:8px 12px; margin:12px 0; font-size:.83em; color:#276749; }
    .warn-box { background:#fff5f5; border-left:3px solid #fc8181;
                padding:8px 12px; margin:12px 0; font-size:.83em; color:#c53030; }
    .join-box { background:#f8f0fd; border:1px solid #e9d5ff; border-radius:6px;
                padding:12px 16px; margin:12px 0; }
    .join-status { font-size:.83em; color:#4a0080; margin-top:6px; }
    .sec-lbl  { font-weight:bold; font-size:.8em; color:#6A0DAD;
                text-transform:uppercase; letter-spacing:.05em; margin:18px 0 6px 0; }
    #invite { width:100%; height:90px; font-family:monospace; font-size:.74em;
              word-break:break-all; }
    #server_log { height:280px; overflow-y:auto; background:#1a202c; color:#e2e8f0;
                  font-family:monospace; font-size:.76em; border-radius:6px;
                  padding:10px 14px; white-space:pre-wrap; border:none; }
    hr { border-color:#e2e8f0; margin:16px 0; }
    .btn-primary,
    .btn-primary:active,
    .btn-primary.active { background-color: #6A0DAD !important;
                          border-color: #5a0a91 !important; }
    .btn-primary:hover,
    .btn-primary:focus  { background-color: #5a0a91 !important;
                          border-color: #4a0080 !important; }
  "))),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('scrollLog', function(x) {
      setTimeout(function() {
        var el = document.getElementById('server_log');
        if (el) el.scrollTop = el.scrollHeight;
      }, 60);
    });
  ")),

  tags$h3("Federated Site",
          uiOutput("status_badge", inline = TRUE)),

  uiOutput("warn_ui"),

  # ---- Join a study (invite flow) --------------------------------
  div(class = "sec-lbl", "Join a study"),
  uiOutput("joined_ui"),
  div(class = "join-box",
      tags$textarea(id = "invite", placeholder = "Paste your invite here (starts with FEDSTAT2.)"),
      div(style = "margin-top:8px;",
          actionButton("btn_join", "Join", class = "btn btn-primary btn-sm")),
      uiOutput("join_status_ui")),

  uiOutput("address_ui"),

  hr(),
  div(class = "sec-lbl", "Configuration"),

  fluidRow(
    column(5, uiOutput("file_ui")),
    column(3, numericInput("port", "Port", value = 8000, min = 1, max = 65535, step = 1)),
    column(4, passwordInput("token", "Token", placeholder = "optional"))
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

  rv <- reactiveValues(
    proc         = NULL,
    log          = character(0),
    status       = "stopped",   # "stopped" | "starting" | "running"
    config       = .load_config(),
    join_status  = NULL,        # text shown under the Join box
    need_register = FALSE       # register once the server reaches "running"
  )

  # Prefill the token field from a saved config on first load.
  observe({
    cfg <- isolate(rv$config)
    if (!is.null(cfg) && !is.null(cfg$token))
      updateTextInput(session, "token", value = cfg$token)
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
      if (isolate(rv$status) == "starting" &&
          any(grepl("Running plumber|Listening|listening on|port",
                    isolate(rv$log), ignore.case = TRUE))) {
        rv$status <- "running"
        # Auto-register once the server is up, if we joined via an invite.
        if (isolate(rv$need_register) && !is.null(isolate(rv$config))) {
          msg <- .do_register(isolate(rv$config), as.integer(isolate(input$port)))
          rv$join_status <- msg
          rv$log <- c(rv$log, paste0("\n--- ", msg, " ---"))
          rv$need_register <- FALSE
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

    updateTextInput(session, "token", value = cfg$token)
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
    port <- isolate(input$port)
    if (nzchar(.ts_ip)) {
      div(class = "addr",
          div(class = "addr-lbl", "Your address (the coordinator reaches you here)"),
          div(class = "addr-url", sprintf("http://%s:%d", .ts_ip, port)))
    } else {
      div(class = "addr",
          div(class = "addr-lbl", "Local address (Tailscale not detected)"),
          div(class = "addr-url", sprintf("http://localhost:%d", port)),
          div(style = "font-size:.82em; color:#666; margin-top:4px;",
              "The coordinator must be on the same machine or local network."))
    }
  })

  # ---- File selector (auto-detect or manual browse) --------------
  output$file_ui <- renderUI({
    if (length(.csv_files) > 0) {
      selectInput("data_file", "Data file",
                  choices = setNames(.csv_files, basename(.csv_files)))
    } else {
      tagList(
        div(style = "color:#c53030; font-size:.83em; margin-bottom:4px;",
            sprintf(
              "No CSV found. Put your file in this folder, then restart Start Site: %s",
              .data_dir
            )),
        fileInput("data_file_upload", "Data file (.csv)",
                  accept = ".csv", buttonLabel = "Browse…",
                  placeholder = "No file selected")
      )
    }
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

    data_path <- if (length(.csv_files) > 0) {
      input$data_file
    } else {
      req(input$data_file_upload)
      input$data_file_upload$datapath
    }

    if (is.null(data_path) || !nzchar(data_path) || !file.exists(data_path)) {
      showNotification("Data file not found",
                       type = "error"); return()
    }

    port  <- as.integer(input$port)
    token <- trimws(input$token)

    env_vars                  <- Sys.getenv()
    env_vars["FED_DATA_FILE"] <- data_path
    env_vars["FED_PORT"]      <- as.character(port)
    if (nzchar(token)) env_vars["FED_TOKEN"] <- token

    rv$log    <- character(0)
    rv$status <- "starting"
    # If we joined via an invite, register once the server is up.
    rv$need_register <- !is.null(rv$config) &&
                        identical(token, rv$config$token)

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
