# api_server.R
# =====================================================================
# Federated Statistics - Site HTTP API (run at each hospital)
#
# Usage:
#   Rscript api_server.R --data path/to/site_data.csv --port 8000
#
# Or set environment variables:
#   FED_DATA_FILE=path/to/data.csv
#   FED_PORT=8000
#   FED_TOKEN=mysecrettoken   (optional; coordinator must send same token)
#   FED_MIN_N=20              (refuse queries with fewer rows than this)
# =====================================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(fedstats)   # provides create_server()
})

# ------------------------------------------------------------------
# Configuration: CLI args override env vars
# ------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, env_var, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) && length(args) >= idx + 1) return(args[idx + 1])
  val <- Sys.getenv(env_var, unset = "")
  if (nzchar(val)) return(val)
  default
}

DATA_FILE <- get_arg("--data",  "FED_DATA_FILE", NULL)
PORT      <- as.integer(get_arg("--port",  "FED_PORT",  "8000"))
TOKEN     <- get_arg("--token", "FED_TOKEN", "")        # "" = no auth
MIN_N     <- as.integer(get_arg("--min-n", "FED_MIN_N", "20"))
# Privacy threshold for individual cells (per-group summaries, 2x2 cells,
# per-variable extremes). Separate from MIN_N (whole-site participation);
# 5 is the standard medical disclosure-control cell threshold.
MIN_CELL  <- as.integer(get_arg("--min-cell", "FED_MIN_CELL", "5"))
VALIDATE_QUANTILE_MIN_N <- as.integer(get_arg("--validate-quantile-min-n",
                                              "FED_VALIDATE_QUANTILE_MIN_N", "20"))
QUERY_LOG_FILE          <- get_arg("--query-log", "FED_QUERY_LOG_FILE",
                                   file.path("engine", "site", "site_query_audit.jsonl"))
QUERY_RATE_MAX          <- as.integer(get_arg("--query-rate-max", "FED_QUERY_RATE_MAX", "500"))
QUERY_RATE_WINDOW       <- as.numeric(get_arg("--query-rate-window", "FED_QUERY_RATE_WINDOW", "600"))
QUERY_DUP_WINDOW        <- as.numeric(get_arg("--query-dup-window", "FED_QUERY_DUP_WINDOW", "60"))
QUERY_SCOPE_MAX         <- as.integer(get_arg("--query-scope-max", "FED_QUERY_SCOPE_MAX", "50"))
QUERY_SCOPE_WINDOW      <- as.numeric(get_arg("--query-scope-window", "FED_QUERY_SCOPE_WINDOW", "3600"))

if (is.null(DATA_FILE) || !nzchar(DATA_FILE)) {
  stop("Provide --data <csv_path> or set FED_DATA_FILE.")
}
if (!file.exists(DATA_FILE)) {
  stop("Data file not found: ", DATA_FILE)
}

cat(sprintf("[api_server] Loading data: %s\n", DATA_FILE))
site_data <- read.csv(
  DATA_FILE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN", "NULL")
)

srv <- create_server(site_data, min_n = MIN_N, min_cell = MIN_CELL,
                     validate_quantile_min_n = VALIDATE_QUANTILE_MIN_N)
cat(sprintf("[api_server] Loaded %d rows. min_n=%d, min_cell=%d. Listening on port %d.\n",
            nrow(site_data), MIN_N, MIN_CELL, PORT))
cat(sprintf("[api_server] Query audit log: %s\n", QUERY_LOG_FILE))

# ------------------------------------------------------------------
# Helpers: input validation for the structured model_spec protocol
# ------------------------------------------------------------------

# Validate a single variable name received over the wire.
# Accepts standard R/CSV column names; rejects anything that could be
# interpreted as R code if interpolated into an expression.
.validate_varname <- function(name) {
  if (!is.character(name) || length(name) != 1L ||
      !grepl("^[A-Za-z][A-Za-z0-9_.]{0,63}$", name))
    stop(paste0("Invalid variable name: '", name, "'"))
  invisible(name)
}

# Validate a model_spec object and reconstruct a safe R formula.
# Every string that reaches as.formula() originates from validated
# variable names, never from raw network input.
.spec_to_formula <- function(spec, data) {
  if (is.null(spec))
    stop("Request must include a model_spec object.")
  outcome <- .validate_varname(spec$outcome)
  preds   <- vapply(unlist(spec$predictors, use.names = FALSE),
                    .validate_varname, character(1L))
  if (length(preds) == 0L)
    stop("model_spec: at least one predictor is required.")

  missing_vars <- setdiff(c(outcome, preds), names(data))
  if (length(missing_vars) > 0L)
    stop(paste0("Variable(s) not found in dataset: ",
                paste(missing_vars, collapse = ", ")))

  intercept <- !identical(spec$intercept, FALSE)
  rhs <- if (intercept) paste(preds, collapse = " + ")
         else paste0("0 + ", paste(preds, collapse = " + "))
  as.formula(paste(outcome, "~", rhs), env = baseenv())
}

.model_scope <- function(spec) {
  if (is.null(spec)) return(NULL)
  list(
    outcome = .validate_varname(spec$outcome),
    predictors = sort(vapply(unlist(spec$predictors, use.names = FALSE),
                             .validate_varname, character(1L))),
    intercept = !identical(spec$intercept, FALSE)
  )
}

# ------------------------------------------------------------------
# Helper: check bearer token if one is configured
# ------------------------------------------------------------------
check_token <- function(req) {
  if (!nzchar(TOKEN)) return(invisible(NULL))   # auth disabled
  auth <- req$HTTP_AUTHORIZATION
  if (is.null(auth) || !grepl("^Bearer ", auth)) {
    stop("Unauthorized: missing or malformed Authorization header.")
  }
  if (!fedstats::fed_ct_equal(sub("^Bearer ", "", auth), TOKEN)) {
    stop("Unauthorized: invalid token.")
  }
  invisible(NULL)
}

# Run before each handler's own tryCatch so an auth failure reports 401
# (distinct from a 400 validation error further into the same handler).
# Returns NULL if authorized, or the error payload to return early otherwise
# — callers do `auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)`.
check_auth_401 <- function(req, res) {
  tryCatch({ check_token(req); NULL },
          error = function(e) { res$status <- 401; list(error = conditionMessage(e)) })
}

# ------------------------------------------------------------------
# Query audit + anti-differencing guards
# ------------------------------------------------------------------
.query_state <- new.env(parent = emptyenv())
.query_state$rate <- list()
.query_state$scope <- list()

.sort_recursive <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x))]
    x <- lapply(x, .sort_recursive)
  }
  x
}

.canonical_json <- function(x) {
  as.character(jsonlite::toJSON(.sort_recursive(x), auto_unbox = TRUE, null = "null"))
}

.hash_text <- function(x, n = 24L) {
  substr(sodium::bin2hex(sodium::sha256(charToRaw(x))), 1L, n)
}

.audit_append <- function(actor, endpoint, decision, scope = NULL, reason = NULL) {
  dir.create(dirname(QUERY_LOG_FILE), recursive = TRUE, showWarnings = FALSE)
  rec <- list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    actor = actor,
    endpoint = endpoint,
    decision = decision,
    scope = scope,
    reason = reason
  )
  line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null")
  cat(line, "\n", file = QUERY_LOG_FILE, append = TRUE, sep = "")
}

.actor_id <- function(req) {
  auth <- req$HTTP_AUTHORIZATION
  if (!is.null(auth) && grepl("^Bearer ", auth)) {
    token <- sub("^Bearer ", "", auth)
    return(paste0("token_", .hash_text(token)))
  }
  ip <- if (!is.null(req$REMOTE_ADDR)) req$REMOTE_ADDR else "unknown"
  paste0("ip_", .hash_text(ip))
}

.scope_signature <- function(endpoint, scope) {
  .hash_text(.canonical_json(list(endpoint = endpoint, scope = scope)), n = 32L)
}

.query_guard <- function(req, res, endpoint, scope = NULL, sensitive = FALSE) {
  actor <- .actor_id(req)
  now <- as.numeric(Sys.time())

  hits <- .query_state$rate[[actor]]
  hits <- if (is.null(hits)) numeric(0) else hits[hits > now - QUERY_RATE_WINDOW]
  if (length(hits) >= QUERY_RATE_MAX) {
    res$status <- 429
    .query_state$rate[[actor]] <- hits
    .audit_append(actor, endpoint, "blocked_rate", scope,
                  sprintf(">%d requests in %.0f seconds", QUERY_RATE_MAX, QUERY_RATE_WINDOW))
    return(list(error = "Too many requests in a short time. Please wait before querying this site again."))
  }
  .query_state$rate[[actor]] <- c(hits, now)

  if (sensitive) {
    sig <- .scope_signature(endpoint, scope)
    hist <- .query_state$scope[[actor]]
    if (is.null(hist)) hist <- list()

    keep_recent <- vapply(hist, function(x) is.list(x) && !is.null(x$ts) &&
                            x$ts > now - QUERY_SCOPE_WINDOW, logical(1))
    hist <- hist[keep_recent]

    seen <- hist[[sig]]
    if (!is.null(seen) && (now - seen$ts) < QUERY_DUP_WINDOW) {
      res$status <- 429
      .query_state$scope[[actor]] <- hist
      .audit_append(actor, endpoint, "blocked_duplicate", scope,
                    sprintf("Repeated sensitive query within %.0f seconds", QUERY_DUP_WINDOW))
      return(list(error = "This same sensitive query was asked very recently. Reuse the existing result or wait a little before trying again."))
    }

    if (is.null(seen) && length(hist) >= QUERY_SCOPE_MAX) {
      res$status <- 429
      .query_state$scope[[actor]] <- hist
      .audit_append(actor, endpoint, "blocked_scope_budget", scope,
                    sprintf(">%d distinct sensitive queries in %.0f seconds",
                            QUERY_SCOPE_MAX, QUERY_SCOPE_WINDOW))
      return(list(error = "Too many distinct sensitive queries were requested from this site. Narrow the analysis plan or wait before probing more variables."))
    }

    hist[[sig]] <- list(ts = now, scope = scope, endpoint = endpoint)
    .query_state$scope[[actor]] <- hist
  }

  .audit_append(actor, endpoint, "allowed", scope)
  NULL
}

# ------------------------------------------------------------------
# Plumber API definition
# ------------------------------------------------------------------
#* @apiTitle Federated Statistics Site API
#* @apiDescription Exposes local aggregate statistics for federated GLM.

pr <- plumber::Plumber$new()

# jsonlite::toJSON()'s default `digits = 4` rounds to 4 *decimal places*,
# not significant figures — it silently zeroes any value under 5e-5 (e.g.
# small gradient/beta entries) and truncates larger ones. That corrupts the
# iterative Newton-Raphson exchange (beta/grad/hess) between coordinator
# and site. Use full double precision on every response.
pr$setSerializer(plumber::serializer_json(auto_unbox = TRUE, null = "null",
                                           na = "null", digits = 15))

# ---- /termnames ------------------------------------------------
pr$handle("POST", "/termnames", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    if (!is.null(body$formula))
      stop("formula field no longer accepted; send model_spec instead.")
    scope <- .model_scope(body$model_spec)
    gate <- .query_guard(req, res, "termnames", scope, sensitive = FALSE)
    if (!is.null(gate)) return(gate)
    formula <- .spec_to_formula(body$model_spec, site_data)
    list(termnames = as.list(srv$termnames(formula)))
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /grad_hess ------------------------------------------------
pr$handle("POST", "/grad_hess", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    if (!is.null(body$formula))
      stop("formula field no longer accepted; send model_spec instead.")
    family <- if (!is.null(body$family)) body$family else "binomial_logit"
    if (!identical(family, "binomial_logit"))
      stop(paste0("Unsupported family: '", family,
                  "'. Only binomial_logit is currently supported."))
    scope <- c(.model_scope(body$model_spec), list(family = family))
    gate <- .query_guard(req, res, "grad_hess", scope, sensitive = FALSE)
    if (!is.null(gate)) return(gate)
    formula    <- .spec_to_formula(body$model_spec, site_data)
    beta_vals  <- as.numeric(unlist(body$beta, use.names = FALSE))
    beta_names <- unlist(body$beta_names)
    beta       <- setNames(beta_vals, beta_names)

    r <- srv$grad_hess(formula, beta)
    
    # Serialise matrix as flat vector + dims
    list(
      n         = r$n,
      termnames = as.list(names(r$grad)),
      grad      = as.list(as.numeric(r$grad)),
      hess      = list(
        data    = as.list(as.numeric(r$hess)),
        nrow    = nrow(r$hess),
        ncol    = ncol(r$hess),
        rownames = as.list(rownames(r$hess)),
        colnames = as.list(colnames(r$hess))
      ),
      ll = r$ll
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /lm_suffstats ---------------------------------------------
pr$handle("POST", "/lm_suffstats", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    if (!is.null(body$formula))
      stop("formula field no longer accepted; send model_spec instead.")
    scope <- .model_scope(body$model_spec)
    gate <- .query_guard(req, res, "lm_suffstats", scope, sensitive = FALSE)
    if (!is.null(gate)) return(gate)
    formula <- .spec_to_formula(body$model_spec, site_data)

    r <- srv$lm_suffstats(formula)
    
    list(
      n         = r$n,
      termnames = as.list(r$termnames),
      XtX       = list(
        data    = as.list(as.numeric(r$XtX)),
        nrow    = nrow(r$XtX),
        ncol    = ncol(r$XtX),
        rownames = as.list(rownames(r$XtX)),
        colnames = as.list(colnames(r$XtX))
      ),
      Xty = as.list(as.numeric(r$Xty)),
      yTy = r$yTy
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /summary_numeric ------------------------------------------
pr$handle("POST", "/summary_numeric", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body    <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    varname <- .validate_varname(body$varname)
    if (!varname %in% names(site_data))
      stop(paste0("Variable not found in dataset: '", varname, "'"))
    gate <- .query_guard(req, res, "summary_numeric",
                         list(varname = varname), sensitive = TRUE)
    if (!is.null(gate)) return(gate)
    r <- srv$summary_numeric(varname)
    list(type = r$type, n = r$n, sum = r$sum, sumsq = r$sumsq)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /group_summaries ------------------------------------------
pr$handle("POST", "/group_summaries", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body     <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    varname  <- .validate_varname(body$varname)
    groupvar <- .validate_varname(body$groupvar)
    missing_vars <- setdiff(c(varname, groupvar), names(site_data))
    if (length(missing_vars) > 0L)
      stop(paste0("Variable(s) not found in dataset: ",
                  paste(missing_vars, collapse = ", ")))
    gate <- .query_guard(req, res, "group_summaries",
                         list(varname = varname, groupvar = groupvar),
                         sensitive = TRUE)
    if (!is.null(gate)) return(gate)
    r <- srv$group_summaries(varname, groupvar)

    # Convert each group's stats list to JSON-safe form. A group below the
    # privacy threshold comes back as {suppressed:true} with no numbers —
    # pass that through unchanged (do NOT synthesise n/sum/sumsq for it).
    stats_out <- lapply(r$stats, function(z) {
      if (isTRUE(z$suppressed)) list(suppressed = TRUE)
      else list(n = z$n, sum = z$sum, sumsq = z$sumsq)
    })

    list(type = r$type, stats = stats_out, min_cell = r$min_cell)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /counts_2x2 -----------------------------------------------
pr$handle("POST", "/counts_2x2", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    xvar <- .validate_varname(body$xvar)
    yvar <- .validate_varname(body$yvar)
    missing_vars <- setdiff(c(xvar, yvar), names(site_data))
    if (length(missing_vars) > 0L)
      stop(paste0("Variable(s) not found in dataset: ",
                  paste(missing_vars, collapse = ", ")))
    gate <- .query_guard(req, res, "counts_2x2",
                         list(vars = sort(c(xvar, yvar))), sensitive = TRUE)
    if (!is.null(gate)) return(gate)
    r <- srv$counts_2x2(xvar, yvar)
    # If any cell was below the threshold the whole table is withheld —
    # forward the marker (and only the safe total), never the cells.
    if (isTRUE(r$suppressed)) {
      list(type = r$type, suppressed = TRUE, n = r$n, min_cell = r$min_cell)
    } else {
      list(
        type = r$type,
        n00  = r$n00, n01 = r$n01,
        n10  = r$n10, n11 = r$n11,
        n    = r$n
      )
    }
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
})

# ---- /validate -------------------------------------------------
# Uses auto_unbox=TRUE so boolean/scalar fields arrive as plain JSON
# values (true, 6680) rather than single-element arrays ([true], [6680]).
# Plumber's default serializer wraps scalars in arrays, which breaks
# isTRUE() checks on the coordinator side after httr parses with
# simplifyVector=FALSE.
pr$handle("POST", "/validate", function(req, res) {
  auth <- check_auth_401(req, res); if (!is.null(auth)) return(auth)
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    if (!is.null(body$formula))
      stop("formula field no longer accepted; send model_spec instead.")
    vars_spec <- body$vars_spec
    formula   <- if (!is.null(body$model_spec))
                   .spec_to_formula(body$model_spec, site_data) else NULL
    min_n     <- if (!is.null(body$min_n)) as.integer(body$min_n) else 20L
    gate <- .query_guard(
      req, res, "validate",
      list(
        vars = sort(names(vars_spec)),
        model = .model_scope(body$model_spec)
      ),
      sensitive = TRUE
    )
    if (!is.null(gate)) return(gate)
    srv$validate_data(vars_spec, formula, min_n)
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
}, serializer = plumber::serializer_json(auto_unbox = TRUE, null = "null", na = "null", digits = 15))

# ---- /health (simple ping) -------------------------------------
pr$handle("GET", "/health", function(req, res) {
  tryCatch({
    check_token(req)
    list(status = "ok", rows = nrow(site_data))
  }, error = function(e) {
    res$status <- 401
    list(error = conditionMessage(e))
  })
})

# ------------------------------------------------------------------
# Start — bind to the Tailscale interface (the system's trust boundary).
# Uses the shared fed_bind_host() so the "no Tailscale -> loopback, never
# 0.0.0.0" rule lives in exactly one place. Without Tailscale the server is
# reachable only from this machine, so patient-derived aggregates are never
# exposed to the LAN/internet by a misconfiguration. FED_BIND_HOST overrides
# for operators who genuinely run on another private network.
# ------------------------------------------------------------------
bind <- fedstats::fed_bind_host()
if (isTRUE(bind$forced)) {
  cat(sprintf("[api_server] Binding to FED_BIND_HOST override: %s\n", bind$host))
} else if (isTRUE(bind$tailscale)) {
  cat(sprintf("[api_server] Binding to Tailscale interface: %s\n", bind$host))
} else {
  cat(paste0("[api_server] WARNING: Tailscale not detected. Binding to loopback ",
             "(127.0.0.1) — the coordinator on another machine cannot reach this ",
             "site until Tailscale is connected. (Set FED_BIND_HOST to override.)\n"))
}

pr$run(host = bind$host, port = PORT)
