# Phase 3 — Manual Testing Checklist

Branch: `alice/phase3-polish` (stacked on `alice/phase2-onboarding`)

Covers the seven `PLAN.md` charter items for Phase 3 — **Reliability &
Polish**: request timeout, friendly error messages, parallel/non-blocking
site pings, live reachability badges, reactive Tailscale IP on the site,
and launcher-script cleanup. This is explicitly *not* new cryptography or
privacy mechanics — see `PHASE1_TESTING.md` / `PHASE2_TESTING.md` for those.
Mark each item PASS / FAIL / SKIP.

Uses the same four real site CSVs as Phase 2 (`denmark.csv`, `finland.csv`,
`norway.csv`, `sweden.csv`) — put them anywhere on your machine and set
`csv_dir` below, or select them in a GUI picker. No paths are hardcoded in
this document.

---

## Setup

```r
# In R, from the project root — reinstall fedstats (fed_friendly_http_error
# added) and the coordinator's new deps (future, promises).
install.packages("fedstats", repos = NULL, type = "source")
Rscript engine/setup.R coordinator   # or: source("engine/setup.R"); ... role <- "coordinator"

csv_dir <- "<the folder where you placed the four CSVs>"
```

---

## 1. Request timeout (automated)

Confirms `.remote_post()` never hangs — a wrong port fails fast, and a
genuinely slow site (not just an unreachable one) is aborted at
`FED_REQUEST_TIMEOUT_S` instead of hanging forever.

```r
library(fedstats); library(processx); library(httr)
host <- fed_bind_host()$host; tok <- fed_token()
csv  <- file.path(csv_dir, "denmark.csv")
env  <- Sys.getenv(); env["FED_DATA_FILE"] <- csv
env["FED_PORT"] <- "8140"; env["FED_TOKEN"] <- tok
p <- process$new("Rscript", "engine/site/api_server.R", env = env, wd = getwd(),
                 stdout = "|", stderr = "|"); Sys.sleep(3)
url <- sprintf("http://%s:8140", host)

# Normal call still works (no regression from adding a timeout).
stopifnot(create_remote_server(url, tok)$summary_numeric("age")$n == 4169)

# A short timeout against a routable-but-unresponsive address (RFC 5737
# TEST-NET-1 — guaranteed not to answer) aborts at the configured bound,
# not indefinitely.
Sys.setenv(FED_REQUEST_TIMEOUT_S = "2")
t0 <- Sys.time()
err <- tryCatch({ create_remote_server("http://192.0.2.1:8000", tok)$summary_numeric("age"); NULL },
                error = function(e) conditionMessage(e))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
stopifnot(elapsed < 10, identical(err, "Site not responding — it may be offline."))
Sys.unsetenv("FED_REQUEST_TIMEOUT_S")

p$kill(); cat(sprintf("Section 1 OK — timeout enforced in %.1fs\n", elapsed))
```

**Expected:** prints `Section 1 OK`, elapsed time close to the configured
2s bound (not the old unbounded hang).

---

## 2. Friendly error messages (automated)

Confirms the three charter-named cases, and that an unrelated error (e.g. an
unknown variable, still a 400) keeps its specific message rather than being
replaced by something generic.

```r
library(fedstats); library(processx)
host <- fed_bind_host()$host; tok <- fed_token()
csv  <- file.path(csv_dir, "denmark.csv")
env  <- Sys.getenv(); env["FED_DATA_FILE"] <- csv
env["FED_PORT"] <- "8141"; env["FED_TOKEN"] <- tok
p <- process$new("Rscript", "engine/site/api_server.R", env = env, wd = getwd(),
                 stdout = "|", stderr = "|"); Sys.sleep(3)
url <- sprintf("http://%s:8141", host)

# Wrong token -> 401 -> friendly message (every protected endpoint, not
# just /health).
err <- tryCatch({ create_remote_server(url, "WRONG")$summary_numeric("age"); NULL },
                error = function(e) conditionMessage(e))
stopifnot(identical(err, "Authentication failed — check your site's token."))

# Closed port -> friendly message.
dead <- sprintf("http://%s:8142", host)
err <- tryCatch({ create_remote_server(dead, tok)$summary_numeric("age"); NULL },
                error = function(e) conditionMessage(e))
stopifnot(identical(err, "Site unreachable — is the server running?"))

# Unknown variable (400, NOT auth-related) -> specific message preserved.
err <- tryCatch({ create_remote_server(url, tok)$summary_numeric("not_a_real_column"); NULL },
                error = function(e) conditionMessage(e))
stopifnot(grepl("not_a_real_column", err), !identical(err, "Authentication failed — check your site's token."))

p$kill(); cat("Section 2 OK — 401 / connection-refused / non-auth messages all correct\n")
```

**Expected:** prints `Section 2 OK`.

---

## 3. Parallel, non-blocking ping (automated)

Confirms N sites are pinged concurrently (wall-clock ≈ max latency, not
sum), using throwaway slow test servers — no product code involved besides
`ping_one()` / `ping_sites_async()` from `coordinator_app.R`.

```r
library(fedstats); library(processx); library(promises); library(future)
future::plan(future::multisession)

# Load ping_one/ping_sites_async without launching the full Shiny app.
lines  <- readLines("engine/coordinator/coordinator_app.R")
cutoff <- grep("^ui <- fluidPage", lines)[1]
env2   <- new.env()
setwd("engine/coordinator")
eval(parse(text = paste(lines[1:(cutoff - 1)], collapse = "\n")), envir = env2)
setwd("../..")
if (!is.null(env2$registrar_proc) && env2$registrar_proc$is_alive()) env2$registrar_proc$kill()
file.remove(Filter(file.exists, c("engine/coordinator/registered_sites.json",
                                  "engine/coordinator/coordinator_key.json")))

# A throwaway plumber app whose /health sleeps before answering.
writeLines(c(
  'suppressPackageStartupMessages(library(plumber))',
  'args <- commandArgs(trailingOnly = TRUE)',
  'PORT <- as.integer(args[1]); DELAY <- as.numeric(args[2])',
  'pr <- plumber::Plumber$new()',
  'pr$handle("GET", "/health", function(req, res) { Sys.sleep(DELAY); list(status="ok", rows=100) })',
  'pr$run(host = "127.0.0.1", port = PORT)'
), "slow_health_server.R")

delay <- 2; ports <- c(9101, 9102, 9103)
procs <- lapply(ports, function(port)
  process$new("Rscript", c("slow_health_server.R", port, delay), stdout = "|", stderr = "|"))
Sys.sleep(2)
sites <- lapply(ports, function(port)
  list(url = sprintf("http://127.0.0.1:%d", port), token = "", name = paste0("Site", port)))

t0 <- Sys.time()
serial <- vapply(seq_along(sites), function(i)
  env2$ping_one(sites[[i]]$url, sites[[i]]$token, i, sites[[i]]$name), character(1))
serial_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

t0 <- Sys.time()
resolved <- NULL
env2$ping_sites_async(sites) %...>% (function(x) resolved <<- x)
while (is.null(resolved)) { later::run_now(); Sys.sleep(0.05) }
parallel_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat(sprintf("Serial: %.1fs   Parallel: %.1fs   (%d sites x %ds)\n",
            serial_s, parallel_s, length(sites), delay))
stopifnot(parallel_s < serial_s * 0.7, all(grepl("\\[OK\\]", resolved)))

for (pr in procs) pr$kill(); file.remove("slow_health_server.R")
cat("Section 3 OK — parallel ping meaningfully faster than serial\n")
```

**Expected:** prints `Section 3 OK`; parallel time noticeably below serial
(observed on this machine: ~6.6s serial vs ~3.9s parallel for 3×2s sites —
parallel is bounded by max-latency plus one-time worker startup, not
sum-of-latency).

---

## 4. Live status badges (automated)

Pure-function test of `ping_badge_state()` — no Shiny session needed.

```r
lines  <- readLines("engine/coordinator/coordinator_app.R")
cutoff <- grep("^ui <- fluidPage", lines)[1]
env2   <- new.env()
setwd("engine/coordinator")
eval(parse(text = paste(lines[1:(cutoff - 1)], collapse = "\n")), envir = env2)
setwd("../..")
if (!is.null(env2$registrar_proc) && env2$registrar_proc$is_alive()) env2$registrar_proc$kill()
file.remove(Filter(file.exists, c("engine/coordinator/registered_sites.json",
                                  "engine/coordinator/coordinator_key.json")))

now <- Sys.time()
stopifnot(is.null(env2$ping_badge_state(NULL)))
stopifnot(identical(env2$ping_badge_state(list(ok = FALSE, checked_at = now)), "offline"))
stopifnot(identical(env2$ping_badge_state(list(ok = TRUE, checked_at = now), now = now), "connected"))
stopifnot(identical(env2$ping_badge_state(
  list(ok = TRUE, checked_at = now - env2$PING_STALE_S - 1), now = now), "stale"))
stopifnot(identical(env2$ping_badge_state(
  list(ok = TRUE, checked_at = now - env2$PING_STALE_S + 1), now = now), "connected"))

cat("Section 4 OK — none/offline/connected/stale all correct, including the boundary\n")
```

**Expected:** prints `Section 4 OK`.

---

## 5. Reactive Tailscale IP (manual)

Toggling Tailscale connectivity disrupts a live tailnet other people may be
relying on, so this is manual, ideally on a machine not mid-analysis.

- [ ] **5a** With Tailscale **disconnected**, start the site app (`Start
  Site`). The address panel shows "Local address (Tailscale not detected)".
- [ ] **5b** Without restarting the app, connect Tailscale. Within ~3
  seconds, the address panel updates to show the real `100.x.x.x` address —
  no app restart needed.
- [ ] **5c** Start the server (Start Server) while connected; confirm the
  registration address sent to the coordinator uses the live IP, not
  `localhost`.

---

## 6. `network instructions.txt` cleanup (review only)

- [ ] **6a** The Tailscale account-setup section (§1) is unchanged.
- [ ] **6b** The old CLI workflow (`server.R`/`api_server.R`/`run.R`,
  `FED_MODE=remote`) is gone, replaced by a short accurate pointer to the
  current GUI flow.

---

## 7. Coordinator launcher Tailscale check (manual — Mac/Linux)

Windows launchers were already consistent between roles; no change there.

- [ ] **7a** On Mac or Linux, with Tailscale **not** connected, double-click
  `Start Coordinator`. Step 3 shows the warn-and-continue message — **no
  sudo password prompt**.
- [ ] **7b** The wording and behaviour matches `Start Site` on the same OS
  (both just check `tailscale ip -4` and offer to continue anyway).

---

## Regression — Phase 1/2 still intact

```r
# Section 10 equivalent (Phase 2): correct/wrong token still behave right
# (message text changed by Phase 3 item 4 — see the note in
# PHASE2_TESTING.md Section 10).
# Section 12 equivalent (Phase 2 FINAL): rerun the five-analysis pooled
# cross-check against all four real sites — must still match within 1e-6.
```

Rerun `PHASE2_TESTING.md` Sections 10 and 12 verbatim (both already updated
with a Phase 3 note where behaviour intentionally changed). Both must still
report OK.

---

## Sign-off

| Item | Result | Notes |
|------|--------|-------|
| 1 Request timeout (automated) | PASS | aborts at configured bound, no hang |
| 2 Friendly error messages (automated) | PASS | 401 / unreachable / non-auth all correct |
| 3 Parallel non-blocking ping (automated) | PASS | ~6.6s serial vs ~3.9s parallel (3×2s sites) |
| 4 Live status badges (automated) | PASS | none/offline/connected/stale + boundary |
| 5 Reactive Tailscale IP (manual) | | |
| 6 network instructions.txt cleanup (review) | PASS | reviewed in this session |
| 7 Coordinator launcher Tailscale check (manual) | | |
| Regression — Phase 2 §10 (auth) | PASS | see PHASE2_TESTING.md note |
| Regression — Phase 2 §12 (FINAL, 5 analyses) | PASS | unchanged, still matches pooled baseline |
