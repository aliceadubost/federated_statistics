# Phase 1 — Manual Testing Checklist

Branch: `alice/phase1-security`

Run all tests with a local in-process setup (two synthetic CSV datasets) before
testing over a real Tailscale connection. Mark each item PASS / FAIL / SKIP
with a note.

---

## Setup

```r
# In R, from the project root — reinstall fedstats after the package changes
install.packages("fedstats", repos = NULL, type = "source")

# Generate two small synthetic datasets
set.seed(1)
n <- 200
d1 <- data.frame(
  age     = round(rnorm(n, 55, 12)),
  sex     = sample(c("M","F"), n, replace = TRUE),
  bmi     = round(rnorm(n, 27, 5), 1),
  outcome = rbinom(n, 1, 0.3),
  score   = rnorm(n, 100, 15)
)
d2 <- data.frame(
  age     = round(rnorm(n, 58, 10)),
  sex     = sample(c("M","F"), n, replace = TRUE),
  bmi     = round(rnorm(n, 28, 4), 1),
  outcome = rbinom(n, 1, 0.35),
  score   = rnorm(n, 102, 14)
)
write.csv(d1, "data/site1.csv", row.names = FALSE)
write.csv(d2, "data/site2.csv", row.names = FALSE)
```

---

## 1. Protocol change — structured model_spec (Commit 1)

### 1a. Happy path — all five analysis types via remote servers

Start two local API server processes (or use in-process servers for speed) and
confirm all federated functions return results:

```r
library(fedstats)
s1 <- create_server(d1, min_n = 5)
s2 <- create_server(d2, min_n = 5)
servers <- list(s1, s2)

# Numeric summary
r <- fed_numeric(servers, "age")
stopifnot(r$n == 400, abs(r$mean - mean(c(d1$age, d2$age))) < 0.01)

# Group summary / Welch t
r <- fed_welch_t(servers, "score", "sex", "M", "F")
stopifnot(is.numeric(r$p))

# Chi-square 2x2
r <- fed_chisq_2x2(servers, "outcome", "sex")  # sex needs to be 0/1
# (adapt variables as needed)

# Linear regression
r <- fed_lm(servers, score ~ age + bmi)
stopifnot(length(r$coefficients) == 3)
pooled <- lm(score ~ age + bmi, data = rbind(d1, d2))
stopifnot(max(abs(r$coefficients - coef(pooled))) < 1e-8)

# Logistic regression
r <- fed_logistic_newton(servers, outcome ~ age + bmi)
stopifnot(r$converged)
```

**Expected:** all pass. The pooled `lm` coefficients must match within 1e-8
(regression baseline from PLAN.md).

---

### 1b. Old formula field is rejected

Start the API server locally and send a raw formula string:

```bash
curl -s -X POST http://localhost:8000/termnames \
  -H "Content-Type: application/json" \
  -d '{"formula": "outcome ~ age"}'
```

**Expected:** HTTP 400, body contains `"formula field no longer accepted"`.

Repeat for `/lm_suffstats`, `/grad_hess`, `/validate`.

---

### 1c. Interaction formula raises user-facing error on the client

```r
library(fedstats)
s <- create_remote_server("http://localhost:8000")
tryCatch(
  fed_lm(list(s), outcome ~ age * bmi),
  error = function(e) cat(conditionMessage(e))
)
```

**Expected:** error message mentions "interaction terms" and "pre-computed
column", not a cryptic R traceback.

---

### 1d. In-formula transformation raises user-facing error

```r
tryCatch(
  fed_lm(list(s), outcome ~ age + I(bmi^2)),
  error = function(e) cat(conditionMessage(e))
)
```

**Expected:** error message mentions "in-formula transformations".

---

### 1e. Predictor not in dataset returns clear 400

```r
# Via remote server
tryCatch(
  fed_lm(list(s), outcome ~ age + nonexistent_column),
  error = function(e) cat(conditionMessage(e))
)
```

**Expected:** error message contains `"Variable(s) not found in dataset:
nonexistent_column"`, not a cryptic model.matrix error.

---

### 1f. Invalid variable name rejected (injection attempt)

```bash
curl -s -X POST http://localhost:8000/summary_numeric \
  -H "Content-Type: application/json" \
  -d '{"varname": "x; system(\"id\")"}'
```

**Expected:** HTTP 400, body contains `"Invalid variable name"`.

---

### 1g. No-intercept model round-trips correctly

```r
r <- fed_lm(servers, score ~ 0 + age + bmi)
pooled_noint <- lm(score ~ 0 + age + bmi, data = rbind(d1, d2))
stopifnot(max(abs(r$coefficients - coef(pooled_noint))) < 1e-8)
```

**Expected:** coefficients match pooled lm without intercept.

---

## 2. /health authentication (Commit 2)

### 2a. /health with correct token returns 200

```bash
# Start server with FED_TOKEN=testtoken
curl -s -o /dev/null -w "%{http_code}" \
  -H "Authorization: Bearer testtoken" \
  http://localhost:8000/health
```

**Expected:** `200`.

---

### 2b. /health without token returns 401

```bash
curl -s -o /dev/null -w "%{http_code}" http://localhost:8000/health
```

**Expected:** `401`.

---

### 2c. /health with wrong token returns 401

```bash
curl -s -o /dev/null -w "%{http_code}" \
  -H "Authorization: Bearer wrongtoken" \
  http://localhost:8000/health
```

**Expected:** `401`.

---

### 2d. /health with no token configured returns 200 (no-auth mode)

Start server without `FED_TOKEN`. Repeat 2b request.

**Expected:** `200` — unauthenticated ping works when no token is set.

---

### 2e. Coordinator Ping button still works

In the coordinator GUI, enter a site URL and click "Ping sites".

**Expected:** ping shows `[OK] n = X rows` as before (coordinator already
sends the token).

---

## 3. Tailscale interface binding (Commit 3)

### 3a. With Tailscale connected — server binds to 100.x.x.x

Start the site server with Tailscale running. Check the startup log.

**Expected:** `[api_server] Binding to Tailscale interface: 100.x.x.x`

Confirm the server is NOT reachable on `localhost` or the LAN IP:
```bash
curl -s http://127.0.0.1:8000/health   # Expected: connection refused
```

---

### 3b. Without Tailscale — server falls back to 0.0.0.0 with warning

Stop Tailscale, start the server.

**Expected:** log contains `WARNING: Tailscale not detected. Binding to
0.0.0.0`. Server starts and responds on localhost (for local testing).

---

## 4. Per-query min_n enforcement (Commit 4)

### 4a. Query below min_n threshold is refused

```r
# Create server with min_n=50, query with a filtered dataset smaller than that
small <- d1[1:30, ]
s <- create_server(small, min_n = 50)  # 30 rows < 50 → startup should fail
```

**Expected:** startup error: `"Site has fewer than 50 rows"` (existing check).

```r
# Now test per-query: server starts fine, but a formula with few complete cases
d_sparse <- d1
d_sparse$bmi[1:185] <- NA   # only 15 complete cases for bmi
s2 <- create_server(d_sparse, min_n = 20)
tryCatch(s2$lm_suffstats(score ~ age + bmi), error = function(e) cat(conditionMessage(e)))
```

**Expected:** error: `"Query covers only 15 complete case(s); minimum is 20"`.

---

### 4b. summary_numeric below min_n

```r
d_few <- d1[1:10, ]
s <- create_server(d_few, min_n = 20)
tryCatch(s$summary_numeric("age"), error = function(e) cat(conditionMessage(e)))
```

**Expected:** error mentioning 10 non-missing values, minimum 20.

---

### 4c. counts_2x2 below min_n

```r
tryCatch(s$counts_2x2("outcome", "sex"), error = function(e) cat(conditionMessage(e)))
```

**Expected:** same style error.

---

### 4d. Query above min_n succeeds

```r
s_ok <- create_server(d1, min_n = 20)   # d1 has 200 rows
r <- s_ok$summary_numeric("age")
stopifnot(r$n == 200)
```

**Expected:** succeeds.

---

## 5. README Privacy section (Commit 5)

Open `README.md` and verify:

- [ ] The section is titled "Privacy model" (not "Privacy guarantee")
- [ ] It explicitly states per-query `min_n` enforcement
- [ ] It explicitly mentions the narrow-query inference limitation
- [ ] It explicitly states the trusted-coordinator assumption
- [ ] It mentions WireGuard/Tailscale as the transport encryption layer
- [ ] No sentence implies stronger guarantees than the system actually provides

---

## 6. Regression baseline

Run the full analysis flow end-to-end using the coordinator GUI with two local
site servers and the `demo_linear_regression.R` template:

- [ ] Site server starts and shows correct address
- [ ] Coordinator can ping both sites
- [ ] Validate data passes
- [ ] Run analysis completes and shows results
- [ ] `fed_lm` coefficients match `lm()` on pooled data within 1e-8
- [ ] All five demo templates run without error

---

## Sign-off

| Item | Result | Notes |
|------|--------|-------|
| 1a Happy path — all five analysis types | | |
| 1b Old formula field rejected (400) | | |
| 1c Interaction formula → user-facing error | | |
| 1d Transformation formula → user-facing error | | |
| 1e Missing column → clear 400 | | |
| 1f Injection attempt → rejected (400) | | |
| 1g No-intercept round-trip | | |
| 2a /health with correct token → 200 | | |
| 2b /health no token → 401 | | |
| 2c /health wrong token → 401 | | |
| 2d /health no-auth mode → 200 | | |
| 2e Coordinator Ping still works | | |
| 3a Tailscale present → binds to 100.x.x.x | | |
| 3b No Tailscale → fallback + warning | | |
| 4a Formula with few complete cases → error | | |
| 4b summary_numeric below min_n → error | | |
| 4c counts_2x2 below min_n → error | | |
| 4d Query above min_n → succeeds | | |
| 5  README privacy section review | | |
| 6  Full regression baseline | | |
