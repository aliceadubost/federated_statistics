# Phase 2 — Manual Testing Checklist

Branch: `alice/phase2-onboarding` (stacked on `alice/phase1-security`)

Covers the invite-bundle onboarding flow: signed invites, the coordinator
registrar, per-site tokens, the site Join flow, and — critically — that
**existing shared-token sites keep working without reconfiguration**. The final
section is a **cross-phase regression**: the same five federated analyses from
Phase 1, run against sites onboarded the new way, must produce the same numbers.

Design reference: `DESIGN_invite_bundle.md`. Mark each item PASS / FAIL / SKIP.

---

## Test data — four real sites

This phase is tested with four real clinical CSVs, one site per file:

| Site (CSV) | Rows | Onboarding in this plan |
|------------|------|-------------------------|
| `denmark.csv` | 4,169 | new invite flow (also the UX-timing test) |
| `finland.csv` | 6,680 | new invite flow |
| `norway.csv`  | 2,557 | new invite flow |
| `sweden.csv`  | 3,219 | **legacy** shared-token path (backward-compat) |
| **Total** | **16,625** | mixed session: 3 invited + 1 legacy |

Shared column schema (all four files):
`clinic`, `diag_grp` (radiculopathy / myelopathy), `diag_myelo` (0/1), `age`,
`sexM` (0/1), `bmi` (NAs), `nrs_arm_preop` (0–10, NAs), `nrs_arm_12m` (0–10, NAs),
`mcid_arm_12m` (0/1, NAs), `country_site`.

Put the four CSVs anywhere on your machine and **select each one in the site
GUI's data-file picker** — there is no fixed location. Where an automated
snippet needs a file, it uses `file.choose()` (pick the file when prompted) or a
`csv_dir` variable you set to the folder holding the four files. **No paths are
hardcoded in this document.**

A real run needs Tailscale connected (the registrar and site servers bind to the
Tailscale interface). Two or more machines on the same tailnet are ideal; one
machine with several site servers on different ports + browser windows works for
a functional pass.

---

## Setup

```r
# In R, from the project root — reinstall fedstats after the package changes.
install.packages("fedstats", repos = NULL, type = "source")
# (or: R CMD INSTALL fedstats)
```

---

## 1. Invite format & crypto (automated)

```r
library(fedstats)
kp <- fed_keypair()
inv <- fed_invite_create("SweSpine", "host.ts.net:8731", fed_sid(),
                         fed_token(), kp$private, name = "Karolinska", ttl_days = 7)
pr  <- fed_invite_parse(inv)
stopifnot(pr$ok, identical(pr$pk, kp$public), pr$payload$study == "SweSpine")

# Tampering and truncation are rejected
bad <- paste0(substr(inv, 1, nchar(inv) - 3), "zzz")
stopifnot(!fed_invite_parse(bad)$ok)
stopifnot(!fed_invite_parse(substr(inv, 1, 40))$ok)

# Expiry is enforced
exp_inv <- fed_invite_create("S", "h:8731", fed_sid(), fed_token(),
                             kp$private, ttl_days = 0)
stopifnot(isTRUE(fed_invite_parse(exp_inv, now = as.integer(Sys.time()) + 10)$expired))
cat("Section 1 OK\n")
```

**Expected:** prints `Section 1 OK`. Invite length is ~440–490 chars,
prefix `FEDSTAT2.`.

---

## 2. Registry state machine (automated)

```r
source("engine/coordinator/registry.R")   # from project root
now <- as.integer(Sys.time())
sid <- "s_test01"; tok <- "tok_test"; sk <- fed_keypair(); addr <- "http://100.1.1.1:8000"

reg <- reg_add_invite(reg_empty(), sid, "Site","Study", tok, now + 86400L)
stopifnot(reg$sites[[sid]]$invite_state == "issued")

# wrong token / unknown sid
stopifnot(reg_register(reg, sid, "BAD", addr, sk$public)$status == 401)
stopifnot(reg_register(reg, "nope", tok, addr, sk$public)$status == 404)

# fresh register -> consumed, identity recorded
r <- reg_register(reg, sid, tok, addr, sk$public); reg <- r$reg
stopifnot(r$status == 200, reg$sites[[sid]]$invite_state == "consumed",
          identical(reg$sites[[sid]]$site_pk, sk$public))

# same identity retry -> heartbeat; different address -> 409 pending
stopifnot(reg_register(reg, tok = tok, sid = sid, site_addr = addr, site_pk = sk$public)$status == 200)
stopifnot(reg_register(reg, tok = tok, sid = sid, site_addr = "http://100.9.9.9:8000",
                       site_pk = sk$public)$status == 409)

# revoke & remove -> gone, token on revocation list
reg <- reg_revoke_remove(reg, sid)
stopifnot(is.null(reg$sites[[sid]]), tok %in% reg$revoked)
cat("Section 2 OK\n")
```

**Expected:** prints `Section 2 OK`. The full transition set
(issued → in_use → consumed, retry vs collision, expiry, revoke) is exercised.

---

## 3. Registrar endpoint (automated, live)

Start the registrar against a temp registry, then drive it with `httr`:

```r
library(fedstats); library(httr); library(jsonlite); library(processx)
host <- fed_bind_host()$host
regfile <- file.path(tempdir(), "reg.json"); unlink(regfile)
sid <- fed_sid(); tok <- fed_token()
source("engine/coordinator/registry.R")
reg_save(reg_add_invite(reg_empty(), sid, "S","Study", tok, as.integer(Sys.time())+86400L), regfile)

env <- Sys.getenv(); env["FED_REGISTRY_FILE"] <- regfile; env["FED_REGISTRAR_PORT"] <- "8731"
p <- process$new("Rscript", "engine/coordinator/registrar.R", env = env, wd = getwd(),
                 stdout = "|", stderr = "|")
Sys.sleep(3)

reg_post <- function(token, sid, addr, sk, bad = FALSE) {
  ts <- as.integer(Sys.time()); m <- fed_register_message(sid, addr, sk$public, ts)
  sig <- if (bad) fed_sign("x", sk$private) else fed_sign(m, sk$private)
  POST(sprintf("http://%s:8731/register", host),
       add_headers(Authorization = paste("Bearer", token), `Content-Type`="application/json"),
       body = toJSON(list(sid=sid, site_addr=addr, site_pk=sk$public, ts=ts, sig=sig),
                     auto_unbox=TRUE), encode = "raw", timeout(5))
}
sk <- fed_keypair(); addr <- "http://100.5.5.5:8000"
stopifnot(status_code(reg_post(tok, sid, addr, sk)) == 200)            # fresh
stopifnot(status_code(reg_post(tok, sid, addr, sk)) == 200)            # heartbeat
stopifnot(status_code(reg_post("WRONG", sid, addr, sk)) == 401)        # bad token
stopifnot(status_code(reg_post(tok, sid, addr, sk, bad = TRUE)) == 401)# bad signature
codes <- sapply(1:25, function(i) status_code(reg_post("X", sid, addr, sk)))
stopifnot(any(codes == 429))                                          # rate limit
p$kill(); cat("Section 3 OK\n")
```

**Expected:** prints `Section 3 OK`. Confirms Bearer-token + signature auth,
heartbeat, and rate limiting (429), all on the Tailscale-bound listener.

---

## 4. Coordinator GUI — invites & registry (manual)

Launch **Start Coordinator**. Then:

- [ ] **4a** The sidebar shows `Registrar: <host>:8731` and a key fingerprint.
  (If it shows "Registrar not running", check the launcher console.)
- [ ] **4b** Click **Invite a site**, enter study + site name (e.g. "Denmark"),
  **Create invite**. A `FEDSTAT2.` string appears with a working **Copy** button.
- [ ] **4c** The new site appears in the table with status **Invited**.
- [ ] **4d** `git status` shows `registered_sites.json` and `coordinator_key.json`
  ignored (never committed).
- [ ] **4e** On a Unix host, the launcher console prints no world-readable warning
  (the registry and key files are `600`).

---

## 5. Site Join flow (manual) — using `denmark.csv`

Launch **Start Site** (second machine, or a second browser). Then:

- [ ] **5a** Paste the Denmark invite, click **Join** → a confirm dialog shows the
  study name and the coordinator key fingerprint **matching 4a**.
- [ ] **5b** Confirm → "Joined study …" banner appears; the Token field is filled.
- [ ] **5c** Select **denmark.csv** in the data-file picker, click **Start Server**
  → status reaches **Running** and the log shows `Registered with coordinator.`
- [ ] **5d** Back on the coordinator, Denmark flips to **Registered** with its
  address — without anyone typing the address.
- [ ] **5e** A `site_config.json` now exists (gitignored). Stop and restart the
  Denmark server → it re-registers automatically (no re-paste).

---

## 6. UX timing — onboard one site in under a minute (Denmark)

Goal: a non-technical operator onboards a site end-to-end in **under 60 seconds**,
**without looking anything up** in this document or elsewhere.

Run it as a stopwatch test. **Start the clock** when the coordinator clicks
**Invite a site**; **stop the clock** when Denmark shows **Registered** on the
coordinator. (Assumes both apps are already open and Tailscale is up — this times
the onboarding interaction, not app startup or R installation.)

Steps, in order, no pausing to read:
1. Coordinator: **Invite a site** → type "Denmark" → **Create invite** → **Copy**.
2. Send the invite to the site operator (paste into chat/email).
3. Site: paste into the invite box → **Join** → confirm.
4. Site: pick **denmark.csv** → **Start Server**.
5. Coordinator: Denmark appears as **Registered**.

- [ ] **6a** Total elapsed time: ______ seconds  (target < 60).
- [ ] **6b** No need to consult documentation or type any URL/token by hand.
- [ ] **6c** If it ran long, note where the time went (copy/paste friction,
  file picker, waiting for "Running"): __________________________________

---

## 7. Onboard the rest — mixed new + legacy session (REQUIRED)

> **Later removed:** after live multi-machine testing on `alice/phase3-polish`
> confirmed the invite flow works end-to-end, the legacy shared-token path
> (§7.2 below, the site's Token field, and the coordinator's **Add manually**
> button) was removed entirely in favour of invite-only onboarding. §7.2, the
> `Manual` badge, and the legacy references in Section 12 are kept below as a
> historical record of what Phase 2 originally supported — they no longer
> apply to the current app. §7.1 (the invite flow) is unaffected.

Bring all four sites online in one coordinator session: three via invites, one
the **old way**. The coordinator must handle the mix.

**7.1 Invite flow — Finland and Norway**

- [ ] **7a** Repeat Section 5 for **finland.csv** (invite labelled "Finland").
- [ ] **7b** Repeat Section 5 for **norway.csv** (invite labelled "Norway").
- [ ] **7c** Coordinator table shows Denmark, Finland, Norway all **Registered**.

**7.2 Legacy shared-token path — Sweden (no invite, no reconfiguration)**

This is the backward-compatibility case: a site set up the Phase 1 way must work
unchanged. Start the Sweden site **without** pasting an invite.

- [ ] **7d** On the Sweden site, leave the invite box **empty**. Type a shared
  token (e.g. `swespine2026`) in the **Token** field, select **sweden.csv**,
  **Start Server**. (Equivalent CLI form, also valid:
  `FED_DATA_FILE=…/sweden.csv FED_TOKEN=swespine2026 Rscript engine/site/api_server.R`.)
- [ ] **7e** The Sweden server starts and shows its address, exactly as in Phase 1
  (no invite, no `site_config.json`, no key material created for this site).
- [ ] **7f** On the coordinator, click **Add manually**, enter the Sweden URL and
  the same token, **Add**. Sweden appears with a **Manual** badge.
- [ ] **7g** Coordinator now lists 4 sites: 3 **Registered** + 1 **Manual**.

**Expected:** the legacy workflow is untouched, and invited and legacy sites
coexist in the same session. The combined set is used by the regression in
Section 12.

---

## 8. End-to-end onboarding smoke test (GUI)

> **Note:** `demo_descriptives.R`'s `VARS_SPEC` originally referenced
> `smoker`, `eq5d_index_preop`, `ndi_index_preop` — columns that don't exist
> in the four real site CSVs (`denmark/finland/norway/sweden.csv`, schema in
> the table above). Loading it as-written would fail **8c** with "variable
> not found" errors. Trimmed `VARS_SPEC` (and the downstream summary/table
> code) down to the columns that actually exist: `age`, `bmi`, `sexM`,
> `nrs_arm_preop`, `diag_grp`. Verified standalone against all four CSVs:
> `Validation: PASS` (3 non-blocking warnings — a few patients aged 9–17
> fall outside the `age` min=18 sanity range) and the Table 1 output
> populates correctly.

With all four sites online (Section 7):

- [ ] **8a** Load `analysis/templates/demo_descriptives.R` on the coordinator.
- [ ] **8b** **Ping sites** → shows `[OK] n = … rows` for all four (each via its
  own per-site token; Sweden via its shared token — none typed per-query).
- [ ] **8c** **Validate data** passes across all four.
- [ ] **8d** **Run analysis** completes and shows result tabs.

---

## 9. Failure cases & security behaviours (scenario-based)

Realistic mistakes with these four sites. Each maps to a mechanism already
unit-tested in Sections 2–3.

- [ ] **9a Wrong invite to the wrong operator.** Create the Finland invite with
  site label "Finland". You email it to the **Norway** operator by mistake; they
  paste it and click **Join**.
  **Expected:** the Join confirm dialog leads with **"Intended site: Finland"**
  and the note "If that is not you, do not join — you were sent the wrong
  invite." The Norway operator sees this and **cancels** rather than registering
  Norway's data under Finland's identity.
  Verify both halves: (i) the label shown matches the label the coordinator typed
  when creating the invite; (ii) an invite created with no label shows
  "(no label set)" instead of a blank line.
  **Residual note:** the invite is still cryptographically valid, so the dialog
  cannot *prevent* a determined operator from proceeding — the label is a
  visible safeguard the operator must heed, plus the coordinator can sanity-check
  the address that registers.
- [ ] **9b Expired invite.** The Denmark operator was away; the invite you sent is
  older than `FED_INVITE_TTL_DAYS`. (To force this quickly, set
  `FED_INVITE_TTL_DAYS=0` before creating a fresh invite, or wait past `exp`.)
  Pasting it and clicking Join shows **"This invite has expired."** Re-issue a new
  invite and confirm Join then works.
- [ ] **9c Revoked invite.** Norway withdraws from the study. On the coordinator,
  click **Revoke** on Norway. Norway's server then attempts to re-register
  (restart it) → the registrar rejects it; Norway does not reappear in the table.
  Confirms revocation is enforced, not merely hidden.
- [ ] **9d Retry after a network drop.** During Finland's Join, drop the network
  briefly (disable Wi-Fi for a few seconds) while the server is starting, then
  restore it. Finland retries from the **same address + same key** → registration
  completes (status **Registered**); no coordinator action and no re-paste needed.
- [ ] **9e Leaked / forwarded invite collision.** Denmark has two computers; the
  invite gets pasted on a **second** machine and Start Server is run there too.
  The second host registers with the same invite but a **different address/key**
  → coordinator shows **Needs approval** (not silent acceptance). **Approve**
  adopts the new host; leaving it unapproved keeps the original.
- [ ] **9f TOFU key change.** Re-join a site with an invite signed by a *different*
  coordinator key → the confirm dialog shows the
  "coordinator's key has changed" warning.
- [ ] **9g Optional fingerprint match.** The coordinator's fingerprint (4a) matches
  the one shown to the site (5a). Offered for out-of-band verification, never
  forced.

---

## 10. Backward-compatibility — unchanged server accepts a per-site token (automated)

Confirms `api_server.R` authenticates a per-site token and rejects a wrong
one. Run from the project root with Tailscale up; **pick one of the four CSVs
when prompted**:

```r
library(fedstats); library(processx); library(httr)
host <- fed_bind_host()$host; tok <- fed_token()
csv  <- file.choose()                     # select e.g. denmark.csv
env <- Sys.getenv(); env["FED_DATA_FILE"] <- csv
env["FED_PORT"] <- "8123"; env["FED_TOKEN"] <- tok
p <- process$new("Rscript", "engine/site/api_server.R", env = env, wd = getwd(),
                 stdout = "|", stderr = "|"); Sys.sleep(3)
url <- sprintf("http://%s:8123", host)
n_total <- create_remote_server(url, tok)$summary_numeric("age")$n
stopifnot(n_total > 0)                     # correct token works
stopifnot(tryCatch({ create_remote_server(url, "WRONG")$summary_numeric("age"); FALSE },
                   error = function(e) identical(conditionMessage(e),
                     "Authentication failed — check your site's token.")))
p$kill(); cat("Section 10 OK — server auth unchanged\n")
```

> **Phase 3 update:** a wrong token used to surface as a raw
> `Remote call failed [400] ... Unauthorized: invalid token.` (matched loosely
> via `grepl("401|nauthor", ...)`). Phase 3 (1) makes every protected endpoint
> report auth failures as **HTTP 401** specifically (previously only `/health`
> did — the others conflated auth failures with ordinary 400 validation
> errors), and (2) translates that into the friendly message asserted above.
> Other error types (e.g. `min_n` violations, unknown variables) are
> unaffected — they still return 400 with their original specific message.

---

## 11. Phase 1 mechanisms intact (manual)

- [ ] **11a** Each site server still binds to the Tailscale interface
  (`Binding to Tailscale interface: 100.x.x.x` in the log); falls back to
  `0.0.0.0` with a warning when Tailscale is absent.
- [ ] **11b** Per-query `min_n` enforcement still refuses small queries (e.g. a
  filter leaving < 20 complete cases returns a clear error).
- [ ] **11c** No change to the statistical wire protocol or `api_server.R`
  statistical endpoints — Phase 2 only adds the coordinator `/register` listener.

---

## 12. FINAL — cross-phase regression (the Phase 1 math, the new way)

**This is the last test.** With all four sites still online and onboarded as in
Section 7 (Denmark / Finland / Norway via invites, Sweden via the legacy shared
token), run the five Phase 1 federated analyses against them and confirm the
numbers are unchanged. Because the per-site tokens and addresses come from the
invite flow (plus the one manual legacy site), this proves Phase 2 onboarding did
not disturb the Phase 1 math, and that a **mixed** session aggregates correctly.

**Build the server list from what the coordinator actually onboarded** (registry
= invited sites; append the manually-added legacy Sweden site):

```r
library(fedstats); source("engine/coordinator/registry.R")
reg <- reg_load("engine/coordinator/registered_sites.json")
invited <- Filter(function(r) identical(r$invite_state, "consumed") &&
                              !is.null(r$site_addr), reg$sites)
servers <- lapply(unname(invited), function(r) create_remote_server(r$site_addr, r$token))

# Append the legacy shared-token site (Sweden). Fill in its live values:
legacy_url   <- "<Sweden site address, e.g. http://100.x.x.x:8000>"
legacy_token <- "<the FED_TOKEN you set for Sweden, e.g. swespine2026>"
servers <- c(servers, list(create_remote_server(legacy_url, legacy_token)))
stopifnot(length(servers) == 4)
```

### 12.1 Descriptives — `fed_numeric(servers, "age")`

```r
r <- fed_numeric(servers, "age")
stopifnot(r$n == 16625,                       # all four sites connected
          abs(r$mean - 54.32)   < 0.01,
          abs(r$sd   - 11.82)   < 0.01,
          abs(r$sum  - 903078)  < 2)
cat(sprintf("age: n=%d mean=%.2f sd=%.2f sum=%.0f\n", r$n, r$mean, r$sd, r$sum))
```

**Expected:** `n=16625, mean=54.32, sd=11.82, sum=903078`. The `n == 16625` check
also confirms the mixed session (3 invited + 1 legacy) is fully connected.

### 12.2 Welch t — `fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")`

```r
w <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")
cat(sprintf("welch: t=%.4f df=%.1f p=%.3g\n", w$t, w$df, w$p))
```

**Expected:** matches the Phase 1 baseline. Independent cross-check on pooled data
in §12.6.

### 12.3 Chi-square 2×2 — `fed_chisq_2x2(servers, "sexM", "diag_myelo")`

```r
x <- fed_chisq_2x2(servers, "sexM", "diag_myelo")
cat(sprintf("chisq: X2=%.4f df=%d p=%.3g N=%d\n", x$statistic, x$df, x$p, x$N))
```

**Expected:** matches the Phase 1 baseline. Cross-check in §12.6.

### 12.4 Linear regression — `fed_lm(servers, nrs_arm_12m ~ age + sexM)`

```r
m <- fed_lm(servers, nrs_arm_12m ~ age + sexM)
print(round(m$coefficients, 6))
```

**Expected:** coefficients match the Phase 1 baseline **within ~1e-6**.

### 12.5 Logistic regression — `fed_logistic_newton(servers, mcid_arm_12m ~ age + sexM + nrs_arm_preop)`

```r
g <- fed_logistic_newton(servers, mcid_arm_12m ~ age + sexM + nrs_arm_preop)
stopifnot(g$converged)
print(round(g$coefficients, 6))
```

**Expected:** converges; coefficients match the Phase 1 baseline **within ~1e-6**.

### 12.6 Independent pooled cross-check (you hold all four CSVs)

Because the four CSVs are on your machine, reproduce the baseline directly by
pooling the raw data and comparing to the federated results above. Set `csv_dir`
to the folder where you placed the files (referenced **by name**, no hardcoded
path):

```r
csv_dir <- "<the folder where you placed the four CSVs>"
files   <- file.path(csv_dir, c("denmark.csv","finland.csv","norway.csv","sweden.csv"))
pooled  <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE,
                                 na.strings = c("", "NA", "NaN", "NULL")))
stopifnot(nrow(pooled) == 16625)

# Descriptives
stopifnot(abs(r$mean - mean(pooled$age)) < 1e-9,
          abs(r$sum  - sum(pooled$age))  < 1e-6)

# Welch t  (base R Welch test on the two diagnosis groups)
sub <- pooled[pooled$diag_grp %in% c("radiculopathy","myelopathy"), ]
tt  <- t.test(age ~ diag_grp, data = sub)              # Welch by default
stopifnot(abs(abs(w$t) - abs(unname(tt$statistic))) < 1e-6,
          abs(w$p - tt$p.value) < 1e-6)

# Chi-square 2x2
ct <- chisq.test(table(pooled$sexM, pooled$diag_myelo), correct = TRUE)
stopifnot(abs(x$statistic - unname(ct$statistic)) < 1e-6,
          abs(x$p - ct$p.value) < 1e-6)

# Linear regression
lm_p <- lm(nrs_arm_12m ~ age + sexM, data = pooled)
stopifnot(max(abs(m$coefficients - coef(lm_p))) < 1e-6)

# Logistic regression
gl_p <- glm(mcid_arm_12m ~ age + sexM + nrs_arm_preop, family = binomial, data = pooled)
stopifnot(max(abs(g$coefficients - coef(gl_p))) < 1e-6)

cat("Section 12 OK — federated results match pooled baseline\n")
```

**Expected:** prints `Section 12 OK …`. The federated results (from invite-
onboarded + legacy sites) equal the pooled base-R results — the same invariant
Phase 1 established, now confirmed through the Phase 2 onboarding path.

> If §12.6 passes but §12.4/§12.5 disagree with your *recorded Phase 1 numbers*,
> the discrepancy is in the Phase 1 record, not the math — the pooled cross-check
> is authoritative. If §12.6 itself fails, Phase 2 changed behaviour: stop and
> investigate before merging.

---

## Sign-off

| Item | Result | Notes |
|------|--------|-------|
| 1 Invite format & crypto (automated) | PASS | invite len 448, prefix `FEDSTAT2.` |
| 2 Registry state machine (automated) | PASS | full transition set exercised |
| 3 Registrar endpoint auth + rate limit (automated) | PASS | 401/404/429 all confirmed on Tailscale-bound listener |
| 4 Coordinator GUI — invites & registry | | |
| 5 Site Join flow (Denmark) | | |
| 6 UX timing — onboard in < 60 s | | secs: ___ |
| 7 Mixed session — 3 invited + 1 legacy (Sweden) | | |
| 8 End-to-end onboarding smoke (GUI) | | |
| 9a Wrong invite to wrong operator | | |
| 9b Expired invite rejected | | |
| 9c Revoked invite cannot re-register | | |
| 9d Retry after network drop completes | | |
| 9e Leaked-invite collision → approval | | |
| 9f TOFU key-change warning | | |
| 9g Optional fingerprint matches | | |
| 10 Backward-compat server auth (automated) | PASS | n=4169 (denmark.csv), wrong token → 401 |
| 11 Phase 1 mechanisms intact (binding/min_n/protocol) | | |
| 12 FINAL cross-phase regression (5 analyses) | PASS* | see note below |

\* **Section 12 initially FAILED §12.6's logistic-regression cross-check**
(coefficients differed from pooled `glm()` by up to 2.4e-5, exceeding the
1e-6 tolerance). Root cause: `jsonlite::toJSON()`'s default `digits = 4`
rounds to 4 **decimal places**, not significant figures — it was silently
zeroing gradient/beta entries under 5e-5 and truncating others on *every*
Newton-Raphson round trip (`fedstats/R/remote.R` request body and
`engine/site/api_server.R` response serializer both omitted `digits`).
Fixed by setting `digits = 15` on both sides; re-ran Sections 3, 10, 12 in
full afterward with no regressions. All five federated analyses now match
the pooled base-R baseline within 1e-6.
