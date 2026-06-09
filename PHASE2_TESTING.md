# Phase 2 — Manual Testing Checklist

Branch: `alice/phase2-onboarding` (stacked on `alice/phase1-security`)

Covers the invite-bundle onboarding flow: signed invites, the coordinator
registrar, per-site tokens, the site Join flow, and — critically — that
**existing shared-token sites keep working without reconfiguration**.

Design reference: `DESIGN_invite_bundle.md`. Mark each item PASS / FAIL / SKIP.

---

## Setup

```r
# In R, from the project root — reinstall fedstats after the package changes.
install.packages("fedstats", repos = NULL, type = "source")
# (or: R CMD INSTALL fedstats)
```

A real run needs Tailscale connected (the registrar and site servers bind to the
Tailscale interface). The automated sections below run on one machine; they bind
to that machine's Tailscale IP, so a working `tailscale ip -4` is required. Where
a section is GUI-driven, two machines on the same tailnet are ideal but one
machine with two browser windows works for a smoke test.

A small synthetic dataset for the site:

```r
set.seed(1)
dir.create("data", showWarnings = FALSE)
write.csv(data.frame(age = round(rnorm(40, 55, 12)),
                     sex = sample(c("M","F"), 40, TRUE),
                     outcome = rbinom(40, 1, 0.3)),
          "data/site1.csv", row.names = FALSE)
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
- [ ] **4b** Click **Invite a site**, enter a study + site name, **Create invite**.
  A `FEDSTAT2.` string appears with a working **Copy** button.
- [ ] **4c** The new site appears in the table with status **Invited**.
- [ ] **4d** No `registered_sites.json` is committed: `git status` shows it ignored.
- [ ] **4e** On a Unix host, the launcher console prints no world-readable warning
  (the registry and key files are `600`).

---

## 5. Site Join flow (manual)

Launch **Start Site** (second machine, or second browser). Then:

- [ ] **5a** Paste the invite, click **Join** → a confirm dialog shows the study
  name and the coordinator key fingerprint **matching 4a**.
- [ ] **5b** Confirm → "Joined study …" banner appears; the Token field is filled.
- [ ] **5c** Select the data file, click **Start Server** → status reaches
  **Running** and the log shows `Registered with coordinator.`
- [ ] **5d** Back on the coordinator, the site flips to **Registered** with its
  address — without anyone typing the address.
- [ ] **5e** A `site_config.json` now exists (gitignored). Stop and restart the
  site server → it re-registers automatically (no re-paste).

---

## 6. End-to-end onboarding + analysis (manual)

- [ ] **6a** With the site registered (Section 5), load
  `analysis/templates/demo_descriptives.R` on the coordinator.
- [ ] **6b** **Ping sites** → shows `[OK] n = … rows` for the site (using its
  per-site token, no token typed by the coordinator).
- [ ] **6c** **Validate data** passes.
- [ ] **6d** **Run analysis** completes and shows result tabs.

---

## 7. Backward compatibility — shared-token site (manual, REQUIRED)

A site set up the **old way** must keep working with **no reconfiguration**.

- [ ] **7a** On the site, **do not** paste an invite. Type a token (e.g.
  `swespine2026`) in the **Token** field, select the data file, **Start Server**.
  (Equivalently, start `api_server.R` with `FED_TOKEN=swespine2026` as before.)
- [ ] **7b** The server starts and shows its address, exactly as in Phase 1.
- [ ] **7c** On the coordinator, click **Add manually**, enter the site URL and
  the same token, **Add**. The site appears with a **Manual** badge.
- [ ] **7d** **Ping → Validate → Run** all work against the manually-added site.
- [ ] **7e** A site that previously ran with no token at all still works: empty
  Token field on both sides, **Add manually** with a blank token.

**Expected:** the old shared-token workflow is unchanged; no invite, no
`site_config.json`, no key material is required for a legacy site.

Automated confirmation that the unchanged `api_server` accepts a per-site token
and rejects a wrong one (run from project root, needs `data/site1.csv` and
Tailscale up):

```r
library(fedstats); library(processx); library(httr)
host <- fed_bind_host()$host; tok <- fed_token()
env <- Sys.getenv(); env["FED_DATA_FILE"] <- "data/site1.csv"
env["FED_PORT"] <- "8123"; env["FED_TOKEN"] <- tok
p <- process$new("Rscript", "engine/site/api_server.R", env = env, wd = getwd(),
                 stdout = "|", stderr = "|"); Sys.sleep(3)
url <- sprintf("http://%s:8123", host)
stopifnot(create_remote_server(url, tok)$summary_numeric("age")$n == 40)
stopifnot(tryCatch({ create_remote_server(url, "WRONG")$summary_numeric("age"); FALSE },
                   error = function(e) grepl("401|nauthor", conditionMessage(e))))
p$kill(); cat("Section 7 (server auth) OK\n")
```

---

## 8. Security behaviours (manual)

- [ ] **8a Expiry.** Create an invite, then set `FED_INVITE_TTL_DAYS=0` for a new
  invite (or wait past `exp`). Joining shows "This invite has expired."
- [ ] **8b Revoke and Remove.** On the coordinator, click **Revoke** on a site.
  The row disappears; the site can no longer re-register (its `/register`
  returns an error). Confirms revocation is enforced, not just hidden.
- [ ] **8c Leaked-invite collision.** From a *different* address (or a different
  site keypair), register with the same invite → coordinator shows **Needs
  approval**, not silent acceptance. **Approve** adopts the new identity.
- [ ] **8d TOFU key change.** Re-join on a site with an invite signed by a
  *different* coordinator key → the confirm dialog shows the
  "coordinator's key has changed" warning.
- [ ] **8e Optional fingerprint.** The coordinator's fingerprint (4a) matches the
  one shown to the site (5a). This is offered, not forced.

---

## 9. Regression baseline (manual)

Phase 1 behaviour and the analysis flow must be unchanged:

- [ ] **9a** All five demo templates run end-to-end (as in PHASE1_TESTING §6).
- [ ] **9b** `fed_lm` coefficients still match pooled `lm()` within 1e-8.
- [ ] **9c** Site server still binds to the Tailscale interface; falls back to
  `0.0.0.0` with a warning when Tailscale is absent.
- [ ] **9d** Per-query `min_n` enforcement still refuses small queries.
- [ ] **9e** No change to the statistical wire protocol or `api_server.R`
  endpoints (Phase 2 only adds the coordinator `/register` listener).

---

## Sign-off

| Item | Result | Notes |
|------|--------|-------|
| 1 Invite format & crypto (automated) | | |
| 2 Registry state machine (automated) | | |
| 3 Registrar endpoint auth + rate limit (automated) | | |
| 4 Coordinator GUI — invites & registry | | |
| 5 Site Join flow | | |
| 6 End-to-end onboarding + analysis | | |
| 7 Backward compatibility — shared-token site | | |
| 8a Invite expiry enforced | | |
| 8b Revoke and Remove enforced | | |
| 8c Leaked-invite collision → approval | | |
| 8d TOFU key-change warning | | |
| 8e Optional fingerprint matches | | |
| 9 Regression baseline (Phase 1 intact) | | |
