# Federated Statistics v2 — Project Charter

## Branches

| Branch | Purpose |
|--------|---------|
| `alice/phase1-security` | Phase 1 — Security hardening |
| `alice/phase2-onboarding` | Phase 2 — Onboarding and connection UX |
| `alice/phase3-polish` | Phase 3 — Reliability and polish |

The branches are **stacked**: `alice/phase1-security` is cut from `main`, `alice/phase2-onboarding` is built on Phase 1, and `alice/phase3-polish` will be built on Phase 2. Each phase is still independently testable and reviewable, but later phases contain the commits of earlier ones, so they are not independent PRs against `main`. Merge order is forced: Phase 1 first, then Phase 2, then Phase 3.

---

## Guiding Principles

1. **Don't break what works.** Every change must leave the existing happy path fully functional. A coordinator and site that worked before a phase must still work after it.

2. **Safer AND no harder to use.** Security improvements must not add steps or friction for non-technical users (hospital IT staff, clinicians). If a fix would require extra manual steps, it must be automated or surfaced through the GUI.

3. **Phases are stacked, and independently testable.** Phase 2 builds on Phase 1, and Phase 3 builds on Phase 2. Each phase is still independently testable and reviewable, but later phases contain the commits of earlier ones. Merge order is therefore forced: Phase 1 first, then Phase 2, then Phase 3.

---

## Phases

### Phase 1 — Security (`alice/phase1-security`)

**Goal:** Close the security holes without changing anything a doctor sees in the UI.

**In scope:**
1. Replace formula-as-string protocol with a structured term spec. User-facing R API (`fed_lm`, `fed_logistic`, `fed_welch_t`, etc.) is unchanged — parsing happens client-side, structured JSON goes over the wire, site server reconstructs the formula from whitelisted variable names only.
2. Authenticate the `/health` endpoint with `check_token()`.
3. Bind API server to the Tailscale interface only; fall back to `0.0.0.0` with a logged warning if Tailscale is not detected (preserves local testing).
4. Enforce `min_n` on a per-query basis across every statistical endpoint (`summary_numeric`, `group_summaries`, `counts_2x2`, `grad_hess`, etc.). Return a clear error when a query's underlying row count falls below the threshold.
5. Update the README's Privacy section with an honest, precise statement of the privacy model: no patient-level data leaves the site; all returned aggregates rest on at least `min_n` rows; the threat model and known limitations (e.g. a coordinator running many small queries) are stated explicitly.

**Out of scope for this phase:** any UI changes, any change to the onboarding/connection flow, differential privacy, refactoring unrelated to the items above.

**Definition of done:**
- All five items implemented, one commit per item
- `PHASE1_TESTING.md` written and passed manually
- External review prompt run; findings addressed or explicitly accepted
- No user-visible change to the analysis flow

---

### Phase 2 — Onboarding & Connection UX (`alice/phase2-onboarding`)

**Builds on:** Phase 1. This branch is stacked on `alice/phase1-security`, so it contains the Phase 1 security commits. It is independently testable and reviewable, but must be merged after Phase 1.

**Goal:** Replace the manual "email URLs and tokens" workflow with a single invite-bundle flow that a doctor can complete in under a minute.

**Target user flow:**
- Coordinator clicks "Invite a site" → gets a copyable invite code encoding study name, coordinator's Tailscale address, and a unique per-site token.
- Site operator pastes the invite code into their launcher, clicks Join. Token is configured automatically, study name recorded, site registers itself back to the coordinator.
- Coordinator UI shows registered sites by name with status badges.
- No manual URL typing. No out-of-band token coordination.

**In scope:**
1. Invite-bundle format and encoding (design decision: discuss before implementing)
2. Coordinator-side invite generation and registered-sites state (where state is persisted — file vs reactive-only — to be decided)
3. Site-side invite paste and auto-configuration in the launcher
4. Per-site tokens replacing the shared token, with a backward-compatibility path for sites already configured with the old shared token
5. Registered-sites list view in the coordinator UI (names, addresses, status)

**Out of scope for this phase:** status-badge live updates and resilience polish (deferred to Phase 3), any change to the analysis flow itself.

**Definition of done:**
- All items implemented, one commit per logical step
- Existing shared-token sites continue to work without reconfiguration
- `PHASE2_TESTING.md` written and passed manually
- External review prompt run; findings addressed or explicitly accepted

---

### Phase 3 — Reliability & Polish (`alice/phase3-polish`)

**Goal:** Make the system feel solid — no hangs, fast feedback, clear errors.

**In scope:**
1. `httr::timeout()` on `.remote_post()` in `fedstats/R/remote.R`, configurable via env var, sensible default (30s)
2. Parallelise `ping_one` across sites (future/promises) so the UI does not block
3. Live status badges on the coordinator's registered-sites list (connected / stale / offline based on last successful ping)
4. Friendly error messages: HTTP 401 → "Authentication failed — check your site's token"; connection refused → "Site unreachable — is the server running?"; timeout → "Site not responding — it may be offline."
5. Reactive Tailscale IP resolution in `site_app.R` (fixes the frozen-at-startup issue)
6. Clean up the OUTDATED section of `network instructions.txt` to match the current GUI-based flow
7. Remove the inconsistent `sudo tailscaled` auto-start from coordinator launchers; mirror the site-launcher check-and-warn behaviour

**Out of scope for this phase:** any security or onboarding changes, statistical method changes.

**Definition of done:**
- All items implemented, one commit per item
- `PHASE3_TESTING.md` written and passed manually
- External review prompt run; findings addressed or explicitly accepted
- The "site goes offline mid-analysis" baseline behaviour is now graceful (timeout, clear error) rather than a hang

---

## Regression Baseline

The current working setup (commit `eca1543` on `main`) is the baseline. The following must continue to work correctly after every phase:

- A site can start the server via the GUI launcher (Mac / Windows / Linux)
- The site's Tailscale IP and port are displayed correctly in the browser interface
- The coordinator can paste site URLs and run a federated analysis end-to-end
- All five analysis templates produce correct output: descriptives, Welch t, chi-square, linear regression, logistic regression
- Token authentication works when a token is configured, and the server runs correctly when no token is set
- The `engine/setup.R` dependency installer runs without error on a clean machine
- The `network setup/` scripts complete without error on a machine that already has Tailscale installed

- A reference analysis (`fed_lm` on a known synthetic dataset) produces coefficients matching pooled `lm` within numerical tolerance

Any change that breaks one of these is a regression and must be fixed before the branch is merged.

### Known baseline behaviour Phase 3 will intentionally change

- A site going offline mid-analysis currently hangs the coordinator UI indefinitely. After Phase 3 this will time out with a clear error message. This is an intended improvement, not a regression.

---

## Out of Scope for v2

- Changes to the statistical methods or federated algorithm
- New analysis types
- Infrastructure changes (hosting, containerisation, CI)
