# Security Changes vs `main`

This document summarizes the security and privacy changes introduced in the current hardened branch relative to the `main` branch of the GitHub repository.

## Access Confirmation

This comparison was produced from the local repository at:

- `C:\Users\Alice Alonso\OneDrive\Documentos\MICN lab\federated_statistics`

The repository has a configured GitHub remote:

- `origin = https://github.com/aliceadubost/federated_statistics.git`

The local clone contains both:

- `main`
- `origin/main`

The current working branch used for this comparison is:

- `alice/final-security-hardening`

## Summary Table

| Change | What It Protects | Practical Impact |
|---|---|---|
| Structured `model_spec` instead of raw formula strings | Prevents code injection through network-supplied formula text | Remote callers can no longer send arbitrary formula-like code into the site server |
| Server-side variable-name validation | Prevents malicious column selectors and unexpected symbol injection | Only whitelisted variable-name syntax is accepted before any formula reconstruction |
| Authenticated `/health` endpoint | Prevents unauthenticated probing of site liveness | A protected site does not reveal its status to callers without the token |
| Bind to Tailscale or loopback by default | Prevents accidental exposure of patient-derived aggregates to LAN/internet | If Tailscale is unavailable, the service falls back to `127.0.0.1` instead of `0.0.0.0` |
| Per-query `min_n` enforcement | Prevents returning statistics for undersized samples | Even if a server is running, each statistical request must still meet the minimum sample size |
| Small-cell suppression (`min_cell`) | Prevents direct leakage from tiny groups or rare contingency-table cells | Unsafe subgroup cells are withheld instead of returned |
| Refusal to compute biased inferential results | Prevents silent leakage or misleading pooled tests when a site had to suppress a cell | Welch t-tests and 2x2 chi-square fail closed instead of computing on incomplete data |
| Per-site tokens | Limits the blast radius of a leaked token | A single leaked token affects one site, not the entire study |
| Signed invites with Ed25519 | Prevents invite tampering and forgery | Sites verify that an invite really came from the coordinator before trusting it |
| Authenticated site registration callback | Prevents unauthenticated or forged site registration | The coordinator requires both the invite token and a valid site signature |
| Invite expiry and revocation | Prevents reuse of old onboarding material | Expired or revoked invites/tokens are rejected |
| Replay/collision-aware registration state machine | Prevents silent takeover of an already-issued invite | The coordinator can distinguish a legitimate retry from a second host reusing the invite |
| Registrar rate limiting | Reduces brute force and registration flooding | Excessive registration attempts are throttled |
| Secret-file hardening | Reduces local leakage of tokens and private keys | Registry files and key files are kept out of git and written with restricted permissions |
| Windows ACL verification warnings | Reduces silent permission failures on Windows | The UI can warn when secret files may still be readable by other principals |
| Safer validation output | Reduces leakage through pre-analysis validation | Validation no longer exposes exact minima/maxima, tiny outlier counts, or rare-category detail as easily |
| Stricter quartile release threshold | Reduces leakage through order statistics | Quartiles and median require a larger backing sample than simple means |
| Rare-category count suppression in validation | Reduces disclosure of uncommon levels | Rare levels are not surfaced with easy-to-reconstruct counts |
| Tiny outlier-count suppression | Reduces disclosure through rare out-of-range events | Validation can say that outliers exist without revealing an exact tiny count |
| Site-side sensitive-query audit log | Improves traceability of privacy-relevant access | The site records sensitive descriptive/validation queries in a persistent audit file |
| Repeated-query / anti-differencing guardrails | Reduces inference by repeated probing of very similar questions | Exact duplicate sensitive queries are briefly blocked and excessive distinct probing is limited |
| Coordinator minimum-site guardrail | Reduces cases where one site dominates a federated result too closely | Validation and analysis can be blocked until at least two registered sites are active |
| Persistent coordinator run audit log | Improves accountability and publication traceability | The coordinator records validation and analysis runs, participating sites, and privacy warnings |
| Safer invite-link handling | Reduces SSRF-style risk from arbitrary URLs | The site accepts only expected coordinator short links on the Tailscale path |
| Friendly but specific network/auth errors | Reduces operator confusion around failures without weakening controls | Users see clearer messages for auth failure, timeout, or unreachable sites |
| Dedicated privacy/crypto/network tests | Reduces regression risk | Security-sensitive behaviors now have explicit test coverage instead of being implicit |

## What Changed Most for Patient-Data Leakage Risk

The most important changes for patient-data leakage are:

1. Small-cell suppression and fail-closed inference.
2. Per-query minimum sample-size enforcement.
3. Safer validation output with less granular disclosure.
4. Per-site tokens and signed onboarding.
5. Sensitive-query audit logging and anti-differencing guardrails.

## Local Testing Guidance

### Do I need to run the tests?

Yes, ideally:

1. Run the automated tests you can run locally.
2. Run a short manual workflow test of the coordinator and site apps.
3. Run one privacy-focused scenario test before final presentation.

Automated tests help catch regressions. Manual tests are still important here because the project includes:

- GUI flows
- background subprocesses
- network behavior
- onboarding behavior
- privacy controls that are easiest to confirm through end-to-end use

### Can this be simulated on one laptop?

Yes. You can simulate a realistic end-to-end workflow on one laptop.

Recommended local setup:

1. Start one coordinator window.
2. Start two or three site windows on the same machine.
3. Use different local CSV files for each site.
4. Join each site with its own invite.
5. Start the site servers and run `Ping`, `Validate`, and `Run` from the coordinator.

You can test:

- invite generation and join flow
- token-protected site registration
- per-site analysis calls
- validation behavior
- suppression behavior
- minimum-site guardrails
- audit-log creation

### Suggested one-laptop test plan

1. Normal workflow:
   Start the coordinator, invite two sites, join both, start both servers, then confirm `Ping`, `Validate`, and `Run` work normally.

2. Minimum-site guardrail:
   Leave only one registered site and confirm the coordinator blocks validation and analysis.

3. Validation privacy:
   Use a dataset with rare categories or planted outliers and confirm validation reports the issue without exposing exact tiny counts or raw values.

4. Suppression behavior:
   Use a dataset where a subgroup falls below `min_cell` and confirm descriptive or inferential calls fail closed instead of leaking or silently biasing the result.

5. Invite-link restriction:
   Paste a non-Tailscale or malformed invite URL and confirm the site rejects it.

6. Audit logs:
   Confirm these files are created when expected:
   - `engine/coordinator/study_log.jsonl`
   - `engine/site/site_query_audit.jsonl`

### Can Codex run the tests here?

Partly, yes.

What was successfully checked in this environment:

- R parse checks on the modified files
- a local smoke test of the hardened `validate_data()` behavior

What was not fully runnable here:

- the full `testthat` suite, because `testthat` is not installed in this environment
- the interactive GUI workflow, because that is best verified by launching the coordinator and site apps directly on your machine

### Best practical recommendation

For final confidence, use this three-layer check:

1. Automated package tests.
2. One-laptop end-to-end simulation with two or three local sites.
3. One short privacy-stress run using the dedicated privacy test script.

## Remaining Recommended Improvements

The project is much stronger after this hardening pass, but the most useful remaining improvements would be:

1. Add automated tests specifically for the new anti-differencing and rate-limit logic.
2. Document the new privacy-related environment variables in one place.
3. Add a small visible “privacy protections active” status block in the coordinator UI so non-technical reviewers can immediately see that guardrails are enabled.
