# Final Validation Protocol

Use this protocol before presenting or releasing the final version.

## Goal

Confirm that:

1. The normal workflow still works for non-technical users.
2. Privacy protections are active.
3. Security hardening did not break the coordinator-site pipeline.

## Setup

Use one laptop with:

- 1 coordinator window
- 2 or 3 site windows
- 2 or 3 different local CSV files

Recommended branch:

- `alice/final-security-hardening`

## Step 1: Basic startup

1. Start the coordinator from `Run/Windows/Start Coordinator.bat`.
2. Start two site windows from `Run/Windows/Start Site.bat`.
3. Confirm the apps open without startup errors.

Pass condition:

- coordinator opens
- site windows open
- no immediate crash in the launcher console

## Step 2: Invite and join flow

1. In the coordinator, create one invite per site.
2. Paste each invite into a different site window.
3. Confirm the join dialog shows the study name, coordinator, and key fingerprint.
4. Complete the join flow.

Pass condition:

- each site can join successfully
- no manual token entry is needed
- the intended site label is visible

## Step 3: Start site servers

1. Choose one CSV per site.
2. Click `Start Server` on each site.
3. Wait until each site shows the running/connected state.

Pass condition:

- each site starts successfully
- each site shows a usable address
- each site registers back to the coordinator

## Step 4: Coordinator reachability

1. In the coordinator, confirm the registered sites appear in the sites table.
2. Click `Ping`.

Pass condition:

- all active sites return as reachable
- no authentication or connection errors appear unexpectedly

## Step 5: Minimum-site guardrail

1. Repeat the workflow with only one active site, or temporarily stop all but one.
2. Click `Validate`.
3. Click `Run`.

Pass condition:

- the coordinator blocks validation and analysis with only one active site
- the message is understandable and does not expose technical internals

## Step 6: Validation behavior

1. Load an analysis script with `VARS_SPEC`.
2. Click `Validate`.

Pass condition:

- validation completes normally with sufficiently clean data
- missing columns, bad binary coding, or impossible values are reported clearly
- raw patient-level values are never shown

## Step 7: Privacy-focused validation check

Use a dataset with:

- a rare category
- a tiny outlier count
- a variable with a moderate sample size

Pass condition:

- exact min and max are not returned
- tiny outlier counts are not exposed exactly
- rare category counts are suppressed
- quartiles are withheld when the stricter threshold is not met

## Step 8: Small-cell suppression

Use a dataset where one subgroup is below `min_cell`.

1. Run a grouped descriptive analysis.
2. Run a Welch t-test or 2x2 chi-square that depends on the small subgroup.

Pass condition:

- the small subgroup is suppressed
- inferential analysis fails closed instead of returning a biased result

## Step 9: Invite-link restriction

1. In a site window, paste a malformed or non-Tailscale invite URL.
2. Try to join.

Pass condition:

- the site rejects the link
- the error is clear
- the app does not follow arbitrary URLs

## Step 10: Audit logs

After validation and analysis:

1. Check `engine/coordinator/study_log.jsonl`
2. Check `engine/site/site_query_audit.jsonl`

Pass condition:

- the coordinator log records validation and analysis events
- the site log records sensitive query activity

## Step 11: Final smoke check

Run one complete normal analysis with at least two sites.

Pass condition:

- `Ping` works
- `Validate` works
- `Run` works
- outputs are generated
- no privacy guard triggers unexpectedly in an ordinary workflow

## Optional automated checks

If `testthat` is installed:

1. Run the package tests.
2. Run the privacy test script.

Recommended commands:

```powershell
cd "C:\Users\Alice Alonso\OneDrive\Documentos\MICN lab\federated_statistics"
Rscript fedstats/tests/testthat.R
Rscript analysis/test_privacy_suite.R
```

## Release decision

The build is ready for presentation when:

1. The normal workflow passes.
2. The privacy checks pass.
3. The minimum-site and suppression guardrails behave as expected.
4. The audit logs are created.
5. No unexpected auth, network, or startup regressions appear.
