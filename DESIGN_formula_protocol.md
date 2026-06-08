# Design: Structured Model Specification Protocol (Phase 1, Item 1)

## Threat Model

**What this protocol is defending against:**

An attacker who controls the coordinator process, or who has obtained the bearer
token, must not be able to execute arbitrary code on the site server. The
structured protocol achieves this by ensuring every string interpolated into an
R expression on the server originates from the server's own column names — never
from network input. Variable names received over the wire are validated against a
strict whitelist regex before use; the formula is assembled entirely from those
validated names. No network-supplied string ever reaches `eval`, `parse`,
`system`, or `as.formula` with attacker-controlled content.

**What this protocol does not defend against:**

- A fully compromised site server (OS-level attacker already present on the
  hospital machine)
- Statistical inference attacks (a coordinator issuing many narrow queries to
  re-identify individuals) — this is addressed separately by per-query `min_n`
  enforcement (Phase 1 item 4)
- Network-level eavesdropping — that is Tailscale's responsibility (WireGuard
  encryption); the API itself runs over plain HTTP within the Tailscale network

---

## Problem

The current wire protocol for the three formula-bearing endpoints
(`/termnames`, `/grad_hess`, `/lm_suffstats`) sends an R formula as a raw
string:

```json
{ "formula": "outcome ~ pred1 + pred2" }
```

The site server reconstructs it with `as.formula(body$formula)`. R formula
expressions can embed arbitrary code — `y ~ {system("curl ...", intern=TRUE)}`
is syntactically valid. A compromised coordinator machine, a stolen token, or a
misconfigured network path could run arbitrary R on every participating hospital
server.

The `/validate` endpoint has the same issue via its optional `formula` field.
Variable-name-only endpoints (`/summary_numeric`, `/group_summaries`,
`/counts_2x2`) are structurally safe but currently do no input validation, so
a crafted variable name string could reach `df[[varname]]` without a check.

---

## Proposed fix

Replace formula strings on the wire with a **structured model specification**
(JSON object). The site server reconstructs a formula from validated field
values only — no R expression is ever evaluated from user-supplied text.

The public user-facing API (`fed_lm`, `fed_logistic_newton`, etc.) is
**unchanged**. Users still write R formulas. Parsing happens inside
`create_remote_server()` in `fedstats/R/remote.R`, before anything is
transmitted.

---

## Scope of current formula usage

| User function | Endpoint(s) called | Sends formula? |
|---|---|---|
| `fed_numeric` | `/summary_numeric` | No |
| `fed_group_numeric` | `/group_summaries` | No |
| `fed_welch_t` | `/group_summaries` | No |
| `fed_chisq_2x2` | `/counts_2x2` | No |
| `fed_lm` | `/lm_suffstats` | **Yes** |
| `fed_logistic_newton` | `/termnames`, `/grad_hess` | **Yes** |
| `fed_validate` | `/validate` | **Yes** (optional) |

All current analysis templates use additive main-effects formulas only
(`y ~ x1 + x2 + x3`, optionally with `- 1` to suppress the intercept). No
template uses interactions (`*`, `:`), polynomial terms (`I(x^2)`), or
transformation functions (`log(x)`).

---

## Design decision: main-effects only

The structured spec supports **additive main-effects models only**: an outcome
variable, a list of predictor variable names, and an intercept flag. This
covers 100% of current templates.

**Trade-off accepted:** interaction terms and in-formula transformations
(`I(x*z)`, `log(x)`) are not representable in the new protocol. If a user
writes `fed_lm(servers, y ~ x1 * x2)`, `remote.R` will raise an error with
guidance to pre-compute the interaction column in the data file. This is
preferable to allowing arbitrary R code execution on hospital servers.

Pre-computed interaction columns (added by the site operator to their CSV) are
actually better practice in a federated context: they ensure both sites encode
the interaction identically, make the data contract explicit, and are visible
to the site operator.

---

## JSON schema

### `model_spec` object (replaces the `formula` string field)

```json
{
  "model_spec": {
    "outcome":    "string  — outcome variable name",
    "predictors": ["string", "..."],
    "intercept":  true
  }
}
```

| Field | Type | Required | Default | Notes |
|---|---|---|---|---|
| `outcome` | string | yes | — | Must pass variable-name validation (see below) |
| `predictors` | array of strings | yes | — | At least one; each must pass variable-name validation |
| `intercept` | boolean | no | `true` | Set to `false` for no-intercept models |

#### Variable-name validation rule

```
^[A-Za-z][A-Za-z0-9_.]{0,63}$
```

- Starts with a letter (ASCII)
- Followed by letters, digits, `.`, or `_`
- Maximum 64 characters
- Rejects: spaces, parens, braces, quotes, backticks, operators, empty string

This accepts all standard R and CSV column names. It rejects everything that
could be executable R syntax or an injection attempt.

---

## Per-endpoint wire format (before → after)

### `/termnames`

**Before:**
```json
{ "formula": "outcome ~ x1 + x2" }
```

**After:**
```json
{
  "model_spec": {
    "outcome": "outcome",
    "predictors": ["x1", "x2"],
    "intercept": true
  }
}
```

Response: unchanged — `{ "termnames": ["(Intercept)", "x1", "x2"] }`

---

### `/lm_suffstats`

**Before:**
```json
{ "formula": "height ~ age + sex" }
```

**After:**
```json
{
  "model_spec": {
    "outcome": "height",
    "predictors": ["age", "sex"],
    "intercept": true
  }
}
```

Response: unchanged — XtX, Xty, yTy, n, termnames.

---

### `/grad_hess`

**Before:**
```json
{
  "formula": "outcome ~ age + sex",
  "beta": [0.0, 0.0, 0.0],
  "beta_names": ["(Intercept)", "age", "sex"]
}
```

**After:**
```json
{
  "model_spec": {
    "outcome": "outcome",
    "predictors": ["age", "sex"],
    "intercept": true
  },
  "beta": [0.0, 0.0, 0.0],
  "beta_names": ["(Intercept)", "age", "sex"]
}
```

Response: unchanged — grad, hess, ll, n, termnames.

---

### `/validate`

The optional `formula` string field becomes an optional `model_spec` object.

**Before:**
```json
{
  "vars_spec": { ... },
  "formula": "outcome ~ x1 + x2",
  "min_n": 20
}
```

**After:**
```json
{
  "vars_spec": { ... },
  "model_spec": {
    "outcome": "outcome",
    "predictors": ["x1", "x2"],
    "intercept": true
  },
  "min_n": 20
}
```

If `model_spec` is absent, formula checking is skipped (same as before).

---

### Variable-name endpoints (no structural change, add validation)

`/summary_numeric`, `/group_summaries`, `/counts_2x2` already send only
variable name strings. The request body schema is unchanged; the server adds a
`validate_varname()` call before using any string as a column selector.

---

## Server-side implementation (site server, `api_server.R`)

Two new helpers added to `api_server.R`:

```r
.validate_varname <- function(name) {
  if (!is.character(name) || length(name) != 1L ||
      !grepl("^[A-Za-z][A-Za-z0-9_.]{0,63}$", name))
    stop(paste0("Invalid variable name: '", name, "'"))
  invisible(name)
}

.spec_to_formula <- function(spec) {
  outcome <- .validate_varname(spec$outcome)
  preds   <- vapply(unlist(spec$predictors, use.names = FALSE),
                    .validate_varname, character(1))
  if (length(preds) == 0L) stop("model_spec: at least one predictor required.")
  intercept <- !identical(spec$intercept, FALSE)
  rhs <- if (intercept) paste(preds, collapse = " + ")
         else paste0("0 + ", paste(preds, collapse = " + "))
  as.formula(paste(outcome, "~", rhs), env = baseenv())
}
```

Using `env = baseenv()` means the formula environment contains no user-defined
functions — `as.formula("y ~ system('cmd')", env = baseenv())` cannot resolve
`system` because it is not in `baseenv()`. This is a defence-in-depth layer on
top of the structural validation.

Each endpoint that previously called `as.formula(body$formula)` is replaced
with `.spec_to_formula(body$model_spec)`.

---

## Client-side implementation (`fedstats/R/remote.R`)

One new private helper:

```r
.model_spec_from_formula <- function(formula) {
  tt <- terms(formula)
  vars <- all.vars(tt)
  resp <- as.character(attr(tt, "variables")[[attr(tt, "response") + 1]])
  preds <- setdiff(vars, resp)
  has_intercept <- as.logical(attr(tt, "intercept"))
  if (length(attr(tt, "order")[attr(tt, "order") > 1]) > 0 ||
      any(grepl("^I\\(", as.character(attr(tt, "variables"))[-1])))
    stop(paste0(
      "Formula contains interactions or in-formula transformations, which are\n",
      "not supported in the federated protocol.\n",
      "Pre-compute interaction/transformation columns in your data file instead."))
  list(outcome = resp, predictors = as.list(preds), intercept = has_intercept)
}
```

This is called inside `termnames`, `lm_suffstats`, and `grad_hess` before the
HTTP POST, replacing `.formula_to_string(formula)`.

---

## Files changed

| File | Change |
|---|---|
| `engine/site/api_server.R` | Add `.validate_varname`, `.spec_to_formula`; replace `as.formula(body$formula)` in 4 endpoints; add column-existence check in 4 endpoints; add varname + existence validation in 3 variable-name endpoints; add `family` validation to `/grad_hess` |
| `fedstats/R/remote.R` | Add `.model_spec_from_formula` (with interaction detection and user-facing error); update 3 functions to send `model_spec`; send `family` in `grad_hess` |

No other files change. The in-process `create_server()` in `server.R` is
unaffected — it receives R formulas directly and always will.

---

## Additional coverage

### Categorical predictors and factor levels

The structured spec transmits only variable names, not factor levels. On the
server, `model.matrix()` is called on the reconstructed formula against the
site's own data, so dummy variables are generated from whatever levels exist in
that site's dataset — identical to the current behaviour.

This means level alignment across sites is **not enforced by the protocol**; it
is an existing responsibility of `fed_validate` (which already checks for
unexpected or missing levels). This design does not change that. If Site A codes
sex as `{Male, Female}` and Site B as `{M, F}`, the protocol still transmits the
same JSON spec; the misalignment surfaces during validation, not here.

No protocol change is needed for categoricals. They work exactly as before.

---

### GLM family argument

The `grad_hess` endpoint is currently hardcoded to binomial logit in
`server.R`. There is no family field anywhere. For Phase 1, `grad_hess`
remains binomial-logit only, but the wire format should make this explicit
rather than implicit.

**Addition to `/grad_hess` request:**

```json
{
  "family": "binomial_logit",
  "model_spec": { "outcome": "...", "predictors": [...], "intercept": true },
  "beta": [...],
  "beta_names": [...]
}
```

| Field | Type | Required | Default | Notes |
|---|---|---|---|---|
| `family` | string | no | `"binomial_logit"` | Only accepted value in Phase 1 |

The server validates the `family` field independently of the client. Client-side
validation is for UX (early error); server-side validation is the security
boundary. The server rejects any `family` value other than `"binomial_logit"`
with HTTP 400: `"Unsupported family: 'X'. Only binomial_logit is currently
supported."` A client that omits the field gets the default; a client that sends
anything else is rejected regardless of whether it passed client-side checks.

The client (`remote.R`) sends `"family": "binomial_logit"` explicitly in every
`grad_hess` call.

---

### Predictor name passes regex but is not a column in the dataset

`model.matrix()` would throw a cryptic R error ("object 'foo' not found") which
currently reaches the client as a 400 with an opaque message, or in some error
paths a 500.

**Fix:** each formula-bearing endpoint handler in `api_server.R` adds an
explicit column-existence check immediately after `.spec_to_formula()` and
before any statistical computation:

```r
missing_vars <- setdiff(
  c(spec$outcome, unlist(spec$predictors, use.names = FALSE)),
  names(site_data)
)
if (length(missing_vars) > 0)
  stop(paste0("Variable(s) not found in dataset: ",
              paste(missing_vars, collapse = ", ")))
```

This returns HTTP 400 with a human-readable message. The same check applies to
`/validate` when `model_spec` is present.

Variable-name endpoints (`/summary_numeric`, `/group_summaries`, `/counts_2x2`)
get the same treatment: after `validate_varname()`, check presence in
`names(site_data)` before accessing `site_data[[varname]]`.

---

### `intercept: false` — keep it

The field stays in the schema with default `true`. Removing it would silently
break `fed_lm(servers, y ~ 0 + x1)` or `y ~ -1 + x1`. Cost is zero.
`.model_spec_from_formula()` already extracts the intercept flag via
`attr(terms(formula), "intercept")`, so it round-trips correctly.

---

### User-facing error for unsupported formulas (interactions, transformations)

The error is raised in `.model_spec_from_formula()` in `fedstats/R/remote.R`,
before any network call. The message is:

```
Federated protocol error: formula contains interactions or in-formula
transformations (e.g. y ~ x1 * x2, y ~ I(x^2)). These cannot be
transmitted securely.
To use an interaction, add a pre-computed column to your data file:
  e.g. df$x1_x2 <- df$x1 * df$x2, then use y ~ x1 + x2 + x1_x2
```

Because `coordinator_app.R` wraps the entire analysis `source()` call in
`tryCatch(..., error = function(e) showNotification(conditionMessage(e), type =
"error", duration = 15))`, this message appears as a red notification banner in
the coordinator's browser UI. No R traceback is shown to the user — just the
message above.

---

## What stays the same

- The public R API: `fed_lm(servers, y ~ x1 + x2)` works identically
- All five analysis templates work without modification
- The in-process server (`create_server`) is untouched
- The test/simulation workflow (using local servers) is untouched
- Token auth, min_n, all other endpoints are untouched by this item

---

## Open questions (decide before coding)

**Q1.** Should the server explicitly reject requests that still contain the old
`formula` string field, or silently ignore it?

**Recommendation:** Reject with HTTP 400 and a clear message
(`"formula field no longer accepted; send model_spec instead"`). Fail-loud
prevents old clients silently bypassing the new validation.

**Q2.** Should `.model_spec_from_formula` be exported from the fedstats package
for users who want to inspect the spec?

**Recommendation:** No. It is an internal wire-format detail. Exporting it
invites users to build tooling that depends on the wire format, making future
changes harder.

**Q3.** What about the `validate_data` method on the in-process server
(`server.R`)? Its signature accepts an R formula directly (not via HTTP), so
there is no injection risk there. Leave it unchanged.

**Recommendation:** Agreed — no change needed to the in-process path.
