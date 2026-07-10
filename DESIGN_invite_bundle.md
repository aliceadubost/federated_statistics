# Phase 2 Design — Invite Bundle & Onboarding Flow

**Status:** Revision 2 — incorporates review decisions (signed invites, invite
lifecycle state machine, registration auth + rate limiting, file hardening,
revoke-and-remove). No code is written until this is approved.
**Branch:** `alice/phase2-onboarding` (stacked on `alice/phase1-security`).
**Goal:** Replace the manual "email URLs and tokens" workflow with a single
copy-paste invite that a non-technical operator can complete in under a minute,
without changing the analysis flow or the Phase 1 wire protocol.

---

## 0. The one decision everything else hangs on

Today the system is **one-directional**:

```
Coordinator (Shiny app)  ──HTTP──▶  Site (plumber api_server)
        (caller)                          (responder)
```

The coordinator has **no inbound endpoint**. The operator manually types each
site's `http://100.x.x.x:8000` URL and a shared token into the coordinator GUI
(`coordinator_app.R`, the "Site URLs" textarea + "Security token" field).

The target flow says *"the site registers itself back to the coordinator… no
manual URL typing."* For the coordinator to learn a site's address **without
the operator typing it**, that address has to travel **site → coordinator**.
That direction does not exist yet. So Phase 2 must choose one of:

| Option | How the coordinator learns the site address | New components | Verdict |
|--------|---------------------------------------------|----------------|---------|
| **A. Registration callback (push)** | Site POSTs its address to a small listener on the coordinator | Coordinator runs a minimal `/register` HTTP listener | **Recommended** |
| B. Manual address-back (semi-pull) | Site shows its address; operator copies it to the coordinator | None | Rejected — reintroduces the manual step we're removing |
| C. Hosted rendezvous server | Both talk to a third-party server | External service | Rejected — infra change, out of scope for v2 |

> **Recommendation: Option A.** The invite already needs to carry the
> coordinator's address (so the site knows whom to call). We use exactly that to
> let the site call home once, at Join time. This is the only option that
> actually delivers "no manual URL typing," and it stays inside the Tailscale
> tunnel, so we add no new trust boundary — only a new endpoint.

**Cost of Option A — be honest about it:** the coordinator becomes a *server*,
not just a client. We must stand up a plumber listener alongside the Shiny app.
We will not block the Shiny event loop with it; instead we mirror the pattern
`site_app.R` already uses for `api_server.R`: **run the registrar as a
`processx` subprocess** that writes incoming registrations to a JSON file, and
have the Shiny app poll that file with a `reactiveTimer` (exactly how
`site_app.R` already tails its subprocess log). No new concurrency model, no new
dependency — just a second instance of a pattern already in the repo.

The registrar is a network-exposed endpoint, so it is hardened from the start:

- **Tailscale-bound only.** It binds to the Tailscale interface using the **exact
  binding logic Phase 1 added to `api_server.R`** (the `tailscale ip -4` →
  `100.*` check, fall back to `0.0.0.0` with a logged warning). To avoid
  reinventing it, that logic is factored into one shared helper that both
  `api_server.R` and the registrar call. *(Q1b)*
- **Authenticated.** There is **no unauthenticated registration.** A `POST
  /register` must carry `Authorization: Bearer <invite-token>`; the registrar
  rejects any request whose token does not match a currently-registerable invite
  (issued/in_use, not expired, not revoked — see §5a). The invite signature is
  also verified. *(Q1a)*
- **Rate-limited.** `/register` is throttled per source to blunt token-guessing
  and registration floods — a small in-memory token bucket in the registrar
  process (e.g. N attempts / minute / source, with a global ceiling). Rejected
  attempts return `429` and are logged. *(Q1c)*

Everything below assumes Option A.

---

## 1. Invite-bundle format

### Fields

The invite is a signed envelope: a `payload` object plus the coordinator's
public key and an Ed25519 signature over the payload.

```jsonc
{
  "payload": {
    "v":     1,                       // schema version (forward compat)
    "study": "SweSpine 2026",         // study name, shown to the operator
    "coord": "coordinator.tailnet.ts.net:8731", // MagicDNS host (IP fallback) + registrar port
    "sid":   "s_7f3a9c2e1b",          // site id the coordinator assigns to THIS invite
    "name":  "Karolinska",            // human label the coordinator pre-fills (editable)
    "tok":   "K7zQ4m9xR2...",         // per-site bearer token (24 random bytes, base64url)
    "iat":   1749427200,              // issued-at (unix seconds)
    "exp":   1750032000               // expires-at (unix; configurable, default +7 days)
  },
  "pk":  "9Xb2...",                    // coordinator Ed25519 public key (base64url)
  "sig": "f4A1..."                     // Ed25519 signature over canonical(payload)
}
```

`sid` is the join key: it ties a later registration back to the exact invite the
coordinator generated, so the coordinator can match "who just called home" to a
row in its registry. `tok` is the same secret Phase 1's `check_token()` already
compares — we are **not** changing the token mechanism, only minting one token
per invite instead of one shared across all sites. `pk`/`sig` are new and carry
the signature; see §5 for what they do and do not prove.

### Encoding

`canonical-JSON → UTF-8 → base64url (no padding)`, with a recognisable prefix:

```
FEDSTAT2.eyJwYXlsb2FkIjp7InYiOjEsInN0dWR5Ijoi…sInNpZyI6ImY0QTEifX0
└──┬───┘ └──────────────── base64url(signed envelope) ──────────────┘
 prefix
```

- **`FEDSTAT2.` prefix** — version-2 (signed) format; lets the launcher reject a
  wrong paste instantly and signals the format version to a human. A truncated
  paste fails signature verification, so we no longer need a separate CRC — the
  signature *is* the integrity check (and a real one, not just typo detection).
- **base64url** — URL/email-safe, survives copy-paste, no `+ / =` to mangle.
- **canonical JSON** — the payload is serialised with sorted keys and no
  insignificant whitespace before signing, so the site recomputes the exact same
  bytes to verify the signature.

### What the user sees & how long

A single opaque blob in a read-only box with a **Copy** button — visually like a
JWT. The signature (~64 bytes) and public key (~32 bytes) add ~130 base64 chars
over the unsigned design. Estimated length: signed envelope ~320–360 bytes →
base64url ~430–480 chars + prefix ≈ **~440–490 characters**. Still a single
copy-paste blob; longer than a JWT but well within what email/chat carry intact.

> **Recommendation:** one long copy-paste string, *not* a short human-typeable
> code. Short codes (e.g. `JOIN-4F9K`) require a lookup server to exchange the
> code for the real bundle — that's Option C infra we rejected. QR rendering is a
> nice future affordance but **out of scope** for Phase 2.

---

## 2. Registration flow

### Sequence (happy path)

```
COORDINATOR                              SITE
───────────                              ────
1. Operator clicks "Invite a site",
   optionally types a name.
2. App mints sid + per-site token,
   signs the payload with its Ed25519
   key, writes registry row {sid, name,
   token, study, invite_state:"issued"},
   renders the invite string.
3. Operator sends invite to the site
   (email/chat — out of band, one time).
                                         4. Operator pastes invite, clicks "Join".
                                         5. Launcher decodes, verifies prefix +
                                            Ed25519 signature + exp; pins coord pk
                                            (TOFU; warns if it changed — §5);
                                            shows "Join study 'SweSpine 2026'?"
                                         6. On confirm: generates its own Ed25519
                                            keypair (first launch) and saves
                                            site_config.json (study, coord, sid,
                                            token, coord pk, site keypair).
                                         7. Starts local api_server with
                                            FED_TOKEN = token (per-site).
                                         8. Resolves own Tailscale addr + port.
        ◀──── POST /register ──────────  9. POSTs {sid, name, site_addr, site_pk},
        {Authorization: Bearer <tok>}       body signed with the site private key;
                                            Authorization: Bearer <tok>.
10. Registrar (rate-limited) checks tok
    matches a registerable invite for sid
    (issued/in_use, not expired/revoked),
    records site_addr + registered_at,
    transitions issued→in_use→consumed
    ONLY on success, returns 200. ───────▶ 11. Site shows "Registered with
                                               coordinator ✓ — server running."
12. Shiny timer reads the registry file,
    the site appears in the list.
13. (Existing) /health ping confirms the
    site answers → status:"connected".
```

- **HTTP calls added:** exactly **one new call** — `POST /register`,
  site → coordinator (call #9). Every existing call (`/health`, `/grad_hess`,
  `/lm_suffstats`, …) is unchanged and still coordinator → site.
- **How the coordinator knows it worked:** the registrar subprocess writes the
  registry JSON; the Shiny app's `reactiveTimer` picks up the change and renders
  the new row. The site appearing in the list *is* the confirmation. "Is it
  actually answering queries" is then confirmed by the existing `/health` ping
  (manual **Refresh** button in Phase 2; live badges are Phase 3).
- **What the site stores & persists:** a `site_config.json` (study, coord addr,
  sid, token, **pinned coordinator public key**, and the **site's own Ed25519
  keypair** generated on first launch — see §5a) at a fixed path (proposed
  `engine/site/site_config.json`, or `data/.fedsite.json`). On relaunch the
  launcher reads it, so the operator **never re-pastes** — it re-uses the same
  identity, restarts the server, and re-registers (idempotent re-registration;
  see §5a + §6). This file holds a token → **gitignored, permission-hardened, and
  flagged sensitive** (same treatment as the coordinator registry; see §3).

---

## 3. Coordinator-side state

### Where to store the registry

| Option | Survives restart | Human-readable | Dependency | Verdict |
|--------|------------------|----------------|------------|---------|
| In-memory (reactiveValues only) | ❌ loses tokens on restart | n/a | none | Rejected — tokens *must* persist |
| **Local JSON file** | ✅ | ✅ | none | **Recommended** |
| Embedded SQLite | ✅ | ❌ (needs a tool) | `RSQLite` | Overkill for a handful of sites |

> **Recommendation: a single JSON file**, e.g.
> `engine/coordinator/registered_sites.json`. Site counts are tiny (a handful),
> the file is inspectable by a worried IT person, it needs no new package, and it
> survives restarts — which is mandatory because **the per-site tokens live
> here.** SQLite buys us nothing at this scale. It must be **gitignored** and
> documented as sensitive (contains tokens).

### File hardening — tokens are secrets *(Q2)*

Both the registry and the coordinator's **Ed25519 private key file** (see §5)
hold secrets, so on write we restrict them to the current user:

- **Unix:** `Sys.chmod(path, "600")` immediately after creating/writing the file.
- **Windows:** reset inheritance and grant only the current user via
  `icacls "<path>" /inheritance:r /grant:r "%USERNAME%:F"`.
- **Fallback / belt-and-braces:** on startup, check the file's mode; if it is
  group/world-readable (or, on Windows, the ACL grants more than the owner),
  **log a clear warning** ("`registered_sites.json` is readable by other users —
  it contains site tokens"). If cross-platform ACL handling proves messy, the
  warning is the guaranteed-portable floor; the `chmod`/`icacls` is best-effort
  on top. The site's `site_config.json` gets the identical treatment.

### Per-site record

```jsonc
{
  "sid":           "s_7f3a9c2e1b",
  "name":          "Karolinska",
  "study":         "SweSpine 2026",
  "token":         "K7zQ4m…",
  "site_addr":     "http://100.86.16.91:8000",  // null until registered
  "invite_state":  "consumed",    // issued | in_use | consumed | expired | revoked (§5a)
  "conn_status":   "connected",   // connected | stale  (display only; from /health ping)
  "created_at":    1749427200,
  "registered_at": 1749427810,    // null until the site calls home
  "last_seen":     1749427999     // updated by /health ping
}
```

`invite_state` is the authoritative lifecycle field the registrar enforces
(§5a). `conn_status` is a *display-only* reachability hint from the existing
`/health` ping — and remains a static, on-refresh value in Phase 2 (live badges
are Phase 3). A separate **revocation list** (set of revoked tokens/sids) is
persisted alongside the registry and consulted on every `/register` (§5).

### GUI display

Replace the free-text "Site URLs" textarea with a **registered-sites table**
(Name · Address · Status) plus:

- an **"Invite a site"** button → opens the invite string + Copy,
- an **"Add site manually"** escape hatch (URL + token) for backward compat /
  air-gapped cases,
- a per-row **"Revoke and Remove"** action (§5 — revokes the token, then drops
  the row),
- the existing Ping / Validate / Run buttons, now operating on the **registry**
  rather than typed text.

Status in Phase 2 is a static label refreshed on Ping/Refresh; **live badges are
Phase 3.**

---

## 4. Per-site tokens vs. the shared token

### Which token goes to which site

The coordinator no longer sends one token to everyone. At analysis time it builds
one remote server **per site, with that site's own token** from the registry:

```r
# today (coordinator_app.R): one shared token for all
ss <- lapply(urls, function(u) create_remote_server(u, tok))

# Phase 2: per-site token from the registry
ss <- lapply(sites, function(s) create_remote_server(s$site_addr, s$token))
```

`create_remote_server(base_url, token)` **already accepts a per-call token**, so
this is a small change in the coordinator app — the `fedstats` package and the
wire protocol need **no change at all**.

### Backward compatibility (a pre-Phase-2 site must keep working)

> **Later removed:** this backward-compat path (the site's manual Token field
> and the coordinator's "Add site manually") was removed on `alice/phase3-polish`
> after live multi-machine testing confirmed the invite flow alone works
> end-to-end — the app is invite-only now. Kept below as the historical
> rationale for why it was designed this way originally.

The crucial enabler: **`api_server.R` is mode-agnostic.** It just compares
whatever token it was given (`check_token()` is unchanged from Phase 1). A
per-site token and the old shared token are indistinguishable to the server — it
only ever checks "does the bearer match *my* `FED_TOKEN`."

So backward compatibility is essentially **free at the protocol layer** and lives
entirely in the GUI:

- **Old shared-token site:** started the old way (token typed into the site GUI
  or `FED_TOKEN` env var), no `site_config.json`. The coordinator reaches it via
  **"Add site manually"** with the shared token. Works exactly as today.
- **New per-site site:** launched from an invite, has a `site_config.json` with
  `sid` + per-site token, registered itself automatically.

### How the *site* knows which mode it's in

Presence of `site_config.json` (or an invite paste) ⇒ per-site mode. Absence ⇒
legacy mode (operator types a token / sets the env var, as today). The api_server
doesn't care either way. We will keep the legacy path fully functional — this is
a Phase 2 *Definition of Done* item.

---

## 5. Security considerations

### Blast radius if an invite leaks (forwarded email, screenshot)

An invite reveals the coordinator's tailnet address + one **per-site** token.
What an attacker can do is bounded by two gates:

1. **Tailscale membership.** Both coordinator and site live on the WireGuard
   tailnet. An attacker who is *not* on the tailnet cannot reach either endpoint,
   leaked invite or not. This is the dominant control.
2. **Single token, single site.** The leaked token authenticates traffic for
   **one** site only. With per-site tokens, one leaked invite ≠ compromise of the
   whole study. (This is itself a security **improvement** over Phase 1's single
   shared token — worth stating in the README.)

If an on-tailnet attacker holds a leaked, still-valid invite they could (a) race
to `POST /register` a bogus `site_addr` so the coordinator queries the wrong
host, or (b) if they can reach the real site, present the token and pull
aggregates (still floored by `min_n`). Mitigations below shrink both.

### Expiry / single-use / revocable

> **Recommendation: implement all three.** They directly bound the leak window.
- **Expiry (configurable).** `exp` field, default **+7 days**, but the default is
  **not hardcoded** — it is read from `FED_INVITE_TTL_DAYS` (env var) with a
  coordinator-settable override, so a study can shorten it (e.g. 1 day) or
  lengthen it as policy requires. *(Q4a)* The launcher refuses an expired invite
  with a clear message; the registrar independently rejects an expired invite at
  `/register` (never trust only the client-side check).
- **Single-use, success-gated.** An invite onboards exactly **one** site
  identity, and is marked consumed **only after a successful registration
  callback** — not on a mere attempt. The exact transitions are in §5a (this is
  your P2). A failed attempt (network blip, error) leaves the invite reusable so
  the site can retry without bothering the coordinator.
- **Revoke and Remove (not just hide).** *(Q4b)* "Remove" is **"Revoke and
  Remove"**: the token/sid is added to a persisted **revocation list** that is
  checked on **every** `/register` request. Effects:
  - a **revoked invite cannot register** (rejected at `/register`), and
  - a **previously-registered site cannot continue to call in** — because every
    re-registration / heartbeat also passes through the revocation check, a
    removed site is locked out, not merely hidden from the table.
  - "Hidden-but-still-valid" is explicitly rejected as a security model. The
    revocation list is the source of truth; the table row is just a view of it.
  - *Note on scope:* the revocation list governs **inbound** calls to the
    coordinator (`/register`). Outbound analysis queries already stop the moment
    the coordinator drops the site from its query set, so there is nothing
    further to revoke on the site side — the per-site token simply stops being
    used.

### Signing — adopted for day one *(P1)*

You asked me to either implement Ed25519 signing now or argue the case for
deferring. **I'm adopting signing for Phase 2**, with one honest caveat about
what it does and does not buy, because shipping it without that caveat would be
false assurance.

**What we implement:**
- The coordinator generates a long-lived **Ed25519 keypair once**, persisting the
  private key at `engine/coordinator/coordinator_key.json` (secret →
  permission-hardened per §3, gitignored). `sodium` (already in the project's
  install list in `network instructions.txt`) provides Ed25519, so no new
  dependency.
- Every invite carries `pk` (public key) and `sig` (signature over
  canonical(`payload`)). The launcher verifies `sig` against `pk` **before**
  acting on the invite — a partial paste, a corrupted blob, or a field flipped in
  transit all fail verification and are refused.
- The registrar **also** re-verifies the signature at `/register`, on top of the
  token check, so a forged registration without a valid signature is rejected
  server-side too.

**The honest caveat — why `pk` in the invite is necessary but not sufficient.**
Because the public key travels *inside* the invite, an attacker who can replace
the **entire** invite on the out-of-band channel (the email/chat used to send it)
can also substitute their own `pk` and sign their forgery. So an embedded-key
signature, *alone*, proves the invite is internally consistent and untampered
**after** signing — it does **not**, by itself, prove the invite came from the
legitimate coordinator on first contact. To close that gap we add a real trust
anchor:

- **TOFU pinning.** On first join, the site pins the coordinator's `pk` into
  `site_config.json`. Every later invite/interaction from that coordinator must
  present the **same** key; a changed key raises a prominent warning rather than
  silently trusting it. This defeats *subsequent* impersonation.
- **Fingerprint verification (recommended, low-friction).** The coordinator
  displays a short `pk` fingerprint (e.g. 8 hex groups). An operator who reads it
  out-of-band (a phone call, an existing trusted channel) and compares it on the
  launcher's confirm screen gets *first-contact* authenticity too. This stays
  optional so it adds no mandatory step, but it is the only thing that fully
  achieves "the site can tell a real invite from a forged one in transit."

Net: signing + TOFU pinning gives strong protection against tampering and
follow-on impersonation for near-zero cost; the optional fingerprint check closes
the first-contact MITM. This is strictly better than the unsigned design and I'm
glad you pushed for it. The residual first-contact risk is documented, not
hand-waved.

---

## 5a. Invite lifecycle state machine *(P2)*

The authoritative `invite_state` per invite, enforced by the registrar:

```
            mint
              │
              ▼
        ┌──────────┐  valid attempt arrives          ┌──────────┐
        │  issued  │ ──(token+sig+not exp/revoked)──▶ │  in_use  │
        └──────────┘   (re-entrant: retries OK)       └──────────┘
            │  │                                          │   │
   exp ≤ now│  │revoke                       success      │   │ failure
            │  │                        (addr stored,     │   │ (blip/error):
            ▼  ▼                         200 returned)    ▼   │ stays in_use,
        ┌─────────┐   ┌──────────┐                ┌──────────┐│ retry allowed
        │ expired │   │ revoked  │◀────revoke──── │ consumed │◀┘
        └─────────┘   └──────────┘  (any state)   └──────────┘
                          ▲                            │
                          └──────────revoke────────────┘
```

| State | Meaning | Can register? |
|-------|---------|---------------|
| **issued** | Minted, never successfully used. | Yes |
| **in_use** | A valid registration attempt is underway but not yet confirmed. Soft, **re-entrant** — repeated attempts are allowed so a retry after a blip works. | Yes |
| **consumed** | A registration **succeeded** (site_addr stored, 200 returned). | Only re-registration by the *same* identity (see below) |
| **expired** | `exp ≤ now`. | No (re-issue required) |
| **revoked** | Coordinator ran Revoke and Remove; token on the revocation list. | No (terminal) |

**Transitions and what each requires:**

1. **mint → issued** — coordinator generates the invite.
2. **issued → in_use** — a `/register` arrives passing *all* gates: token matches
   this `sid`, signature valid, `now < exp`, not revoked, within rate limit. This
   state is re-entrant: a second attempt while `in_use` is fine (idempotent).
   **On first entry to `in_use`, the registrar records the requesting site's
   identity** — its `site_addr` **and** its `site_pk` (see "Requester identity"
   below) — and pins them to the invite. That recorded pair is what every later
   attempt is compared against.
3. **in_use → consumed** — the registrar successfully persists `site_addr` and
   returns 200, and only then. **This is the sole path to `consumed`** — a failed
   or interrupted attempt does **not** consume the invite (directly answering
   P2's network-blip concern).
4. **in_use → in_use (stay)** — attempt failed mid-flight (timeout, write error,
   site didn't ack). Invite remains usable; **a retry from the recorded identity
   (same `site_addr` + same `site_pk`, valid signature) is treated as the same
   registration completing and is allowed to proceed to `consumed`.** A
   concurrent/subsequent attempt from a *different* identity is held for approval
   (see below), not silently accepted.
5. **issued / in_use → expired** — wall-clock reaches `exp`. Checked on each
   request and lazily on registry load.
6. **any → revoked** — coordinator clicks Revoke and Remove. Terminal; the token
   is added to the revocation list and checked on every subsequent request.

**Requester identity (`site_addr` + `site_pk`).** To make "same host" verifiable
rather than guessable, the site generates its **own** Ed25519 keypair on first
launch, persists it in `site_config.json` (private key → hardened per §3), and on
every `/register` sends its **public key** (`site_pk`) and signs the request body
with the matching private key. The registrar verifies that signature and records
`(site_addr, site_pk)` on entry to `in_use`. A leaked invite alone (which carries
only the *token*, not the site's private key) therefore cannot silently complete
or take over a registration — the original host holds a key the attacker does
not. This is the cryptographic backing for the retry-vs-collision rule.

**Retry semantics during `in_use` (this is the clarified rule).**
- **Same identity = same registration completing.** A retry whose `site_addr`
  **and** `site_pk` match the recorded pair (and whose request signature verifies)
  is treated as the original registration finishing after a network drop. It is
  allowed: `in_use → consumed`. The operator sees success, not an error — no need
  to contact the coordinator.
- **Different identity = potential leaked-invite collision.** An attempt during
  `in_use` (or after `consumed`) whose `site_addr` **or** `site_pk` differs from
  the recorded pair is **not** auto-accepted. It is surfaced in the coordinator
  UI as an explicit approval prompt ("a different host is trying to register with
  this invite — addr X, key fingerprint Y. Approve?"). Only on coordinator
  approval is the recorded identity replaced; otherwise the attempt is refused.

**`consumed` and legitimate re-registration (restart).** A consumed invite still
lets the **same** site re-announce after a restart, because that's required by
§2's "never re-paste" goal. The same identity rule applies:
- Re-registration with the recorded `site_addr` + `site_pk` (valid signature) →
  accepted as an idempotent heartbeat; updates `last_seen`. Stays `consumed`.
- Re-registration with the same token but a **different `site_addr` or
  `site_pk`** → held for coordinator approval, exactly as the collision case
  above. This distinguishes a legitimate Tailscale-IP change *from the same host*
  (same `site_pk`, new address — still prompts, but the matching key tells the
  coordinator it's the real site) from a second host replaying a leaked invite
  (different key), without ever silently locking out or silently admitting
  either.

---

## 6. Failure modes

| Scenario | Behaviour |
|----------|-----------|
| **Invite from a different study pasted by mistake** | Launcher shows `study` on the confirm step ("Join study 'SweSpine 2026'?") so a mismatch is visible. The registration also can't succeed against the wrong coordinator — that coordinator never issued this `sid`, so `POST /register` is rejected. |
| **Two sites paste the same invite** | Invites are per-site (unique `sid` + token). The first to register successfully **consumes** the invite (§5a). A *second host* then presents the same token from a **different `site_addr`**, which is **not** auto-accepted — it surfaces in the coordinator UI as a new-address approval prompt, so two hosts cannot silently collide on one identity. (A genuine restart from the same address is accepted as an idempotent heartbeat.) |
| **Coordinator address in the invite is stale (Tailscale IP changed)** | `POST /register` fails (refused/timeout). Site shows "Couldn't reach the coordinator at `<addr>` — ask for a fresh invite." **Mitigation (Q3, adopted):** the invite's `coord` field prefers the coordinator's **MagicDNS hostname** (e.g. `coordinator.tailnet.ts.net`), falling back to the raw `100.x` IP only when MagicDNS isn't resolvable — the hostname survives IP changes, so outstanding invites keep working across a coordinator IP change. Friendly errors + reactive IP land in Phase 3. |

---

## 7. Out of scope for Phase 2 (confirming)

- **Live status badges** on the registered-sites list → Phase 3.
- **Reactive Tailscale IP refresh** (the frozen-at-startup `.ts_ip`) → Phase 3.
- **Any change to the analysis flow itself** (templates, `fedstats` stats).
- Additionally treated as out of scope here:
  - **The Phase 1 wire protocol / `check_token()` / `api_server.R` statistical
    endpoints** — unchanged. Phase 2 adds the GUI flow plus one new `/register`
    endpoint (on the coordinator's registrar) and invite signing; it does not
    touch the existing site endpoints or their auth.
  - **Mandatory fingerprint verification** — the out-of-band `pk` fingerprint
    check (§5) stays *optional* in Phase 2; we don't force it into the flow.
  - **QR codes**, short codes, and any **hosted rendezvous/lookup server**.

> **Now in scope (changed from Rev 1):** invite **signing** (Ed25519, §5) is
> implemented in Phase 2, not deferred.

---

## Decisions locked (this revision)

| # | Decision | Detail |
|---|----------|--------|
| §0 / Q1 | **Registrar listener approved** | Minimal `/register` plumber subprocess on the coordinator + JSON file polled by the Shiny app. |
| Q1a | **Authenticated registration** | `/register` requires a valid invite token (Bearer) **and** a valid signature; no unauthenticated registration. |
| Q1b | **Tailscale-bound registrar** | Reuses Phase 1's `api_server.R` binding logic, factored into one shared helper (no reinvention). |
| Q1c | **Rate-limited registration** | Per-source token bucket; `429` on excess; logged. |
| Q2 | **Registry = gitignored JSON, hardened** | `chmod 600` / `icacls` owner-only; startup warning if world-readable. Same for `site_config.json` and the coordinator private key. |
| Q3 | **MagicDNS preferred** | `coord` uses `*.ts.net` hostname when resolvable, raw `100.x` IP as fallback. |
| Q4a | **Configurable expiry** | `FED_INVITE_TTL_DAYS` (default 7), not hardcoded. |
| Q4b | **Revoke and Remove** | Token added to a persisted revocation list, checked on every `/register`; revoked invites can't register and removed sites can't call in. |
| Q5 | **Registrar port 8731** | Tailscale-bound; **documented in the README** so hospital IT knows what to expect. |
| P1 | **Signed invites from day one** | Ed25519 via `sodium`; `pk`+`sig` in the invite; site verifies before acting; TOFU key pinning + optional fingerprint check (§5). |
| P2 | **Success-gated single-use** | `consumed` only after a successful callback; full state machine in §5a. Registrar records requester identity `(site_addr + site_pk)` on entry to `in_use`; same-identity retries complete, different-identity attempts need coordinator approval. |

## Build order (once you approve this revision)

Per PLAN.md "one commit per logical step," I'd implement in this sequence — each
step independently testable, happy path never broken:

1. **Crypto + format helpers** (`fedstats`): Ed25519 keygen/sign/verify,
   canonical-JSON, invite encode/decode, the shared Tailscale-bind helper. Pure
   functions, unit-testable with no GUI.
2. **Coordinator registry + registrar subprocess**: JSON store, file hardening,
   `/register` with auth + rate limit + state machine + revocation list.
3. **Coordinator GUI**: Invite-a-site, registered-sites table, Revoke and Remove,
   per-site tokens wired into `create_remote_server`, manual-add escape hatch.
4. **Site launcher**: invite paste → verify → pin → save `site_config.json` →
   start server → register; persistence + re-registration on restart.
5. **Backward-compat pass + README/port docs + `PHASE2_TESTING.md`**.

No code is written until you approve this revision.
