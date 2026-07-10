# Federated Statistics

Run statistical analyses across multiple hospital databases **without any patient data ever leaving each hospital**.

---

## The idea in one sentence

Each hospital keeps its own data. This tool sends the *question* to each hospital, gets back only summary numbers (totals, averages, model outputs), and combines those summaries into a final result, exactly as if you had run the analysis on all the data together.

---

## Who does what

There are two roles:

**Site** — a hospital or registry that holds patient data. They run the **Site** launcher once, which starts a small local server. That server answers statistical questions from the coordinator. No patient rows are ever transmitted.

**Coordinator** — the researcher who wants results. They run the **Coordinator** launcher, load an analysis script, and click Run. Results appear in the browser as tables and plots.

---

## What you need

- [R](https://cran.r-project.org) (version 4.1 or later)
- [Tailscale](https://tailscale.com), connected to the study's private network. Free, takes about 2 minutes to install. The coordinator must add you to their tailnet before anything else here will work.

The first launch installs everything else automatically.

---

## Getting started

### If you are a site (hospital), and this is your first time

You'll get one message from the coordinator with everything below — this
is just what each piece does.

1. **Install Tailscale**: [tailscale.com/download](https://tailscale.com/download), then accept the network invite the coordinator sends you (separately, via Tailscale itself) and sign in.
2. **Get the software**: once connected, open the link the coordinator sent you and click the download button. Unzip it anywhere.
3. Open the `Run` folder inside the unzipped kit and double-click **Start Site** for your operating system (Mac/Windows/Linux). It checks R and Tailscale for you and offers to install anything missing.
4. Paste the **study invite** into the app and click **Join** (confirm the study name and coordinator's key fingerprint).
5. Select your CSV data file and click **Start Server** — the port is chosen for you automatically.

Your site registers itself with the coordinator automatically — nothing to type or send by hand. You can stop the server at any time by clicking **Stop Server**.

### If you are a site and already have the software

Just double-click **Start Site**, paste the invite for the new study, **Join**, pick your data file, **Start Server**.

### If you are the coordinator (researcher)

1. Double-click **Start Coordinator** in the `Run/` folder — it starts a small registration listener on **port 8731** (Tailscale interface only) so sites can register back to you, and self-hosts a install kit for new site operators (see below).
2. Click **Browse** and select an analysis script.
3. For each site, click **Invite a site**. The "Invite created" dialog gives you a short link to send — or, for someone with nothing installed yet, expand **"First time?"** for one combined message covering Tailscale, the software, and the invite. Registered sites appear in the list automatically.
4. Click **Ping → Validate → Run Analysis**.
5. Results appear as tabs: tables, plots, and a console with key numbers.

#### Network ports

| Service | Port | Bound to |
|---------|------|----------|
| Site API server | chosen automatically (from 8000 up) | Tailscale interface |
| Coordinator registrar | `8731` (`FED_REGISTRAR_PORT`) | Tailscale interface |

Invites expire after 7 days by default (`FED_INVITE_TTL_DAYS`).

---

## Analysis scripts

Ready-to-use templates are in the `analysis/templates/` folder:

| File | What it does |
|------|-------------|
| `demo_descriptives.R` | Table 1: means, SDs, proportions |
| `demo_welch_t.R` | Compare a continuous variable between two groups |
| `demo_chisq.R` | Test association between two yes/no variables |
| `demo_linear_regression.R` | Predict a continuous outcome |
| `demo_logistic_regression.R` | Predict a yes/no outcome (odds ratios) |

Each template has `# ── ADAPT:` comments marking the lines you need to change for your own variables.

---

## Privacy model

**What the tool does:**
- No patient rows ever leave the site. Only aggregate statistics are transmitted: counts, sums, sums of squares, and model gradients.
- Every aggregate returned by a site is based on at least **`min_n` rows** (default: 20). Queries that would reveal statistics from fewer rows are refused with an error. This is enforced at the site server, not just at startup.
- Each site controls its own server. Site operators can see every query being made and stop the server at any time.
- **Per-site tokens.** Joining via an invite is the only onboarding path, and every invite carries its own unique token — so a leaked invite or token affects only that one site, never the whole study.
- **Signed invites.** Invites are signed with the coordinator's Ed25519 key; the site verifies the signature and pins the key on first use (and can verify a short key fingerprint out of band). The coordinator's registration listener accepts a registration only with a valid invite token *and* a valid signature, is rate-limited, and is bound to the Tailscale interface.
- Network traffic between sites and coordinator is encrypted by Tailscale (WireGuard). The API itself runs over plain HTTP within that encrypted tunnel.

**Known limitations:**
- A coordinator who issues many carefully chosen narrow queries could potentially infer information about small groups, even with `min_n` enforcement. This tool does not implement differential privacy. For analyses involving very sensitive subgroups, consult your institution's data governance team.
- The `min_n` threshold is a configurable floor, not a formal privacy guarantee. Higher values provide stronger protection at the cost of excluding sites with small datasets.
- The tool assumes the coordinator is a trusted researcher. It does not protect against a malicious coordinator who has legitimate access.

---

