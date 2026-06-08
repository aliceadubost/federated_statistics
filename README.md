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
- An internet connection between sites and coordinator (or [Tailscale](https://tailscale.com) for secure cross-institution VPN. A Tailscale account is free and takes 5 minutes to set up)

The first launch installs everything else automatically.

---

## Getting started

### If you are a site (hospital)

1. Open the `Run/Mac`, `Run/Windows`, or `Run/Linux` folder
2. Double-click **Start Site**
3. A browser window opens — select your CSV data file and click **Start Server**
4. Tell the coordinator your address (shown on screen, e.g. `http://100.x.x.x:8000`)

That's it. You can stop the server at any time by clicking **Stop Server**.

### If you are the coordinator (researcher)

1. Double-click **Start Coordinator** in the same `Run/` folder
2. A browser window opens — click **Browse** and select an analysis script
3. Paste the addresses of all participating sites
4. Click **Run Analysis**
5. Results appear as tabs: tables, plots, and a console with key numbers

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
- Token-based authentication (optional but recommended) ensures only the intended coordinator can query a site.
- Network traffic between sites and coordinator is encrypted by Tailscale (WireGuard). The API itself runs over plain HTTP within that encrypted tunnel.

**Known limitations:**
- A coordinator who issues many carefully chosen narrow queries could potentially infer information about small groups, even with `min_n` enforcement. This tool does not implement differential privacy. For analyses involving very sensitive subgroups, consult your institution's data governance team.
- The `min_n` threshold is a configurable floor, not a formal privacy guarantee. Higher values provide stronger protection at the cost of excluding sites with small datasets.
- The tool assumes the coordinator is a trusted researcher. It does not protect against a malicious coordinator who has legitimate access.

---

