# Federated Statistics — Site Setup (Quick Start)

**What this is:** you'll run a small program that lets a study coordinator
compute statistics across several hospitals — **without any patient data ever
leaving your computer**. Only aggregate numbers (counts, sums, averages) are
shared, never individual patient records.

You do **not** need to know anything technical. Follow the steps below.

---

## Before you start, you need two things

1. **Tailscale** installed and signed in. This is the secure private network
   the hospitals use to reach each other. Get it from **https://tailscale.com/download**,
   install it, and sign in (your coordinator will have added you, or use your
   organisation's account).
2. **Your data as a CSV file** — one row per patient, with the columns the
   study asked for.

---

## Steps

1. **Unzip** this kit somewhere easy to find (e.g. your Desktop).

2. **Open the "Start Site" file for your computer:**
   - Windows → `Run/Windows/Start Site.bat`
   - Mac → `Run/Mac/Start Site.command`
   - Linux → `Run/Linux/Start Site.sh`

   The first time, it installs what's needed (this can take a few minutes).
   Then a page opens in your web browser. Keep the small black window open —
   closing it stops the program.

3. In the page, **paste the invite link** the coordinator sent you, click
   **Join**, and confirm the study details look right.

4. **Choose your data file** — pick it from the list, or click **Browse…**.

5. Click **Start Server** and wait for **“✓ You're connected.”**

6. **Keep this window open** during the study session. You can minimise it.
   When the study is done, click **Stop Server** or just close the window.

---

## If something looks wrong

- **“Tailscale not detected.”** Open the Tailscale app and sign in; the
  warning clears by itself once you're connected.
- **“This invite has expired.”** Ask the coordinator for a fresh invite link.
- **“Intended site” is not you.** You were sent the wrong invite — don't join;
  tell the coordinator.
- **The window closed by itself with an error.** Re-open the “Start Site”
  file; if it keeps failing, send the coordinator what the window said.

---

**Your privacy is protected by design:** the coordinator can only ask for
aggregate statistics, subgroups too small to be safe are automatically
withheld, and you can see every request and stop the server at any time.
