#!/bin/bash
# ============================================================
#   Federated Statistics — Site Server  (macOS)
#   Double-click this file in Finder to start your server.
# ============================================================
#
#   What this does:
#     • Starts a secure local server that shares aggregate
#       statistics from your registry data with the coordinator.
#     • No individual patient records ever leave this computer —
#       only counts, means, and model summaries are transmitted.
#     • The server runs only while this window is open.
#
#   If you see "permission denied" when double-clicking:
#     Open Terminal, paste this, and press Enter:
#       chmod +x ~/path/to/mac/Start\ Site.command
# ============================================================

# Go to project root (parent of this mac/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo ""
echo "╔══════════════════════════════════════════════╗"
echo "║     Federated Statistics — Site Server       ║"
echo "╚══════════════════════════════════════════════╝"
echo ""
echo "  No individual patient records will leave this computer."
echo "  Keep this window open for the duration of the analysis."
echo ""

# ── Step 1 of 3: Check R ────────────────────────────────────
echo "[ Step 1 of 3 ]  Checking R..."

if ! command -v Rscript &>/dev/null; then
  echo ""
  echo "  ✗  R is not installed (or not found on your PATH)."
  echo ""
  echo "     Install R in one of two ways:"
  echo "       Option 1 — Homebrew (if installed):  brew install r"
  echo "       Option 2 — Download the installer from CRAN:"
  echo "                  https://cran.r-project.org/bin/macosx/"
  echo ""
  read -rp "  Press Enter to open the download page in your browser..."
  open "https://cran.r-project.org/bin/macosx/"
  echo "  After installing R, close this window and double-click this file again."
  read -rp "  Press Enter to close..."
  exit 1
fi

echo "  ✓  $(Rscript --version 2>&1 | head -1)"
echo ""

# ── Step 2 of 3: R packages ─────────────────────────────────
echo "[ Step 2 of 3 ]  Checking required R packages..."

Rscript engine/setup.R site
if [ $? -ne 0 ]; then
  echo ""
  echo "  ✗  Package setup failed. See errors above."
  read -rp "  Press Enter to close this window..."
  exit 1
fi
echo ""

# ── Step 3 of 3: Tailscale ──────────────────────────────────
echo "[ Step 3 of 3 ]  Checking Tailscale (secure network connection)..."

if ! command -v tailscale &>/dev/null; then
  echo ""
  echo "  ✗  Tailscale is not installed on this computer."
  echo ""
  echo "     Tailscale is what lets your computer connect securely to"
  echo "     the study coordinator — it's required before you can join."
  echo ""
  echo "     Download it from:  https://tailscale.com/download"
  echo ""
  read -rp "  Press Enter to open the download page in your browser..."
  open "https://tailscale.com/download"
  echo "  After installing, sign in and accept the study's network invite"
  echo "  (sent to you separately), then close this window and double-click"
  echo "  this file again."
  read -rp "  Press Enter to close..."
  exit 1
fi

TAILSCALE_IP=$(tailscale ip -4 2>/dev/null | head -1)

if [ -n "$TAILSCALE_IP" ]; then
  echo "  ✓  Tailscale connected.  Your IP: $TAILSCALE_IP"
  echo "     Your address will be shown in the browser interface."
else
  echo ""
  echo "  ⚠  Tailscale is installed but not connected."
  echo "     Open the Tailscale app, sign in, and make sure you've"
  echo "     accepted the study's network invite."
  echo ""
  read -rp "  Press Enter to continue anyway..."
fi
echo ""

# ── Launch GUI ──────────────────────────────────────────────
echo "══════════════════════════════════════════════════════"
echo "  Opening site server interface in your browser..."
echo ""
echo "  • Select your data file and configure the server."
echo "  • Click 'Start Server' when ready."
echo "  • Keep this window open — closing it stops the server."
echo "══════════════════════════════════════════════════════"
echo ""

Rscript -e "shiny::runApp('engine/site/site_app.R', launch.browser = TRUE)"

echo ""
echo "  Interface closed."
read -rp "  Press Enter to close this window..."
