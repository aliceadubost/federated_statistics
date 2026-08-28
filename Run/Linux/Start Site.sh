#!/bin/bash
# ============================================================
#   Federated Statistics — Site Server  (Linux)
#   Run:  bash "linux/Start Site.sh"
#   Or make it executable and double-click in your file manager.
# ============================================================
#
#   What this does:
#     • Starts a secure local server that shares aggregate
#       statistics from your registry data with the coordinator.
#     • No individual patient records ever leave this computer —
#       only counts, means, and model summaries are transmitted.
#     • The server runs only while this terminal is open.
# ============================================================

# Go to project root (parent of this linux/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo ""
echo "┌──────────────────────────────────────────────┐"
echo "│     Federated Statistics — Site Server       │"
echo "└──────────────────────────────────────────────┘"
echo ""
echo "  No individual patient records will leave this computer."
echo "  Keep this terminal open for the duration of the analysis."
echo ""

# ── Step 1 of 3: Check R ────────────────────────────────────
echo "[ Step 1 of 3 ]  Checking R..."

if ! command -v Rscript &>/dev/null; then
  echo ""
  echo "  ✗  R is not installed."
  echo ""
  echo "     Install R on Ubuntu/Debian:"
  echo "       sudo apt update && sudo apt install -y r-base r-base-dev"
  echo ""
  read -rp "  Would you like to install R now? (requires sudo) [y/N]: " INSTALL_R
  if [[ "$INSTALL_R" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y r-base r-base-dev
    if ! command -v Rscript &>/dev/null; then
      echo "  ✗  Installation failed. Please install R manually and re-run."
      exit 1
    fi
  else
    echo "  Please install R and re-run this script."
    exit 1
  fi
fi

echo "  ✓  $(Rscript --version 2>&1 | head -1)"
echo ""

# ── System libraries (Linux only) ───────────────────────────
echo "  Checking system libraries needed by R packages..."
MISSING_LIBS=()
for lib in libcurl4-openssl-dev libssl-dev libxml2-dev; do
  dpkg -l "$lib" &>/dev/null 2>&1 || MISSING_LIBS+=("$lib")
done

if [ ${#MISSING_LIBS[@]} -gt 0 ]; then
  echo "  Some libraries are missing: ${MISSING_LIBS[*]}"
  read -rp "  Install them now? (requires sudo) [y/N]: " INSTALL_LIBS
  if [[ "$INSTALL_LIBS" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y "${MISSING_LIBS[@]}"
  else
    echo "  Skipping — R package installation may fail without these."
  fi
else
  echo "  ✓  System libraries OK."
fi
echo ""

# ── Step 2 of 3: R packages ─────────────────────────────────
echo "[ Step 2 of 3 ]  Checking required R packages..."

Rscript engine/setup.R site
if [ $? -ne 0 ]; then
  echo ""
  echo "  ✗  Package setup failed. See errors above."
  read -rp "  Press Enter to close..."
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
  echo "     Install it:  curl -fsSL https://tailscale.com/install.sh | sh"
  echo "     Or download: https://tailscale.com/download/linux"
  echo ""
  echo "     After installing, run 'sudo tailscale up', sign in, and accept"
  echo "     the study's network invite (sent to you separately), then"
  echo "     re-run this script."
  echo ""
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
  echo "     Run:  sudo tailscale up"
  echo "     Then sign in and make sure you've accepted the study's"
  echo "     network invite."
  echo ""
  read -rp "  Press Enter to continue anyway..."
fi
echo ""

# ── Launch GUI ──────────────────────────────────────────────
echo "════════════════════════════════════════════════════════"
echo "  Opening site server interface in your browser..."
echo ""
echo "  • Select your data file and configure the server."
echo "  • Click 'Start Server' when ready."
echo "  • Keep this terminal open — closing it stops the server."
echo "════════════════════════════════════════════════════════"
echo ""

Rscript -e "shiny::runApp('engine/site/site_app.R', launch.browser = TRUE)"

echo ""
echo "  Interface closed."
exit 0
