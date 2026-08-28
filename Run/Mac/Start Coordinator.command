#!/bin/bash
# ============================================================
#   Federated Statistics — Coordinator  (macOS)
#   Double-click this file in Finder to open the interface.
# ============================================================
#
#   What this does:
#     • Opens the analysis coordinator in your web browser.
#     • You will enter the addresses of all participating sites,
#       then run the federated analysis from the browser window.
#     • Make sure every site has already started their server
#       before you click "Run Analysis".
#
#   If you see "permission denied" when double-clicking:
#     Open Terminal, paste this, and press Enter:
#       chmod +x ~/path/to/mac/Start\ Coordinator.command
# ============================================================

# Go to project root (parent of this mac/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

close_terminal_window() {
  if [ "${TERM_PROGRAM:-}" = "Apple_Terminal" ] && command -v osascript &>/dev/null; then
    osascript >/dev/null 2>&1 <<'APPLESCRIPT' &
tell application "Terminal"
  if (count of windows) > 0 then close front window saving no
end tell
APPLESCRIPT
  fi
}

echo ""
echo "╔══════════════════════════════════════════════╗"
echo "║   Federated Statistics — Coordinator         ║"
echo "╚══════════════════════════════════════════════╝"
echo ""
echo "  Before continuing, make sure:"
echo "    • Tailscale is connected (check the menu bar icon)"
echo "    • All site operators have started their servers and"
echo "      sent you their addresses (http://100.x.x.x:8000)"
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

Rscript engine/setup.R coordinator
if [ $? -ne 0 ]; then
  echo ""
  echo "  ✗  Package setup failed. See errors above."
  read -rp "  Press Enter to close this window..."
  exit 1
fi
echo ""

# ── Step 3 of 3: Tailscale ──────────────────────────────────
echo "[ Step 3 of 3 ]  Checking Tailscale..."

MY_IP=""
if command -v tailscale &>/dev/null; then
  MY_IP=$(tailscale ip -4 2>/dev/null | head -1)
fi

if [ -n "$MY_IP" ]; then
  echo "  ✓  Tailscale connected.  Your IP: $MY_IP"
else
  echo ""
  echo "  ⚠  Tailscale is not running or not connected."
  echo "     Open the Tailscale app and connect, then close this window"
  echo "     and double-click this file again."
  echo "     Download Tailscale:  https://tailscale.com/download"
  echo ""
  read -rp "  Press Enter to continue anyway..."
fi
echo ""

# ── Launch ──────────────────────────────────────────────────
echo "══════════════════════════════════════════════════════"
echo "  Starting coordinator interface..."
echo ""
echo "  Your browser will open automatically in a few seconds."
echo "  If it doesn't open, look for a URL in the output below"
echo "  (starts with  http://127.0.0.1:...)  and paste it into"
echo "  your browser manually."
echo ""
echo "  Keep this window open while running the analysis."
echo "  To stop: close this window or press Ctrl+C."
echo "══════════════════════════════════════════════════════"
echo ""

Rscript -e "shiny::runApp('engine/coordinator/coordinator_app.R', launch.browser = TRUE)"
APP_STATUS=$?

echo ""
echo "  Coordinator stopped."
if [ $APP_STATUS -eq 0 ]; then
  close_terminal_window
fi
exit $APP_STATUS
