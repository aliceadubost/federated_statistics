@echo off
setlocal enabledelayedexpansion
title Federated Statistics — Coordinator

:: Go to project root (parent of this windows\ folder)
cd /d "%~dp0..\.."

:: R on Windows refuses to start ("Erreur fatale : 'R_TempDir' contains
:: space") when TEMP/TMP contains a space — which happens whenever the
:: Windows user name has one (e.g. "C:\Users\Alice Alonso\..."). The usual
:: fix (switching to the short 8.3 path form) doesn't help on systems where
:: 8.3 name generation is disabled (the modern default), so use a fixed,
:: always-space-free temp folder instead.
set "TEMP=%SystemDrive%\fedstats_rtmp"
set "TMP=%SystemDrive%\fedstats_rtmp"
if not exist "%TEMP%" mkdir "%TEMP%" >nul 2>&1

echo.
echo  +--------------------------------------------+
echo  ^|   Federated Statistics — Coordinator       ^|
echo  +--------------------------------------------+
echo.
echo   Before continuing, make sure:
echo     - Tailscale is connected (check the system tray icon)
echo     - All site operators have started their servers and
echo       sent you their addresses (http://100.x.x.x:8000)
echo.

:: ── Step 1 of 3: Find R ─────────────────────────────────────
echo [ Step 1 of 3 ]  Checking R...

set "RSCRIPT="
where Rscript >nul 2>&1
if !errorlevel! equ 0 (
    set "RSCRIPT=Rscript"
    goto :r_found
)
for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
        set "RSCRIPT=%%D\bin\Rscript.exe"
        set "PATH=%%D\bin;!PATH!"
        goto :r_found
    )
)
for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
        set "RSCRIPT=%%D\bin\Rscript.exe"
        set "PATH=%%D\bin;!PATH!"
        goto :r_found
    )
)

echo.
echo   X  R is not installed on this computer.
echo.
echo      Please download and install R from:
echo        https://cran.r-project.org/bin/windows/base/
echo.
echo      After installing R, close this window and
echo      double-click this file again.
echo.
start https://cran.r-project.org/bin/windows/base/
pause
exit /b 1

:r_found
for /f "tokens=* usebackq" %%V in (`"!RSCRIPT!" --version 2^>^&1`) do (
    echo   OK  %%V & goto :r_ver_done
)
:r_ver_done
echo.

:: ── Step 2 of 3: R packages ──────────────────────────────────
echo [ Step 2 of 3 ]  Checking required R packages...
"!RSCRIPT!" engine\setup.R coordinator
if !errorlevel! neq 0 (
    echo.
    echo   X  Package setup failed. See errors above.
    pause & exit /b 1
)
echo.

:: ── Step 3 of 3: Tailscale ───────────────────────────────────
echo [ Step 3 of 3 ]  Checking Tailscale...

set "MY_IP="
for /f "tokens=* usebackq" %%I in (`tailscale ip -4 2^>nul`) do (
    set "MY_IP=%%I" & goto :ts_done
)
:ts_done

if not "!MY_IP!"=="" (
    echo   OK  Tailscale connected.  Your IP: !MY_IP!
) else (
    echo   ^!  Tailscale is not connected.
    echo       If site connections fail, open the Tailscale app and connect.
)
echo.

:: ── Launch ───────────────────────────────────────────────────
echo  +------------------------------------------------------+
echo  ^|  Starting coordinator interface...                  ^|
echo  ^|                                                     ^|
echo  ^|  Your browser will open automatically.              ^|
echo  ^|  If it doesn't, look for a URL like                 ^|
echo  ^|  http://127.0.0.1:XXXX in the output below          ^|
echo  ^|  and paste it into your browser manually.           ^|
echo  ^|                                                     ^|
echo  ^|  Keep this window open while running the analysis.  ^|
echo  ^|  Close this window to stop the coordinator.         ^|
echo  +------------------------------------------------------+
echo.

"!RSCRIPT!" -e "shiny::runApp('engine/coordinator/coordinator_app.R', launch.browser = TRUE)"

echo.
echo   Coordinator stopped.
pause
