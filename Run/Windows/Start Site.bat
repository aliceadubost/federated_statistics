@echo off
setlocal enabledelayedexpansion
title Federated Statistics — Site Server

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
echo  ^|   Federated Statistics — Site Server       ^|
echo  +--------------------------------------------+
echo.
echo   No individual patient records will leave this computer.
echo   Keep this window open for the duration of the analysis.
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
"!RSCRIPT!" engine\setup.R site
if !errorlevel! neq 0 (
    echo.
    echo   X  Package setup failed. See errors above.
    pause & exit /b 1
)
echo.

:: ── Step 3 of 3: Tailscale ───────────────────────────────────
echo [ Step 3 of 3 ]  Checking Tailscale (secure network connection)...

where tailscale >nul 2>&1
if !errorlevel! neq 0 (
    echo.
    echo   X  Tailscale is not installed on this computer.
    echo.
    echo      Tailscale is what lets your computer connect securely to
    echo      the study coordinator - it's required before you can join.
    echo.
    echo      Please download and install it from:
    echo        https://tailscale.com/download
    echo.
    echo      After installing, sign in and accept the study's network
    echo      invite (sent to you separately), then close this window
    echo      and double-click this file again.
    echo.
    start https://tailscale.com/download
    pause
    exit /b 1
)

set "TAILSCALE_IP="
for /f "tokens=* usebackq" %%I in (`tailscale ip -4 2^>nul`) do (
    set "TAILSCALE_IP=%%I" & goto :ts_done
)
:ts_done

if not "!TAILSCALE_IP!"=="" (
    echo   OK  Tailscale connected.  Your IP: !TAILSCALE_IP!
    echo       Your address will be shown in the browser interface.
) else (
    echo.
    echo   ^!  Tailscale is installed but not connected.
    echo       Open the Tailscale app, sign in, and make sure you've
    echo       accepted the study's network invite.
    echo.
    set /p "CONT=   Press Enter to continue anyway..."
)
echo.

:: ── Launch GUI ───────────────────────────────────────────────
echo  +------------------------------------------------------+
echo  ^|  Opening site server interface in your browser...   ^|
echo  ^|                                                     ^|
echo  ^|  * Select your data file and configure the server.  ^|
echo  ^|  * Click 'Start Server' when ready.                 ^|
echo  ^|  * Keep this window open during the analysis.       ^|
echo  ^|    Closing it stops the server.                     ^|
echo  +------------------------------------------------------+
echo.

"!RSCRIPT!" -e "shiny::runApp('engine/site/site_app.R', launch.browser = TRUE)"

echo.
echo   Interface closed.
pause
