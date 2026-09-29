@echo off
REM ============================================================
REM Alfalfa Pan-Genome Database - One-Click Launcher
REM ============================================================

REM --- Find and activate conda ---
set CONDA_BAT=C:\Users\Erick.Amombo\AppData\Local\miniconda3\condabin\conda.bat
if not exist "%CONDA_BAT%" (
    echo ERROR: Cannot find conda at %CONDA_BAT%
    echo Please verify your miniconda installation path.
    pause
    exit /b 1
)

call "%CONDA_BAT%" activate pg_db
if errorlevel 1 (
    echo ERROR: Failed to activate pg_db environment
    pause
    exit /b 1
)

echo.
echo ==============================================
echo   ALFALFA PAN-GENOME DATABASE
echo   Starting all services...
echo ==============================================
echo.

REM ---- 1. Start PostgreSQL ----
echo [1/4] Starting PostgreSQL...
pg_ctl -D "%USERPROFILE%\pg_data" -l "%USERPROFILE%\pg_logs\postgres.log" start >nul 2>&1
timeout /t 3 /nobreak >nul

REM ---- 2. Verify database is reachable ----
echo [2/4] Verifying database...
psql -U postgres -p 5433 -d alfalfa_pangenome -c "SELECT 1;" >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo    Waiting for database...
    timeout /t 5 /nobreak >nul
)

REM ---- 3. Load API key from .Renviron ----
echo [3/4] Loading API key...
if exist "%USERPROFILE%\.Renviron" (
    for /f "tokens=1,* delims==" %%a in ('type "%USERPROFILE%\.Renviron" ^| findstr "GROQ_API_KEY"') do set GROQ_API_KEY=%%b
)

REM ---- 4. Launch the Shiny app ----
echo [4/4] Launching Shiny app on http://localhost:3838
echo.
echo ==============================================
echo   App is starting...
echo   Open: http://localhost:3838
echo   Press Ctrl+C in this window to stop.
echo ==============================================
echo.

cd /D C:\Users\Erick.Amombo\alfalfa_pangenome
R -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)"

pause