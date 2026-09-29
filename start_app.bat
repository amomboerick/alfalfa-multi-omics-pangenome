@echo off
REM ============================================================
REM Alfalfa Pan-Genome Database - One-Click Launcher
REM ============================================================

echo.
echo ==============================================
echo   ALFALFA PAN-GENOME DATABASE
echo   Starting all services...
echo ==============================================
echo.

REM ---- 1. Activate conda environment ----
call conda activate pg_db

REM ---- 2. Start PostgreSQL ----
echo [1/4] Starting PostgreSQL...
pg_ctl -D "%USERPROFILE%\pg_data" -l "%USERPROFILE%\pg_logs\postgres.log" start >nul 2>&1
timeout /t 3 /nobreak >nul

REM ---- 3. Verify database is reachable ----
echo [2/4] Verifying database...
psql -U postgres -p 5433 -d alfalfa_pangenome -c "SELECT 1;" >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo    Database not reachable. Waiting 5 more seconds...
    timeout /t 5 /nobreak >nul
)

REM ---- 4. Set the Groq API key ----
echo [3/4] Setting API key...
REM Load API key from .Renviron (set up separately, do NOT commit)
if exist "%USERPROFILE%\.Renviron" (
    for /f "tokens=1,* delims==" %%a in ('type "%USERPROFILE%\.Renviron" ^| findstr "GROQ_API_KEY"') do set GROQ_API_KEY=%%b
)

REM ---- 5. Launch the Shiny app ----
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