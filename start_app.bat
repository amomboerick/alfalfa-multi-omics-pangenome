@echo off
REM ============================================================
REM Alfalfa Multi-Omics Pan-Genome Database - Launcher
REM Two environments: pg_db (PostgreSQL) + r_env (R/Shiny)
REM ============================================================

set CONDA_BAT=C:\Users\Erick.Amombo\AppData\Local\miniconda3\condabin\conda.bat

echo.
echo ==============================================
echo   ALFALFA MULTI-OMICS PAN-GENOME DATABASE
echo ==============================================
echo.

REM --- Start PostgreSQL (pg_db) ---
echo [1/3] Starting PostgreSQL...
call "%CONDA_BAT%" activate pg_db
pg_ctl -D "%USERPROFILE%\pg_data" -l "%USERPROFILE%\pg_logs\postgres.log" start >nul 2>&1
timeout /t 3 /nobreak >nul

REM --- Load API key from .Renviron ---
echo [2/3] Loading API key...
if exist "%USERPROFILE%\.Renviron" (
    for /f "tokens=1,2 delims==" %%a in ('type "%USERPROFILE%\.Renviron" ^| findstr "GROQ_API_KEY"') do set GROQ_API_KEY=%%b
)

REM --- Launch Shiny app (r_env) ---
echo [3/3] Launching Shiny app on http://localhost:3838
call "%CONDA_BAT%" activate r_env

echo.
echo ==============================================
echo   Open: http://localhost:3838
echo   Press Ctrl+C to stop the app
echo ==============================================
echo.

cd /D C:\Users\Erick.Amombo\alfalfa_pangenome
R -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)"