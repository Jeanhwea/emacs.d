@echo off
setlocal enabledelayedexpansion

for %%I in ("%~dp0..") do set "EBASE=%%~fI"
set "SCRIPT=%EBASE%\scripts\instpkg.el"

set "DRYRUN="
for %%A in (%*) do (
  if /i "%%~A"=="-n" set "DRYRUN=1"
  if /i "%%~A"=="--dry-run" set "DRYRUN=1"
)

if defined EMACS goto :check

for %%C in (emacs.exe) do set "EMACS=%%~$PATH:C"
if defined EMACS if exist "!EMACS!" goto :check
set "EMACS="

call :probe "%ProgramFiles%\Emacs"
if defined EMACS goto :check
call :probe "%ProgramFiles(x86)%\Emacs"
if defined EMACS goto :check
call :probe "%LOCALAPPDATA%\Programs\Emacs"
if defined EMACS goto :check
call :probe "%USERPROFILE%\scoop\apps\emacs\current"
if defined EMACS goto :check
call :probe "D:\tool"
if defined EMACS goto :check
call :probe "C:\"
if defined EMACS goto :check

echo ERROR: cannot locate emacs.exe
echo        set the EMACS environment variable to the full path, e.g.
echo        set "EMACS=C:\Program Files\Emacs\emacs-31.1\bin\emacs.exe"
exit /b 1

:check
if not exist "%EMACS%" (
  echo ERROR: emacs not found at "%EMACS%"
  exit /b 1
)
if not exist "%SCRIPT%" (
  echo ERROR: script not found at "%SCRIPT%"
  exit /b 1
)

echo emacs:  %EMACS%
echo config: %EBASE%
echo script: %SCRIPT%

if defined DRYRUN (
  echo [dry-run] "%EMACS%" --script "%SCRIPT%"
  exit /b 0
)

"%EMACS%" --script "%SCRIPT%"
set "RC=%errorlevel%"
if not "%RC%"=="0" echo FAILED with exit code %RC%
exit /b %RC%

:probe
set "PDIR=%~1"
if "%PDIR%"=="" goto :eof
if not exist "%PDIR%\" goto :eof
if exist "%PDIR%\bin\emacs.exe" (
  set "EMACS=%PDIR%\bin\emacs.exe"
  goto :eof
)
for /f "delims=" %%D in ('dir /b /ad /o-n "%PDIR%\emacs-*" 2^>nul') do (
  if not defined EMACS if exist "%PDIR%\%%D\bin\emacs.exe" set "EMACS=%PDIR%\%%D\bin\emacs.exe"
)
goto :eof
