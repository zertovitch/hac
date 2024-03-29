@echo off

rem  HAC regression test
rem  -------------------
rem  This script is basically the command "hac -v2 all_silent_tests.adb"
rem  preceded by a potential rebuild of the hac executable.

echo (Re-)building HAC
cd..
set hacbuild=unknown
gprbuild -P hac.gpr hac
if NOT "%ERRORLEVEL%" == "0" goto hac_build_failed
cd test
set hacbuild=done

if "%1"=="" goto regression_tests

rem Try without extension
if exist %1.adb ..\hac -v2 %1.adb
if exist %1.adb goto fin

rem Try with extension
..\hac -v2 %1
goto fin

:regression_tests
rem Here HAC will call itself for each test!...

..\hac -v2 all_silent_tests.adb
pause

goto fin

:verbose_tests
..\hac -v2 all_noisy_tests.adb
goto fin

:gnat
rem ******* Building all tests with GNAT.
gprbuild hac_test.gpr

echo ******* Running all tests with GNAT.
for %%e in (*.exe) do %%e

pause
goto fin

:hac_build_failed
echo --- HAC build failed (called from t.cmd) ---
cd test

:fin
set hacbuild=unknown
