@echo off

echo (Re-)building HAC
cd..
call build hac
cd test
set hacbuild=done

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hac -v2 %1.adb
if exist %1.adb goto fin

rem Try with extension
..\hac -v2 %1
goto fin

:gallery
rem Here HAC will call itself for each test!...

..\hac -v2 all_silent_tests.adb
pause

goto fin

:verbose_tests
..\hac -v2 if_then_elsif_else.adb
goto fin

:gnat
rem ******* Building all tests with GNAT.
gprbuild hac_test.gpr

echo ******* Running all tests with GNAT.
for %%e in (*.exe) do %%e

pause

:fin