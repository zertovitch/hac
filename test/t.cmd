@echo off

echo (Re-)building HAX
cd..
call build
cd test
set haxbuild=done

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hax -v2 %1.adb
if exist %1.adb goto fin

..\hax -v2 %1
goto fin

:gallery
rem Here HAX will call itself for each test!...

..\hax -v2 all_silent_tests.adb
pause

goto fin

:verbose_tests
..\hax -v2 if_then_elsif_else.adb
goto fin

:gnat
rem ******* Building all tests with GNAT.
gprbuild hac_test.gpr

echo ******* Running all tests with GNAT.
for %%e in (*.exe) do %%e

pause

:fin