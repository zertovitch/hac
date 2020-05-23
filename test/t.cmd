@echo off

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

REM echo.
REM echo *******[ HAC Testing ]******* "Verbose tests".
REM echo.

REM ..\hax -v2 if_then_elsif_else.adb
REM pause

echo.
echo *******[ HAC Testing ]******* "Silent tests": when there is zero output and no compilation error, then it's all fine.
echo.
rem Here HAX will call itself for each test!...
..\hax all_silent_tests.adb
pause

goto fin

echo ******* Building all tests with GNAT.

gprbuild hac_test.gpr

echo ******* Running all tests with GNAT.

for %%e in (*.exe) do %%e

pause

:fin