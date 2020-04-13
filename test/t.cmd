@echo off

cd..
call build
cd test

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hax -v2 %1.adb
if exist %1.adb goto fin

..\hax -v2 %1
goto fin

:gallery

echo.
echo *******[ HAC Testing ]******* "Verbose tests".
echo.

..\hax -v2 if_then_elsif_else.adb
pause

echo.
echo *******[ HAC Testing ]******* "Silent tests": when there is zero output and no compilation error, it's all fine.
echo.

..\hax -v1 constants.adb
..\hax -v1 declarations.adb
..\hax -v1 enumerations.adb
..\hax -v1 floats.adb
..\hax -v1 integers.adb
..\hax -v1 recursion.adb
..\hax -v1 sorting_tests.adb
..\hax -v1 type_conversion.adb
pause

goto fin

echo ******* Building all tests with GNAT.

gprbuild hac_test.gpr

echo ******* Running all tests with GNAT.

for %%e in (*.exe) do %%e

pause

:fin