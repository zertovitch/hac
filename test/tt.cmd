@echo off

cd..
call build
cd test

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hac_test %1.adb
if exist %1.adb goto fin

..\hac_test %1
goto fin

:gallery

..\hac_test if_then_elsif_else.adb
pause

echo.
echo *******[ HAC Testing ]******* "Silent tests": when there is zero output and no compilation error, it's all fine.
echo.

..\hac_test -q declarations.adb
..\hac_test -q enumerations.adb
..\hac_test -q floats.adb
..\hac_test -q integers.adb
..\hac_test -q type_conversion.adb
pause

:fin