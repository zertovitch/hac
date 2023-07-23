@echo off

echo HAC, *ObjectAda* build

cd..
COPY /b hac_objectada-Win32(Intel)-Debug\hac.exe .
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

REM We run the *GNAT-Compiled* executable of all_silent_tests
REM Reason is that Shell_Execute in HAC compiled by OA fails.
all_silent_tests.exe
REM ..\hac -v2 all_silent_tests.adb
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