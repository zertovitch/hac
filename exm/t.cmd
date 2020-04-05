@echo off

cd..
call build
cd exm

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hac_test %1.adb
if exist %1.adb goto fin

..\hac_test %1
goto fin

:gallery

..\hac_test hello.adb
pause
..\hac_test ackermann.adb
pause
..\hac_test anti_primes.adb
pause
..\hac_test doors.adb
pause
..\hac_test test.adb
pause
..\hac_test test1.adb
pause
..\hac_test shell_sort.adb
pause
..\hac_test merge_sort.adb
pause
..\hac_test days_1901.adb
pause

:fin