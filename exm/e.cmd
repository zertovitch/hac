@echo off

cd..
call build
cd exm

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hax -v2 %1.adb
if exist %1.adb goto fin

..\hax -v2 %1
goto fin

:gallery

..\hax -v2 strings_demo.adb
pause
..\hax -v2 hello.adb
pause
..\hax -v2 arguments.adb arg1 arg2 "arg 3 ..." arg4
pause
..\hax -v2 ackermann.adb
pause
..\hax -v2 anti_primes.adb
pause
..\hax -v2 doors.adb
pause
..\hax -v2 mandelbrot.adb
pause
..\hax -v2 test.adb
pause
..\hax -v2 test1.adb
pause
..\hax -v2 shell_sort.adb
pause
..\hax -v2 merge_sort.adb
pause
..\hax -v2 days_1901.adb
pause

:fin