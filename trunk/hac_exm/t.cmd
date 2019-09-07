@echo off

cd..
call build
cd hac_exm

if not "%1"=="" ..\hac_test %1.adb
if not "%1"=="" goto fin

..\hac_test hello.adb
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