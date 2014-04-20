@echo off

cd..
call build
cd sma_exm

if not "%1"=="" ..\hac_test %1.adb
if not "%1"=="" goto fin

..\hac_test hello.adb
pause
..\hac_test test.adb
pause
..\hac_test test1.adb
pause

:fin