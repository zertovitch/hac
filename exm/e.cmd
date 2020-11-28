@echo off

echo (Re-)building HAC
cd..
call build
cd exm
set hacbuild=done

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hac -v2 %1.adb
if exist %1.adb goto fin

rem Try with extension
..\hac -v2 %1
goto fin

:gallery

..\hac gallery.adb

:fin