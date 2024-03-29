@echo off

echo (Re-)building HAC
cd..
gprbuild -P hac.gpr hac
cd exm
set hacbuild=done

if "%1"=="" goto gallery

rem Try without extension
if exist %1.adb ..\hac -v2 %1.adb %2 %3 %4 %5 %6 %7 %8 %9
if exist %1.adb goto fin

rem Try with extension
..\hac -v2 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto fin

:gallery

..\hac gallery.adb

:fin