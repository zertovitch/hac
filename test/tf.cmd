@echo off

rem  HAC regression test
rem  -------------------
rem  Regression test in "fast" mode.

echo (Re-)building HAC
cd..
del hac.exe
gprbuild -XHAC_Build_Mode=Fast -P hac -largs obj/hac_icon.rbj 
cd test
set hacbuild=done

..\hac -v2 all_silent_tests.adb
pause
