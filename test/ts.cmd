@echo off

rem  HAC regression test
rem  -------------------
rem  Regression test in "small" mode.

echo (Re-)building HAC
cd..
del hac.exe
gprbuild -XHAC_Build_Mode=Small -P hac -largs obj/hac_icon.rbj 
strip -s hac*.exe
cd test
set hacbuild=done

..\hac -v2 all_silent_tests.adb
pause
