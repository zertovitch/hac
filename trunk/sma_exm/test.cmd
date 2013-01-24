cd..
call build
cd sma_exm

if "%1"=="" ..\hac_test *.adb
if not "%1"=="" ..\hac_test %1.adb
