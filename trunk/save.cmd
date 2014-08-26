rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------

set year=%date:~-4,4%

set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%

set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%

set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%

set min=%time:~3,2%

set nice_date=%year%-%month%-%day%_%hour%.%min%
set nice_date=%year%-%month%-%day%


rem --------------------------

zipada -edf hac_%nice_date% *.ad* *.gpr save.cmd build.cmd *.txt sma_exm/*.ad* sma_exm/special/*.ad* sma_exm/*.gpr sma_exm/t.cmd obj/debug/debug.pra obj/fast/create_dir.txt
