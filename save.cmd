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

set nice_date=%year%-%month%-%day%
set nice_date=%year%-%month%-%day%_%hour%.%min%


rem --------------------------

set root=hac

cd..

set examples=%root%/exm/*.ad* %root%/exm/special/*.ad* %root%/exm/*.gpr %root%/exm/e.cmd
set tests=%root%/test/*.ad* %root%/test/*.gpr %root%/test/t.cmd %root%/test/*.aru %root%/test/future/*.ad*

set files=%root%/*.ad* %root%/*.gpr %root%/*.xls %root%/save.cmd %root%/build.cmd %root%/*.txt
set files=%files% %root%/debug.pra %root%/obj/debug/create_dir.txt %root%/obj/fast/create_dir.txt
set files=%files% %examples% %tests%

zipada -ep2 %root%/hac-%nice_date%-        %files%
REM zipada -ed3 %root%/hac-%nice_date%-DEFLATE %files%
