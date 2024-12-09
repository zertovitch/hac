call go17

gprbuild -P hac -XHAC_Build_Mode=Profiling -XHAC_OS=Win32

cd exm\aoc\2022
..\..\..\hac -c -v2 aoc_2022_21.adb 

copy gmon.out ..\..\..
del gmon.out
cd ..\..\..
gprof hac.exe >hac_profile.txt
