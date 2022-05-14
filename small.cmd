@echo off
echo Build...
gprbuild -P hac -XHAC_Build_Mode=Small -largs obj/hac_icon.rbj
for %%I in (hac.exe) do echo %%~zI
echo Strip...
strip -s hac.exe
for %%I in (hac.exe) do echo %%~zI
