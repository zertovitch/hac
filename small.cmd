@echo off
echo Build...
del hac*.exe
gprbuild -P hac hac       -XHAC_Build_Mode=Small_Unchecked
copy /B hac.exe hac_unchecked.exe
del hac.exe
gprbuild -P hac           -XHAC_Build_Mode=Small           -largs obj/hac_icon.rbj
del demo\*.exe
gprbuild -P demo/hac_demo -XHAC_Build_Mode=Small           -largs obj/hac_icon.rbj

echo.
for %%I in (hac*.exe) do echo %%~zI ... %%I
for %%I in (demo\*.exe) do echo %%~zI ... %%I

echo.
echo Stripping...
strip -s hac*.exe
strip -s demo/*.exe

echo Done.
echo.
for %%I in (hac*.exe) do echo %%~zI ... %%I
for %%I in (demo\*.exe) do echo %%~zI ... %%I
