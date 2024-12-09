@echo off
echo Build...
del hac*.exe
gprbuild -P hac hac       -XHAC_Build_Mode=Small_Unchecked -XHAC_OS=Win64
copy /B hac.exe hac_unchecked.exe
del hac.exe
gprbuild -P hac           -XHAC_Build_Mode=Small           -XHAC_OS=Win64
del demo\*.exe
gprbuild -P demo/hac_demo -XHAC_Build_Mode=Small           -XHAC_OS=Win64

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
