@echo off

gprbuild %1 -P hac           -XHAC_OS=Win64
gprbuild %1 -P demo/hac_demo -XHAC_OS=Win64

if %errorlevel% == 9009 goto error

echo Press Return
pause
goto :eof

:error

echo.
echo The GNAT Ada compiler was not found in the PATH!
echo.
echo Check https://www.adacore.com/download for GNAT
echo or https://alire.ada.dev/ for ALIRE.
echo The HAC project is available as an ALIRE crate.
echo.
echo Press Return
pause
