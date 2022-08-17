--  Example from command-line:  hac echo.adb <echo.adb >echo.txt

with HAT; use HAT;

procedure Echo is
  s : VString;
begin
  Put_Line ("Type your messages (""STOP!"" or Ctrl-Z to stop):");
  while not End_Of_File loop
    Get_Line (s);
    --  Exit door if program run from a console without Ctrl-Z :
    exit when s = "STOP!";
    Put_Line ("This is the echo...  [" & s & ']');
  end loop;
end Echo;
