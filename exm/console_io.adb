--  Demo of Get / Get_Immediate / Get_Line / Skip_Line / Put / Put_Line / New_Line
--  on the console.
--  No file involved, unless using:  "hac console_io.adb <in_file.txt >out_file.txt"

with HAT; use HAT;

procedure Console_IO is

  procedure Test_Get is
    C1, C2 : Character;
    I1, I2 : Integer;
    R1, R2 : Real;
    V : VString;
  begin
    if Get_Needs_Skip_Line then
      --  Console mode (via the "hac" command, or when compiled with a "full Ada" compiler).
      Put ("Type 2 characters, then Return.");
    else
      --  GUI mode, like from the LEA editor.
      Put_Line ("Type 2 characters - one each time!");
    end if;
    Get (C1); Get (C2);
    if Get_Needs_Skip_Line then Skip_Line; end if;
    Put_Line (+"You have typed [" & C1 & "] [" & C2 & "]");
    --
    if Get_Needs_Skip_Line then
      Put_Line ("Type 2 characters. This time there is no need for pressing Return.");
      Get_Immediate (C1); Put (+"[" & C1 & ']');
      Get_Immediate (C2); Put (+"[" & C2 & ']');
      New_Line;
      Put_Line (+"You have typed [" & C1 & "] [" & C2 & "]");
    end if;
    --
    if Get_Needs_Skip_Line then
      Put ("Type 2 integers (with a space inbetween), then Return.");
    else
      Put_Line ("Type 2 integers.");
    end if;
    Get (I1); Get (I2);
    if Get_Needs_Skip_Line then Skip_Line; end if;
    Put_Line (+"You have typed the numbers [" & I1 & "] [" & Image (I2) & "]");
    --
    if Get_Needs_Skip_Line then
      Put ("Type 2 floats, then Return.");
    else
      Put_Line ("Type 2 floats.");
    end if;
    Get (R1); Get (R2);
    if Get_Needs_Skip_Line then Skip_Line; end if;
    Put_Line (+"You have typed the numbers [" & R1 & "] [" & Image (R2) & "]");
    --
    Put ("Type whatever you want, then Return.");
    Get_Line (V);
    Put_Line ("Congrats, you just typed: [" & V & ']');
  end Test_Get;

begin
  Put_Line ("Do we have a real console/terminal ? ");
  if Get_Needs_Skip_Line then
    Put_Line ("Yes! Ada.Text_IO can get multiple inputs from the same line.");
    Put_Line ("Skip_Line (a ""Return"" keypress) is needed after one or more Get");
  else
    Put_Line ("No! Perhaps this program is run from LEA ?...");
  end if;
  Put ("Please Press Return!");
  Skip_Line;
  Put_Line ("Bravo, you did it!");
  New_Line;
  --
  Test_Get;
end Console_IO;
