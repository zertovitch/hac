with HAC_Pack;  use HAC_Pack;

procedure Console_IO is
  procedure Test_Get is
    C1, C2 : Character;
    I1, I2 : Integer;
    R1, R2 : Real;
    V : VString;
  begin
    if Get_Needs_Skip_Line then
      Put ("Type 2 characters, then Return.");
    else
      Put_Line ("Type 2 characters - one each time!");
    end if;
    Get (C1); Get (C2);
    if Get_Needs_Skip_Line then Skip_Line; end if;
    Put_Line (+"You have typed [" & C1 & "] [" & C2 & "]");
    --
    if Get_Needs_Skip_Line then
      Put_Line ("Type 2 characters. This time there is no need for pressing Return.");
      Get_Immediate (C1);
      Get_Immediate (C2);
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
    Put_Line (+"You have typed the numbers [" & I1 & "] [" & Image(I2) & "]");
    --
    if Get_Needs_Skip_Line then
      Put ("Type 2 floats, then Return.");
    else
      Put_Line ("Type 2 floats.");
    end if;
    Get (R1); Get (R2);
    if Get_Needs_Skip_Line then Skip_Line; end if;
    Put_Line (+"You have typed the numbers [" & R1 & "] [" & Image(R2) & "]");
    --
    Put ("Type whatever you want, then Return.");
    Get_Line (V);
    Put_Line ("Congrats, you just typed: [" & V & ']');
  end;
begin
  Put_Line ("Do we have a real console/terminal ? ");
  if Get_Needs_Skip_Line then
    Put_Line ("Yes! Ada.Text_IO can get multiple inputs from the same line.");
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
