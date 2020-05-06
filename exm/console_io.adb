with HAC_Pack;  use HAC_Pack;

procedure Console_IO is
  procedure Test_Get is
    C1, C2 : Character;
    I1, I2 : Integer;
    R1, R2 : Real;
    V : VString;
  begin
    Put ("Type 2 characters, then Return: ");
    Get (C1); Get_Line (C2);
    Put_Line (+"You have typed [" & C1 & "] [" & C2 & "]");
    --
    Put ("Type 2 integers, then Return: ");
    Get (I1); Get_Line (I2);
    Put_Line (+"You have typed [" & I1 & "] [" & Image(I2) & "]");
    --
    Put ("Type 2 floats, then Return: ");
    Get (R1); Get_Line (R2);
    Put_Line (+"You have typed [" & R1 & "] [" & Image(R2) & "]");
    --
    Put ("Type whatever you want, then Return: ");
    Get_Line (V);
    Put_Line ("You just typed: [" & V & ']');
  end;
begin
  Put ("Please Press Return!");
  Skip_Line;
  Put_Line ("Bravo, you did it!");
  New_Line;
  --
  Test_Get;
end Console_IO;
