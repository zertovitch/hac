with HAT; use HAT;

procedure If_Then_Elsif_Else is

  procedure Test (I : Integer) is
  begin
    Put (I);
    Put (" compared to 10, 20 and 30 is : ");
    if I < 10 then
      Put_Line ("the smallest");
    elsif I < 20 then
      Put_Line ("in [10, 19]");
    elsif I <= 30 then
      Put_Line ("in [20, 30]");
    else
      Put_Line ("the largest");
    end if;
  end Test;

begin
  for J in 8 .. 32 loop
    Test (J);
  end loop;
end If_Then_Elsif_Else;
