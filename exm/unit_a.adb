with Unit_B, Unit_C, HAL;

--  GNAT compilation: gnatmake unit_a -I..\src
--  "src" is for getting the HAL package.

procedure Unit_A is
  v : Integer;
  a_msg : HAL.VString;

  use HAL;

  procedure X is
  begin
    Put("(x>");
    a_msg := +"A";
    Unit_B (v, +"b", +"B");
    v := v * 3;
    Put("<x)");
  end X;

begin
  Put_Line ("Unit_A: demo of modularity features.");
  Put_Line ("  NB: the program and its output are nonsensical, but");
  Put_Line ("  the output should be identical on all Ada implementations.");
  v := 10101;
  a_msg := +"a";
  HAL.Put(+"(a" & a_msg & ">");
  for i in 1 .. 2 loop
    Put(Image(Unit_C (+"u", +"U", v)));
  end loop;
  X;
  Unit_B (v, +"b", +"B");
  X;
  HAL.Put(v, 0);
  HAL.Put("<A" & a_msg & ")");
end Unit_A;