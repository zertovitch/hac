with Unit_B, Unit_C, HAT;

--  GNAT compilation: gnatmake unit_a -I..\src
--  "src" is for getting the HAT package.

procedure Unit_A is
  v : Integer;
  a_msg : HAT.VString;

  use HAT;

  procedure X is
  begin
    Put ("(x>");
    a_msg := +"A";
    Unit_B (v, +"b", +"B");
    v := v * 3;
    Put ("<x)");
  end X;

begin
  Put_Line ("Unit_A: demo of modularity features for subprograms.");
  Put_Line ("------");
  Put_Line ("  NB: the program and its output are nonsensical, but");
  Put_Line ("  the output should be identical on all Ada implementations.");
  New_Line;
  Put_Line ("----[begin]----");
  v := 10101;
  a_msg := +"a";
  HAT.Put (+"(a" & a_msg & ">");
  for i in 1 .. 2 loop
    Put (Image (Unit_C (+"u", +"U", v)));
  end loop;
  X;
  Unit_B (v, +"b", +"B");
  X;
  HAT.Put (v, 0);
  HAT.Put ("<A" & a_msg & ")");
  New_Line;
  Put_Line ("----[end]------");
end Unit_A;
