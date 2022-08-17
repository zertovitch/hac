with Unit_C, Unit_E, Unit_F;

procedure Unit_B (n : in out Integer; title_1, title_2 : HAT.VString) is
  use HAT;
  b_msg : VString := +"b";
  --
  procedure Y is
  begin
    Put ("(y>");
    Put (Image (Unit_C (+"c", +"C", n)));
    HAT.Put ("<y)");
    b_msg := +"B";
  end Y;
  procedure Y2 is
  begin
    Y;
  end Y2;
begin
  Put (+"(" & title_1 & b_msg & n & ">");
  Put (Image (Unit_C (+"s", +"S", n)));
  Y2;
  HAT.Put (+"<" & title_2 & b_msg & ")");
  Unit_E;
  if n /= 777 then
    Unit_F;
  end if;
  n := n * 2;
end Unit_B;
