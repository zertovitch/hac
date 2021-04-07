with Unit_C, HAL;

procedure Unit_B (n : in out Integer; title_1, title_2 : HAL.VString) is
  use HAL;
  b_msg : VString := +"b";
  --
  procedure Y is
  begin
    Put("(y>");
    Put(Image(Unit_C (+"c", +"C", n)));
    HAL.Put("<y)");
    b_msg := +"B";
  end;
  procedure Y2 is
  begin
    Y;
  end;
begin
  Put (+"(" & title_1 & b_msg & n & ">");
  Put(Image(Unit_C (+"s", +"S", n)));
  Y2;
  HAL.Put(+"<" & title_2 & b_msg & ")");
  n := n * 2;
end Unit_B;