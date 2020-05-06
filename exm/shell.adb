with HAC_Pack;  use HAC_Pack;

procedure Shell is
  type OS_Kind is (Nixux, Windoze);
  k : OS_Kind;
  r : Integer;
begin
  if Index (Get_Env ("OS"), "Windows") > 0 then
    k := Windoze;
  else
    k := Nixux;
  end if;
  --
  case k is
    when Nixux   => r := Shell_Execute ("ls *.ad*");
    when Windoze => r := Shell_Execute ("dir *.ad*");
  end case;
  Put_Line (+"Result of command = " & r);
end;