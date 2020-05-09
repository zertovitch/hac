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
  Set_Env ("HAC_Rules", "Good Day, Ladies and Gentlemen!");
  --
  case k is
    when Nixux   => r := Shell_Execute ("echo The env. var. is set... [$HAC_Rules]");
    when Windoze => r := Shell_Execute ("echo The env. var. is set... [%HAC_Rules%]");
  end case;
  Put_Line (+"Result of command = " & r);
end;