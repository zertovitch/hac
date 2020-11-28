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
    when Nixux   => Shell_Execute ("echo The env. var. is set... [$HAC_Rules]", r);
    when Windoze => Shell_Execute ("echo The env. var. is set... [%HAC_Rules%]", r);
  end case;
  Put_Line (+"Result of command = " & r);
  --
  New_Line;
  Shell_Execute ("Command_Impossible", r);
  Put_Line (+"Result of Command_Impossible = " & r);
end Shell;
