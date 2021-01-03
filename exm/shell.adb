with HAC_Pack;  use HAC_Pack;

procedure Shell is
  type OS_Kind is (Nixux, Windoze);
  k : OS_Kind;
  r : Integer;
  f : File_Type;
  ln : constant VString := +"output.lst";
  line : VString;
  --
  procedure Produce_Errors (command : VString) is
  begin
    Shell_Execute (command, r);
    Put_Line (+"Result of " & command & " = " & r);
  end Produce_Errors;
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
  Put_Line (+"Result of echo command = " & r);
  --
  Shell_Execute (+"echo Testing I/O pipe>" & ln, r);
  Put_Line (+"Result of echo command = " & r);
  Open (f, ln);
  Get_Line (f, line);
  Close (f);
  Put_Line (+"--> Contents of file " & ln & " are: [" & line & ']');
  --
  Produce_Errors (+"Command_Impossible");
  Produce_Errors (+"exit 666");
end Shell;
