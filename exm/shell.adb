with HAL; use HAL;

procedure Shell is
  type OS_Kind is (Nixux, Windoze);
  k : OS_Kind;
  r : Integer;
  f : File_Type;
  --
  procedure Pipe_Test (variant : Positive) is
    line : VString;
    ln : constant VString := +"output.lst";
    secret_command : constant VString :=
      +"echo This is the super-secret message for testing I/O pipe";
  begin
    Put_Line (+"Testing outward pipe (command>file), variant " & variant & ".");
    case variant is
      when 1      =>
        Shell_Execute (secret_command & '>' & ln, r);
        Open (f, ln);
        Get_Line (f, line);
        Close (f);
        Put_Line (+"--> Contents of file " & ln & " are: [" & line & ']');
      when others =>
        --  !!wip!! Shell_Execute (secret_command, r, line);
        Put_Line (+"--> Contents of temp output file are: [" & line & ']');
    end case;
    if r /= 0 then
      Put_Line (+"Result of echo command is not 0: " & r);
    end if;
    New_Line;
  end Pipe_Test;
  --
  procedure Produce_Errors (command : VString) is
  begin
    Shell_Execute (command, r);
    Put_Line (+"Result of command """ & command & """ should be not 0, it is: " & r);
    New_Line;
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
  if r /= 0 then
    Put_Line (+"Result of echo command is not 0: " & r);
  end if;
  --
  Pipe_Test (1);
  --  !!wip!! Pipe_Test (2);
  Produce_Errors (+"Command_Impossible");
  Produce_Errors (+"exit 666");
end Shell;
