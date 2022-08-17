with HAT; use HAT;

procedure Shell is
  type OS_Kind is (Nixux, Windoze);
  k : OS_Kind;
  r : Integer;
  f : File_Type;
  --
  procedure Pipe_Test (with_result, with_output : Boolean) is
    line, contents : VString;
    out_name : constant VString := +"output.lst";
    secret_command : constant VString :=
      +"echo This is the ultra-secret message for testing I/O pipe";
    piped_secret_command : constant VString := secret_command & '>' & out_name;
  begin
    Put ("Testing outward pipe (command>something). With result parameter: ");
    Put (with_result);
    Put (". With output parameter: ");
    Put (with_output);
    Put_Line (".");
    if with_output then
      if with_result then
        Shell_Execute (secret_command, r, contents);
      else
        Shell_Execute (secret_command, contents);
      end if;
      Put_Line (+"  --> Contents output VString are: [" & contents & ']');
    else
      if with_result then
        Shell_Execute (piped_secret_command, r);
      else
        Shell_Execute (piped_secret_command);
      end if;
      Put (+"  --> Contents of file " & out_name & " are: [");
      Open (f, out_name);
      while not End_Of_File (f) loop
        Get_Line (f, line);
        Put_Line (line);
      end loop;
      Close (f);
      Put_Line (']');
    end if;
    if with_result and r /= 0 then
      Put_Line (+"Result of echo command is not 0: " & r);
    end if;
    New_Line;
  end Pipe_Test;
  --
  procedure Produce_Errors (command : VString) is
  begin
    Shell_Execute (command, r);
    Put_Line (
      +"Result of command """ & command &
      """ should be not 0. Returned value is: " & r
    );
    if k = Nixux then
      --  WEXITSTATUS
      Put_Line (+"  POSIX: filtered exit code is: " & r / 256 mod 256);
    end if;
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
  for w_res in Boolean loop
    for w_out in Boolean loop
      Pipe_Test (w_res, w_out);
    end loop;
  end loop;
  Produce_Errors (+"Command_Impossible");
  Produce_Errors (+"exit 123");
end Shell;
