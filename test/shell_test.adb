with HAT; use HAT;

procedure Shell_Test is

  --  Corresponds to POSIX' WEXITSTATUS in common implementations.
  function Exit_Status (Shell_Result : Integer) return Integer is
  begin
    Return Shell_Result / 256 mod 256;
  end Exit_Status;

  --  Corresponds to POSIX' WSTOPSIG in common implementations.
  function Stop_Signal (Shell_Result : Integer) return Integer is
  begin
    Return Shell_Result / 256 mod 128;
  end Stop_Signal;

  --  Corresponds to POSIX' WTERMSIG in common implementations.
  function Termination_Signal (Shell_Result : Integer) return Integer is
  begin
    Return Shell_Result mod 128;
  end Termination_Signal ;

  procedure Pipe_Test (n: Integer) is
    r : Integer;
    command : constant VString := +"exit " & n;
    contents : VString;
  begin
    Put (+"Command: " & command & ". Result...");
    Shell_Execute (command, r);
    Put (+" without pipe: " & r & ", POSIX: " & Exit_Status (r));
    Shell_Execute (command, r, contents);
    Put (+", with pipe: " & r & ", POSIX: " & Exit_Status (r));
    Shell_Execute (command & ">dummy_output.txt", r);
    Put (+", with explicit pipe: " & r & ", POSIX: " & Exit_Status (r));
    New_Line;
  end Pipe_Test;
  --
begin
  for n in 0 .. 15 loop
    Pipe_Test (n);
  end loop;
  for n in 255 .. 258 loop
    Pipe_Test (n);
  end loop;
end Shell_Test;
