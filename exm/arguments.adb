with HAT; use HAT;

procedure Arguments is
begin
  Put ("Command-line arguments:");
  Put (Argument_Count);
  New_Line (2);
  Put_Line ("Argument list:");
  Put_Line ("--------------");
  for A in 1 .. Argument_Count loop
    Put_Line ("  --> [" & Argument (A) & ']');
  end loop;
end Arguments;
