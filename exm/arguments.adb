with HAC_Pack;  use HAC_Pack;

procedure Arguments is
begin
  Put ("Command-line arguments:");
  Put (Argument_Count);
  New_Line;
  New_Line;
  Put_Line ("Argument list:");
  Put_Line ("--------------");
  for A in 1 .. Argument_Count loop
    Put_Line ("  --> [" & Argument (A) & ']');
  end loop;
end Arguments;