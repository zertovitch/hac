with HAC_Pack;  use HAC_Pack;

procedure Timing is
  T : Time := Clock;
begin
  Put_Line (Image (T));
  for x in 1 .. 1_000_000 loop
    null;
  end loop;
  --  delay 1.0;
  Put_Line (Image (Clock));
end;