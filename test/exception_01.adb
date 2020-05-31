with HAC_Pack;  use HAC_Pack;

procedure Exception_01 is
  a : array (1 .. 3) of Integer;
begin
  a (4) := 5;
end;
