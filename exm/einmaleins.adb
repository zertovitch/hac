--  Example of use, just written "live" using the
--  LEA editor, during a homeschooling session.

with HAC_Pack;  use HAC_Pack;

procedure Einmaleins is
  x, y : Integer;
begin
  x := 2 + Rand (8);  --  Random, 2 to 10
  y := 2 + Rand (8);  --  Random, 2 to 10
  if Rnd < 0.5 then
    x := x * 10;
  else 
    y := y * 10;
  end if;
  Put (x); Put (y); Put ("  ="); Put (x * y);
end;
