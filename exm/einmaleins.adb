--  Example of use, just written "live" using the
--  LEA editor, during a homeschooling session.

with HAT; use HAT;

procedure Einmaleins is
  x, y : Integer;
begin
  x := 2 + Rand (7);  --  Random, 2 to 9
  y := 2 + Rand (7);  --  Random, 2 to 9
  --
  if Rnd < 0.5 then
    x := x * 10;
  else
    y := y * 10;
  end if;
  --
  Put (x, 0); Put (" * "); Put (y, 0); Put (" = "); Put (x * y, 0);
end Einmaleins;
