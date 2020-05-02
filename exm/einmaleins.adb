--  Example of use, just written "live" using the
--  LEA editor, during a homeschooling session.

with HAC_Pack;  use HAC_Pack;

procedure Einmaleins is
  x, y : Integer;
begin
  x := 1 + Rand (9);
  y := 1 + Rand (9);
  Put (x);
  Put (y);
  Put (x*y);
end;
