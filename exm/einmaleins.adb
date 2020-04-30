--  example of use, just written "live" using the
--  LEA editor, during a homeschooling session.

with HAC_Pack;  use HAC_Pack;

procedure einmaleins is
  x, y : integer;
begin
  x := 1 + rand (9);
  y := 1 + rand (9);
  put(x);
  put(y);
  put(x*y);
end;
