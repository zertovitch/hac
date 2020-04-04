--  This example contains a feature that was missing in HAC 0.01 but is present in HAC 0.02.
--
--  4.6 Type Conversions

with HAC_Pack; use HAC_Pack;

procedure Type_Conversion is
  i : Integer;
  x, y : Float;
begin
  i := 1;
  x := 2.0;
  y := x + Float (i);  --  <-- Here is the wonderful feature!
  --
  -- *Implicit* numerical type conversion is featured by HAC 0.01, but it's NOT Ada!
  --  y := x + i;
  --
  if y /= 3.0 then
    Put_Line ("Compiler bug [A]");
  end if;
end;
