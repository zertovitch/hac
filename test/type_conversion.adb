--  This example contains a feature that was missing in HAC 0.01 but is present in HAC 0.02.
--
--  4.6 Type Conversions

with HAC_Pack; use HAC_Pack;

procedure Type_Conversion is
  i, j : Integer;
  x, y : Real;
begin
  i := 1;
  x := 2.0;
  y := x + Real (i);        --  <-- Here is the wonderful feature!
  j := i + Integer (3.51);  --  <-- Here is the wonderful feature!
  --
  -- *Implicit* numerical type conversion is featured by HAC 0.01, but it's NOT Ada!
  --  y := x + i;
  --
  if y /= 3.0 then
    Put_Line ("Compiler bug [A]");
  end if;
  if j /= 5 then
    Put_Line ("Compiler bug [B]");
  end if;
end;
