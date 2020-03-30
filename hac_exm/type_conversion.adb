--  This example contains a feature that is missing in HAC 0.01
--
--  4.6 Type Conversions

with HAC_Pack; use HAC_Pack;

procedure Type_Conversion is
  i: Integer;
  x: Float;
begin
  i := 1;
  x := 2.0;
  x := x + Float (i);
  --  x := x + i;  --  *Implicit* numerical type conversion is featured by HAC 0.01, but it's NOT Ada!
  Put (x);  --  Should be 3.0
end;