--  This example contains a feature that was missing in HAC 0.01 but is present in HAC 0.02.
--
--  4.6 Type Conversions

with HAT;
with Testing_Utilities;

procedure Type_Conversion is
  use HAT, Testing_Utilities;

  function To_Int (x : Real) return Integer is
  begin
    return Integer (x);
  end To_Int;

  subtype Day_Range is Integer range 1 .. 31;
  i, j : Integer;
  x, y : Real;
  d, e : Duration;
  oa_duration_delta : constant := 0.00006103515625;
  --  ObjectAda Win 32 & 64: type Duration is delta 2.0**(-14) range -131072.0..+131072.0-2.0**(-14);
  dd : Day_Range;
begin
  i := 1234;
  x := Real (i);
  --  dd := Day_Range (x);  --  <---- This should raise a Constraint_Error (not in range)
  --
  dd := 1;
  i := dd;
  x := 0.0 * x + 2.0;
  y := x + Real (i);
  j := i + To_Int (3.51);
  --
  Assert (y = 3.0, +"Compiler bug [Type_Conversion, A]");
  Assert (j = 5,   +"Compiler bug [Type_Conversion, B]");
  --
  --  Duration <-> Real
  --
  d := 0.01;
  e := Duration (0.2 * 5.0);
  delay d;
  x := Real (d);
  if abs (x * 100.0 - Real (e)) > 100.0 * oa_duration_delta then
    Put_Line ("Type_Conversion, Compiler bug [C]");
    Put_Line (+"100 * x = " & 100.0 * x);
    Put_Line (+"e = " & Image (e));
    Failure (+"");
  end if;
end Type_Conversion;
