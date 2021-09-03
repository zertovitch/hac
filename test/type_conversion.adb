--  This example contains a feature that was missing in HAC 0.01 but is present in HAC 0.02.
--
--  4.6 Type Conversions

with HAL; use HAL;

procedure Type_Conversion is

  function To_Int (x : Real) return Integer is
  begin
    return Integer (x);
  end;

  subtype Day is Integer range 1 .. 31;
  i, j : Integer;
  x, y : Real;
  d, e : Duration;
  oa_duration_delta : constant := 0.00006103515625;
  --  OA Win 32 & 64: type Duration is delta 2.0**(-14) range -131072.0..+131072.0-2.0**(-14);
  dd : Day;
begin
  i := 1234;
  x := Real (i);
  --  dd := Day (x);  --  <---- This should raise a Constraint_Error (not in range)
  --
  dd := 1;
  i := dd;
  x := 0.0 * x + 2.0;
  y := x + Real (i);
  j := i + To_Int (3.51);
  --
  if y /= 3.0 then
    Put_Line ("Compiler bug [A]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  if j /= 5 then
    Put_Line ("Compiler bug [B]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  --  Duration <-> Real
  --
  d := 0.01;
  e := Duration (0.2 * 5.0);
  delay d;
  x := Real (d);
  if abs (x * 100.0 - Real (e)) > 100.0 * oa_duration_delta then
    Put_Line ("Compiler bug [C]");
    Put_Line (+"100 * x = " & 100.0 * x);
    Put_Line (+"e = " & Image (e));
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
end Type_Conversion;
