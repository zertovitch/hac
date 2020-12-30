--  This example contains a feature that was missing in HAC 0.01 but is present in HAC 0.02.
--
--  4.6 Type Conversions

with HAC_Pack; use HAC_Pack;

procedure Type_Conversion is

  function To_Int (x : Real) return Integer is
  begin
    return Integer (x);
  end;

  --  *Implicit* numerical type conversion was featured by HAC 0.01, but it's NOT Ada!
  --
  --  function Bogus return Real is
  --  begin
  --    return 666;
  --  end;

  i, j : Integer;
  x, y : Real;
  d, e : Duration;
  oa_duration_delta : constant := 0.00006103515625;
  --  OA Win 32 & 64: type Duration is delta 2.0**(-14) range -131072.0..+131072.0-2.0**(-14);
begin
  i := 1234;
  x := 1234.0;
  --
  --  *Implicit* numerical type conversion was featured by HAC 0.01, but it's NOT Ada!
  --
  --  j := To_Int (777);
  --  y := x + i;
  --  y := x / i;
  --  y := i / x;
  --  if 3 = 3.14 then null; end if;
  --  if 3.14 = 3 then null; end if;
  --  y := To_Int (x);
  --
  i := 1;
  x := 2.0;
  y := x + Real (i);       --  <-- Here is the wonderful feature!
  j := i + To_Int (3.51);  --  <-- Here is the wonderful feature!
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
