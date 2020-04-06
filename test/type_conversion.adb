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
  end if;
  if j /= 5 then
    Put_Line ("Compiler bug [B]");
  end if;
end;
