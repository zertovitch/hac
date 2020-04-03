--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Integers is

  x1 : Integer;
  x2 : Integer;
  x3 : Integer;

  type R is record
    x1 : Integer;
    x2 : Integer;
    x3 : Integer;
  end record;

  v : R;

begin
  v.x1 := 1;
  v.x2 := 3;
  x3 := 5;
  v.x3 := 6;
  if x3 /= 5 then
    Put_Line ("HAC Bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= 1 then
    Put_Line ("HAC Bug [B]");
  end if;
  x3 := v.x3;
  if x3 /= 6 then
    Put_Line ("HAC Bug [C]");
  end if;
end Integers;
