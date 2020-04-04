--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Enumerations is

  type E1 is (a);
  type E2 is (b, c);
  type E3 is (d, e, f);

  x1 : E1;
  x2 : E2;
  x3 : E3;

  type R is record
    x1 : E1;
    x2 : E2;
    x3 : E3;
  end record;

  ww : array (1 .. 7) of R;

  v : R;

begin
  v.x1 := a;
  v.x2 := c;
  x3 := e;
  v.x3 := f;
  if x3 /= e then
    Put_Line ("HAC Bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= a then
    Put_Line ("HAC Bug [B]");
  end if;
  x3 := v.x3;
  if x3 /= f then
    Put_Line ("HAC Bug [C]");
  end if;
  ww (1).x3:= e;
  ww (5).x3:= ww (1).x3;
  ww (1).x3 := f;
  v.x3 := d;
  v.x2 := b;
  if ww (5).x3 /= e then
    Put_Line ("HAC Bug [D]");
  end if;
end Enumerations;
