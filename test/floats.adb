--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Floats is

  x1 : Float;
  x2 : Float;
  x3 : Float;

  type R is record
    x1 : Float;
    x2 : Float;
    x3 : Float;
  end record;

  ww : array (1 .. 7) of R;

  v : R;

begin
  v.x1 := 1.0;
  v.x2 := 3.0;
  x3 := 5.0;
  v.x3 := 6.0;
  if x3 /= 5.0 then
    Put_Line ("HAC Bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= 1.0 then
    Put_Line ("HAC Bug [B]");
  end if;
  x3 := v.x3;
  if x3 /= 6.0 then
    Put_Line ("HAC Bug [C]");
  end if;
  ww (1).x3:= 3.456789;
  ww (5).x3:= ww (1).x3;
  ww (1).x3 := 7.89;
  v.x3 := 1.0;
  v.x2 := 2.0;
  if ww (5).x3 /= 2.345678 + 1.111111 then
    Put_Line ("HAC Bug [D]");
  end if;
end Floats;
