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
end Floats;
