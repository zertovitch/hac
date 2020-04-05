--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Floats is

  x1 : Real;
  x2 : Real;
  x3 : Real;

  type R is record
    x1 : Real;
    x2 : Real;
    x3 : Real;
  end record;

  ww : array (1 .. 7) of R;

  v : R;

  neg_float_value : constant := -5.07;

begin
  v.x1 := 1.0;
  v.x2 := 3.0;
  x3 := 5.0;
  v.x3 := 6.0;
  if x3 /= 5.0 then
    Put_Line ("Compiler bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= 1.0 then
    Put_Line ("Compiler bug [B]");
  end if;
  x3 := v.x3;
  if x3 /= 6.0 then
    Put_Line ("Compiler bug [C]");
  end if;
  ww (1).x3:= 3.456789;
  ww (5).x3:= ww (1).x3;
  ww (1).x3 := 7.89;
  v.x3 := 1.0;
  v.x2 := 2.0;
  if abs (ww (5).x3 - (2.345678 + 1.111111)) > 0.0000001 then
    Put_Line ("Compiler bug [D]");
  end if;
  x2 := neg_float_value;
  if -x2 /= 5.07 then
    Put_Line ("Compiler bug [E]");  --  Former HAC bug: unary minus was ineffeective for floats
  end if;
end Floats;
