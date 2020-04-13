--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Enumerations is

  type E1 is (a);
  type E2 is (b, c);
  type E3 is (d, e, f);
  type E4 is (g, h, i, j);

  x1 : E1;
  x2 : E2;
  x3 : E3;

  type R is record
    x1 : E1;
    x2 : E2;
    x3 : E3;
    x4 : E4;
  end record;

  ww : array (1 .. 7) of R;

  v : R;

  zz : array (d .. f) of R;

begin
  v.x1 := a;
  v.x2 := c;
  x3 := e;
  v.x3 := f;
  if x3 /= e then
    Put_Line ("Compiler bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= a then
    Put_Line ("Compiler bug [B]");  --  Former HAC bug with selectors for enums
  end if;
  x3 := v.x3;
  if x3 /= f then
    Put_Line ("Compiler bug [C]");  --  Former HAC bug with selectors for enums
  end if;
  ww (1).x3:= e;
  ww (5).x3:= ww (1).x3;
  ww (1).x3 := f;
  --
  zz (d).x4 := j;
  zz (e).x4 := i;
  zz (f).x4 := h;
  --
  v.x3 := d;
  v.x2 := b;
  if ww (5).x3 /= e then
    Put_Line ("Compiler bug [D]");
  end if;
  --
  if zz (d).x4 /= j then Put_Line ("Compiler bug [E1]"); end if;
  if zz (e).x4 /= i then Put_Line ("Compiler bug [E2]"); end if;
  if zz (f).x4 /= h then Put_Line ("Compiler bug [E3]"); end if;
  --
  --  put (zz (d).x4);  --  = j (pos = 3)  --  !! should be available through 'Image & 'Pos
  --  put (zz (e).x4);  --  = i (pos = 2)  --  !! should be available through 'Image & 'Pos
  --  put (zz (f).x4);  --  = h (pos = 1)  --  !! should be available through 'Image & 'Pos
  --  new_line;
  --
  --  for var in e .. f loop
  --    put(var);
  --  end loop;
end Enumerations;
