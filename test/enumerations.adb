--  Output should be empty if the compiler is correct.

with HAL; use HAL;

procedure Enumerations is

  type E1 is (a);
  type E2 is (b, c);
  type E3 is (d, e, f);
  type E4 is (g, h, i, j);

  procedure Test_ARRAY is
    aa : array (E3) of E4;
  begin
    aa (e) := g;
  end Test_ARRAY;

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

  zz : array (E3) of R;

  type Animal is (ant, bat, cat, dog);

  pet2 : Animal;

  procedure Test_CASE is
    a : Animal := dog;
  begin
    case a is
      --  when d => null;  --  <-- wrong enum type
      when dog =>
        if a /= dog then
          Put_Line ("Compiler bug [CASE]");
          Set_Exit_Status (1);  --  Compiler test failed.
        end if;
      when others =>
        null;
    end case;
  end Test_CASE;

  subtype Beast is Animal;
  subtype Insect is Animal range ant .. ant;
  subtype Mammal is Animal range bat .. dog;

begin
  v.x1 := a;
  v.x2 := c;
  x3 := e;
  v.x3 := f;
  if x3 /= e then
    Put_Line ("Compiler bug [A]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  x1 := v.x1;
  if x1 /= a then
    Put_Line ("Compiler bug [B]");  --  Former HAC bug with selectors for enums
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  x3 := v.x3;
  if x3 /= f then
    Put_Line ("Compiler bug [C]");  --  Former HAC bug with selectors for enums
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  ww (1).x3 := e;
  ww (5).x3 := ww (1).x3;
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
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  if zz (d).x4 /= j then
    Put_Line ("Compiler bug [E1]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  if zz (e).x4 /= i then
    Put_Line ("Compiler bug [E2]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  if zz (f).x4 /= h then
    Put_Line ("Compiler bug [E3]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  for pet in Mammal loop
    pet2 := pet;
    if pet2 in Insect then
      Put ("Compiler bug membership 1");
    end if;
    if pet2 not in Mammal then
      Put ("Compiler bug membership 2");
    end if;
  end loop;
  --
  Test_CASE;
  --
end Enumerations;
