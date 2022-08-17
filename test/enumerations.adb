--  Output should be empty if the compiler is correct.

with HAT;
with Testing_Utilities;

procedure Enumerations is
  use HAT, Testing_Utilities;

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
        Assert (a = dog, +"Compiler bug [Enumerations, CASE]");
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
  Assert (x3 = e, +"Compiler bug [Enumerations, A]");
  x1 := v.x1;
  Assert (x1 = a, +"Compiler bug [Enumerations, B]");  --  Former HAC bug with selectors for enums
  x3 := v.x3;
  Assert (x3 = f, +"Compiler bug [Enumerations, C]");  --  Former HAC bug with selectors for enums
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
  Assert (ww (5).x3 = e, +"Compiler bug [Enumerations, D]");
  --
  Assert (zz (d).x4 = j, +"Compiler bug [Enumerations, E1]");
  Assert (zz (e).x4 = i, +"Compiler bug [Enumerations, E2]");
  Assert (zz (f).x4 = h, +"Compiler bug [Enumerations, E3]");
  --
  for pet in Mammal loop
    pet2 := pet;
    Assert (not (pet2 in Insect),     +"Compiler bug: Enumerations, membership 1 (IN)");
    Assert (not (pet2 not in Mammal), +"Compiler bug: Enumerations, membership 2 (NOT IN)");
  end loop;
  --
  Test_CASE;
  --
end Enumerations;
