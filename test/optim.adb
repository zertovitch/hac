--  This test is to be used with the "-a" option from the HAC command-line executable:
--  hac -a optim.adb
--
--  We check here some optimizations, such as the removal of unnecessary range checks.

with HAL;

procedure Optim is

  subtype Chiffre is Integer range 1 .. 9;

  subtype Fuzzy is Integer range -100 .. 8;

  type Animal is (ant, bat, cat, dog);
  subtype Beast is Animal;
  subtype Insect is Animal range ant .. ant;
  subtype Mammal is Animal range bat .. dog;

  procedure Assignment_No_Checks is
    x : Chiffre;
    a : Animal;
    i : Insect;
  begin
    x := 1;  --  Low and high bound checks are optimized out.
    for j in 2 .. 9 loop
      x := j;  --  Low and high bound checks are optimized out.
    end loop;
    --  i := dog;  --  This issues (as it should) a compilation error.
    i := ant;
    a := i;
  end;

  procedure Assignment_Upper_Bound_Check_Only (y : Positive) is
    x : Chiffre;
    a : Animal;
    i : Insect;
  begin
    x := y;  --  Low bound check is optimized out, high check remains.
    HAL.Put_Line (Animal'Image (Animal'Last));
    HAL.Put_Line (Animal'Image (Insect'Last));
    a := cat;
    i := a;  --  HAC: run-time error on upper bound check.
             --  GNAT issues even a compile-time error!
  end;

  procedure Assignment_Lower_Bound_Check_Only (y : Fuzzy) is
    x : Chiffre;
  begin
    x := y;  --  High bound check is optimized out, low check remains.
  end;
  
  procedure Assignment_Both_Checks (y : Integer) is
    x : Chiffre;
  begin
    x := y;  --  Low and high bound checks remain.
  end;

  procedure Array_With_Check (y : Integer) is
    a : array (Chiffre) of Character;
  begin
    a (y) := 'x';
  end;

  procedure Array_No_Check (y : Chiffre) is
    a : array (Chiffre) of Character;
  begin
    a (y) := 'x';
    a (1) := 'a';
    for idx in 2 .. 4 loop
      a (idx) := 'a';
    end loop;
    for idx in Chiffre loop
      a (idx) := 'a';
    end loop;
    for idx in a'Range loop
      a (idx) := 'a';
    end loop;
  end;

  i : Integer;
  p : Positive;
  m5 : constant := -5;

begin
  p := -m5;
  p := -(-5);
  i := 5;
  --  i := -Integer'First;  --  This issues (as it should) a compilation error.
  Assignment_No_Checks;
  Assignment_Upper_Bound_Check_Only (Chiffre'Last);
end Optim;
