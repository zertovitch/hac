--  This test is to be used with the "-a" option from the HAC command-line executable.
--  We check here some optimizations, such as the removal of unnecessary range checks.

procedure Optim is

  subtype Chiffre is Integer range 1 .. 9;

  subtype Fuzzy is Integer range -100 .. 8;

  procedure Assignment_No_Checks is
    x : Chiffre;
  begin
    x := 1;  --  Low and high bound checks are optimized out.
    for j in 2 .. 9 loop
      x := j;  --  Low and high bound checks are optimized out.
    end loop;
  end;

  procedure Assignment_Upper_Bound_Check_Only (y : Positive) is
    x : Chiffre;
  begin
    x := y;  --  Low bound check is optimized out, high check remains.
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

begin
  i := 5;
end Optim;
