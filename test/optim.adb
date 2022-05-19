--  This test is to be used with the "-a" option from the HAC command-line executable.
--  We check here some optimizations, such as the removal of unnecessary range checks.

procedure Optim is

  subtype Chiffre is Integer range 1 .. 9;

  subtype Fuzzy is Integer range -100 .. 8;

  procedure Assignment_No_Checks is
    x : Chiffre;
  begin
    x := 1;  --  Low and high bound checks are optimized out.
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

  i : Integer;

begin
  i := 0;
end Optim;
