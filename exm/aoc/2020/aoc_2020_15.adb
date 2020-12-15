--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_15 is
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  last : constant := 2020;
  mem : array (1 .. last) of Integer;
  --
  function Is_new (i, n : Natural) return Boolean is
  begin
    for j in 1 .. i - 2 loop
      if mem (j) = n then return False; end if;
    end loop;
    return True;
  end Is_new;
  --
  function Age (i, n : Natural) return Natural is
  begin
    for j in reverse 1 .. i - 2 loop
      if mem (j) = n then return (i - 1) - j; end if;
    end loop;
    return 0;
  end Age;
  --
  s : Positive;
begin
  mem (1) := 15;
  mem (2) := 12;
  mem (3) :=  0;
  mem (4) := 14;
  mem (5) :=  3;
  mem (6) :=  1;
  s := 7;
  for i in s .. last loop
    if Is_new (i, mem (i - 1)) then
      mem (i) := 0;
    else
      mem (i) := Age (i, mem (i - 1));
    end if;
  end loop;
  if compiler_test_mode then
    if mem (last) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put (mem (last));
  end if;
end AoC_2020_15;
