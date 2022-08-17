--  Solution to Advent of Code 2020, Day 10
-------------------------------------------
--  Adapter Array
--
--  https://adventofcode.com/2020/day/10
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2020_10 is
  use HAT, Interfaces;

  subtype Jolt is Natural;
  --  ^ Full Ada: it's better to define an incompatible
  --    type for Jolts:  `  type Jolt is new Natural;  `.
  Max_Adapters : constant := 1000;

  subtype Adapter_Range is Integer range 1 .. Max_Adapters;

  j : array (Adapter_Range) of Jolt;

  top : Natural := 0;
  jmax : Jolt := 0;
  verbose : constant Boolean := False;

  procedure Search (result : out Integer) is
    c : Jolt := 0;
    found : Boolean;
    diff_1, diff_3 : Natural := 0;
    seen : array (Adapter_Range) of Boolean;
    --
    function Check_all_used return Boolean is
      ok : Boolean := True;
    begin
      for i in 1 .. top loop
        ok := ok and seen (i);
      end loop;
      return ok;
    end Check_all_used;
    --
  begin
    for i in 1 .. top loop
      seen (i) := False;
    end loop;
    while c < jmax loop
      found := False;
      for step in 1 .. 3 loop
        for i in 1 .. top loop
          if not seen (i) then
            if j (i) = c + step then
              if step = 1 then diff_1 := diff_1 + 1; end if;
              if step = 3 then diff_3 := diff_3 + 1; end if;
              seen (i) := True;
              found := True;
              if verbose then
                Put_Line (
                  +"current " & c &
                   "  step " & step &
                   "  found adapter " & i &
                   " rated " & j (i) & " Jolts"
                );
              end if;
              c := j (i);
              exit;
            end if;
          end if;
        end loop;
        exit when found;
      end loop;
    end loop;
    if not Check_all_used then
      Put_Line ("Nooo - some adapters are not used!");
    end if;
    diff_3 := diff_3 + 1;  --  3 jolts from the last adapter to the device.
    result := diff_1 * diff_3;
  end Search;

  procedure Count_Possibilities (result : out Integer_64) is
    cache : array (Adapter_Range) of Integer_64;
    --
    function Count (from : Jolt) return Integer_64 is
      sum : Integer_64 := 0;
    begin
      if from = jmax then
        return 1;
      end if;
      for step in 1 .. 3 loop
        for i in 1 .. top loop
          if j (i) = from + step then
            if cache (i) = 0 then
              --  We compute only once the number of combinations
              --  for a given joltage level. Without cache, the
              --  computation would take an insane amount of time.
              cache (i) := Count (j (i));
            end if;
            sum := sum + cache (i);
          end if;
        end loop;
      end loop;
      return sum;
    end Count;
  begin
    for i in 1 .. top loop
      cache (i) := 0;
    end loop;
    result := Count (0);
  end Count_Possibilities;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  puzzle_1 : Jolt;
  puzzle_2 : Integer_64;
  f : File_Type;
begin
  Open (f, "aoc_2020_10.txt");
  while not End_Of_File (f) loop
    top := top + 1;
    Get (f, j (top));
    if j (top) > jmax then
      jmax := j (top);
    end if;
  end loop;
  Close (f);
  --
  Search (puzzle_1);
  Count_Possibilities (puzzle_2);
  if compiler_test_mode then
    if puzzle_1 /= Integer_Value (Argument (1)) or
       puzzle_2 /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Number of adapters: " & top);
    Put_Line (+"Max jolts: " & jmax);
    Put_Line (+"Result of puzzle 1 (diff_1 * diff_3 when using all adapters): " & puzzle_1);
    Put_Line (+"Result of puzzle 2 (number of ways adapters can be arranged):" & Integer_64'Image (puzzle_2));
    --  Part 1: validated by AoC: 2277
    --  Part 2: validated by AoC: 37024595836928
  end if;
end AoC_2020_10;
