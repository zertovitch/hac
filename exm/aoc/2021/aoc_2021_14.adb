--  Solution to Advent of Code 2021, Day 14
-------------------------------------------
--  Extended Polymerization
--
--  https://adventofcode.com/2021/day/14
--  Copy of questions in: aoc_2021_14_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2021_14 is
  use HAT, Interfaces;
  --
  rule_max : constant := 200;
  subtype Rule_Range is Integer range 1 .. rule_max;
  --
  subtype Alpha is Character range 'A' .. 'Z';
  subtype Pair is String (1 .. 2);
  --  Ideally: `array (1 .. 2) of Alpha` when HAC supports array comparisons.
  --
  type Rule_Type is record
    from    : Pair;
    to      : Alpha;
    child_1 : Rule_Range;
    child_2 : Rule_Range;
  end record;
  --
  rule : array (Rule_Range) of Rule_Type;
  rules : Natural := 0;
  --
  initial_polymer : VString;
  type Pair_Population is array (Rule_Range) of Integer_64;
  zero : Pair_Population;
  gen_pop : array (1 .. 2) of Pair_Population;
  --
  procedure Read_Data is
    sep : String (1 .. 4);
    f : File_Type;
    input : constant VString := +"aoc_2021_14.txt";
  begin
    Open (f, input);
    Get_Line (f, initial_polymer);  --  Original sequence
    Skip_Line (f);
    while not End_Of_File (f) loop
      rules := rules + 1;
      Get (f, rule (rules).from (1));
      Get (f, rule (rules).from (2));
      Get (f, sep);
      Get (f, rule (rules).to);
    end loop;
    Close (f);
    --  Initial population as pairs
    for i in Rule_Range loop
      zero (i) := 0;
    end loop;
    gen_pop (1) := zero;
    for i in 1 .. rules loop
      for j in 1 .. Length (initial_polymer) - 1 loop
        if +rule (i).from = Slice (initial_polymer, j, j + 1) then
          gen_pop (1)(i) := gen_pop (1)(i) + 1;
        end if;
      end loop;
    end loop;
  end Read_Data;
  --
  procedure Prepare_Computation is
    child_1, child_2 : Pair;
  begin
    --  Prepare shortcuts pair -> new pairs.
    for i in 1 .. rules loop
      child_1 (1) := rule (i).from (1);
      child_1 (2) := rule (i).to;
      child_2 (1) := rule (i).to;
      child_2 (2) := rule (i).from (2);
      for j in 1 .. rules loop
        if +rule (j).from = +child_1 then
          rule (i).child_1 := j;
        end if;
        if +rule (j).from = +child_2 then
          rule (i).child_2 := j;
        end if;
      end loop;
    end loop;
  end Prepare_Computation;
  --
  gen : Positive := 1;
  --
  procedure Evolve is
    parents : Integer_64;
    new_gen : constant Positive := 3 - gen;
    child_1_idx, child_2_idx : Positive;
    procedure Increase (pop : in out Integer_64) is
    begin
      pop := pop + parents;
    end Increase;
  begin
    gen_pop (new_gen) := zero;
    for i in 1 .. rules loop
      parents := gen_pop (gen) (i);
      child_1_idx := rule (i).child_1;
      child_2_idx := rule (i).child_2;
      Increase (gen_pop (new_gen) (child_1_idx));
      Increase (gen_pop (new_gen) (child_2_idx));
    end loop;
    gen := new_gen;
  end Evolve;
  --
  r : array (1 .. 2) of Integer_64;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := not compiler_test_mode;
  --
  res : Integer_64;
  --
  procedure Atom_Counts is
    stat : array (Alpha) of Integer_64;
    total : Integer_64 := 0;
    aa : Alpha;
    stat_most_common_element  : Integer_64 := Integer_64'First;  --  "- infinity"
    stat_least_common_element : Integer_64 := Integer_64'Last;   --  "+ infinity"
  begin
    for a in Alpha loop
      stat (a) := 0;
    end loop;
    for r in 1 .. rules loop
      aa := rule (r).from (1);
      stat (aa) := stat (aa) + gen_pop (gen)(r);
      aa := rule (r).from (2);
      stat (aa) := stat (aa) + gen_pop (gen)(r);
    end loop;
    --  Ghost pair at the beginning of the polymer:
    aa := Element (initial_polymer, 1);
    stat (aa) := stat (aa) + 1;
    --  Ghost pair at the end of the polymer:
    aa := Element (initial_polymer, Length (initial_polymer));
    stat (aa) := stat (aa) + 1;
    --  Now all stats for all atomic symbols are exactly double counted.
    for a in Alpha loop
      stat (a) := stat (a) / 2;
      total := total + stat (a);
      if stat (a) > 0 and then verbose then
        Put_Line (+"  " & a & ":" & Integer_64'Image (stat (a)));
      end if;
    end loop;
    if verbose then
      Put_Line (+"  Total:" & Integer_64'Image (total));
    end if;
    for a in Alpha loop
      if stat (a) > 0 then
        if stat (a) < stat_least_common_element then
          stat_least_common_element := stat (a);
        end if;
        if stat (a) > stat_most_common_element then
          stat_most_common_element := stat (a);
        end if;
      end if;
    end loop;
    res := stat_most_common_element - stat_least_common_element;
    --
  end Atom_Counts;
  --
begin
  Read_Data;
  Prepare_Computation;
  if verbose then
    Put_Line (+"Initial: ");
  end if;
  Atom_Counts;
  for gen_count in 1 .. 40 loop
    Evolve;
    if verbose then
      Put_Line (+"Generation " & gen_count & ':');
    end if;
    Atom_Counts;
    if gen_count = 10 then
      r (1) := res;
    end if;
  end loop;
  r (2) := res;
  --
  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: count after 10 generations:" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2: count after 40 generations:" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 2345
    --  Part 2: validated by AoC: 2432786807053
  end if;
end AoC_2021_14;
