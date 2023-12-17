--  Solution to Advent of Code 2023, Day 4
------------------------------------------
--  Scratchcards
--
--  https://adventofcode.com/2023/day/4
--  Copy of questions in: aoc_2023_04_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_04 is
  use AoC_Toolbox, HAT;
  --
  r : array (Part_Type) of Integer;
  --
  procedure Read_Data is
    --  input : constant VString := +"mini.txt"; margin : constant := 8; winners : constant := 5; candidates : constant := 8;
    input : constant VString := +"aoc_2023_04.txt"; margin : constant := 10; winners : constant := 10; candidates : constant := 25;
    --
    sep : String (1 .. 3);
    card_id : String (1 .. margin);  --  A string like "Card 214: "
    winning : array (1 .. winners) of Positive;
    copies : array (0 .. winners) of Positive;
    n, won_1, won_2, cur, next : Natural;
    f : File_Type;
  begin
    Open (f, input);
    for j in 0 .. winners loop
      copies (j) := 1;
    end loop;
    cur := 0;
    while not End_Of_File (f) loop
      Get (f, card_id);
      for j in 1 .. winners loop
        Get (f, winning (j));
      end loop;
      Get (f, sep);
      won_1 := 0;
      won_2 := 0;
      for i in 1 .. candidates loop
        Get (f, n);
        for j in 1 .. winners loop
          if n = winning (j) then
            if won_1 = 0 then
              won_1 := 1;
            else
              won_1 := won_1 * 2;
            end if;
            won_2 := won_2 + 1;
          end if;
        end loop;
      end loop;
      r (part_1) := r (part_1) + won_1;
      r (part_2) := r (part_2) + copies (cur);
      --  Spread the copies of the cards:
      for i in 1 .. won_2 loop
        next := (cur + i) mod (winners + 1);
        copies (next) := copies (next) + copies (cur);
      end loop;
      copies (cur) := 1;
      cur := (cur + 1) mod (winners + 1);
    end loop;
    Close (f);
  end Read_Data;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 23750
    --  Part 2: validated by AoC: 13261850
  end if;
end AoC_2023_04;
