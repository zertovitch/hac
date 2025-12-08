--  Solution to Advent of Code 2025, Day 3
------------------------------------------
--  Lobby
--
--  https://adventofcode.com/2025/day/3
--  Copy of questions in: aoc_2025_03_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory: ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2025.gpr .
with HAT;

procedure AoC_2025_03 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 15;
  input_name : constant VString := +"aoc_2025_03"; n : constant := 100;

  r : array (Part_Type) of VString;

  procedure Do_Part (part : Part_Type) is
    bank : String (1 .. n);
    f : File_Type;
    jolts, total, max_n, pos, d, digits_amount : Natural;
  begin
    total := 0;
    Open (f, input_name & ".txt");

    case part is
      when part_1 => digits_amount := 2;
      when part_2 => digits_amount := 12;
    end case;

    while not End_Of_File (f) loop
      Get (f, bank);
      pos := 1;
      jolts := 0;

      for rest in reverse 1 .. digits_amount loop
        max_n := 0;
        for k in pos .. n - rest + 1 loop
          --  We always need the highest possible n-th digit.
          --  And, the first occurrence of it is always the best: you have
          --  more choice for the next digit.
          d := Ord (bank (k)) - Ord ('0');
          if d > max_n then
            max_n := d;
            pos := k + 1;
          end if;
        end loop;
        jolts := jolts * 10 + max_n;
      end loop;

      total := total + jolts;
    end loop;

    r (part) := +"" & total;
    Close (f);
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Do_Part (part_1);
  Do_Part (part_2);
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 17613.
    --  Part 2: validated by AoC: 175304218462560.
  end if;
end AoC_2025_03;
