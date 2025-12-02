--  Solution to Advent of Code 2025, Day 2
------------------------------------------
--  .
--
--  https://adventofcode.com/2025/day/2
--  Copy of questions in: aoc_2025_02_questions.txt
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

with Interfaces;

procedure AoC_2025_02 is

  use AoC_Toolbox, HAT, Interfaces;

  input_name : constant VString := +"aoc_2025_02_mini";
  --  input_name : constant VString := +"aoc_2025_02";

  first, last : array (1 .. 100) of Integer_64;
  n : Natural := 0;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    sep : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      n := n + 1;
      Get (f, first (n));
      Get (f, sep);
      Get (f, last (n));
      exit when End_Of_File (f);
      Get (f, sep);
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part (part : Part_Type) is
    inv : Integer_64 := 0;

    procedure Check (x : Integer_64) is
      s : constant VString := Image (x);
      pattern : VString;
      t : VString;
      stop : Integer;
    begin
      for len in 1 .. 1 + Length (s) / 2 loop
        pattern := Slice (s, 1, len);
        t := +"";
        case part is
          when part_1 => stop := 2;
          when part_2 => stop := 1 + Length (s) / len;
        end case;
        for rep in 1 .. stop loop
          t := t & pattern;
          if rep >= 2 and then Length (t) = Length (s) and then t = s then
              inv := inv + x;
              return;
            end if;
          end loop;
        end loop;
    end Check;
  begin
    for i in 1 .. n loop
      for id in first (i) .. last (i) loop
        Check (id);
      end loop;
    end loop;
    r (part) := Image (inv);
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part (part_1);
  if compiler_test_mode then
    if r (part_1) /= Argument (1) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Do_Part (part_2);
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 55916882972 (mini: 1227775554).
    --  Part 2: validated by AoC: 76169125915 (mini: 4174379265).
  end if;
end AoC_2025_02;
