--  Solution to Advent of Code 2025, Day $
-------------------------------------------
--  .
--
--  https://adventofcode.com/2025/day/$
--  Copy of questions in: aoc_2025_$$_questions.txt
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

  subtype Jolt_Digit is Character range '1' .. '9';

  procedure Do_Part_1 is
    bank : String (1 .. n);
    f : File_Type;
    max2, jolts, total : Natural;
  begin
    total := 0;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, bank);

      Search :
      for i in reverse Jolt_Digit loop
        for j in 1 .. n - 1 loop
          if bank (j) = i then
            max2 := 0;
            for k in j + 1 .. n loop
              max2 := Max (max2, Ord (bank (k)) - Ord ('0'));
            end loop;
            jolts := (Ord (i) - Ord ('0')) * 10 + max2;
            exit Search;
          end if;
        end loop;
      end loop Search;

      total := total + jolts;
    end loop;
    r (part_1) := +"" & total;
    Close (f);
  end Do_Part_1;

  procedure Do_Part_2 is
    bank : String (1 .. n);
    f : File_Type;
    jolts, total, max_n, pos, d : Natural;
  begin
    total := 0;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, bank);

      Search :
      for i in reverse Jolt_Digit loop
        for j in 1 .. n - 11 loop
          if bank (j) = i then
            jolts := (Ord (i) - Ord ('0'));
            pos := j + 1;
            for rest in reverse 1 .. 11 loop
              max_n := 0;
              for k in pos .. n - rest + 1 loop
                d := Ord (bank (k)) - Ord ('0');
                if d > max_n then
                  max_n := d;
                  pos := k + 1;
                end if;
              end loop;
              jolts := jolts * 10 + max_n;
            end loop;
            exit Search;
          end if;
        end loop;
      end loop Search;

      total := total + jolts;
    end loop;
    r (part_2) := +"" & total;
    Close (f);
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Do_Part_1;
  Do_Part_2;
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
