--  Solution to Advent of Code 2024, Day 3
------------------------------------------
--  Mull It Over
--
--  https://adventofcode.com/2024/day/3
--  Copy of questions in: aoc_2024_03_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_03 is

  use AoC_Toolbox, HAT;

  input_name : constant VString := +"aoc_2024_03";

  r : array (Part_Type) of Integer;

  procedure Read_Data (part : Part_Type) is
    i, j, a, b : Integer;
    f : File_Type;
    s : VString;
    enabled : Boolean := True;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get_Line (f, s);
      Scan_Line :
      loop
        Scan_Expression :
        for fake in 1 .. 1 loop  --  Just for the comfort of the EXIT statement.
          i := Index (s, "mul(");
          exit Scan_Line when i = 0;

          if part = part_2 then
            j := Index (s, "do()");
            if j in 1 .. i - 4 then
              enabled := True;
              Delete (s, 1, j + 3);
              exit Scan_Expression;
            end if;
            j := Index (s, "don't()");
            if j in 1 .. i - 7 then
              enabled := False;
              Delete (s, 1, j + 6);
              exit Scan_Expression;
            end if;
          end if;

          Delete (s, 1, i + 3);
          exit Scan_Expression when Length (s) = 0 or else Element (s, 1) not in '0' .. '9';
          a := 0;
          while Length (s) > 0 and then Element (s, 1) in '0' .. '9' loop
            a := a * 10 + Character'Pos (Element (s, 1)) - Character'Pos ('0');
            Delete (s, 1, 1);
          end loop;
          exit Scan_Expression when Length (s) = 0 or else Element (s, 1) /= ',';
          Delete (s, 1, 1);
          exit Scan_Expression when Length (s) = 0 or else Element (s, 1) not in '0' .. '9';
          b := 0;
          while Length (s) > 0 and then Element (s, 1) in '0' .. '9' loop
            b := b * 10 + Character'Pos (Element (s, 1)) - Character'Pos ('0');
            Delete (s, 1, 1);
          end loop;
          exit Scan_Expression when Length (s) = 0 or else Element (s, 1) /= ')';
          if enabled then
            r (part) := r (part) + a * b;
          end if;
        end loop Scan_Expression;
      end loop Scan_Line;
    end loop;
    Close (f);
  end Read_Data;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data (part_1);
  Read_Data (part_2);
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
    --  Part 1: validated by AoC: 173529487
    --  Part 2: validated by AoC: 99532691
  end if;
end AoC_2024_03;
