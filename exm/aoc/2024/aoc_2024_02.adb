--  Solution to Advent of Code 2024, Day 2
------------------------------------------
--  Red-Nosed Reports
--
--  https://adventofcode.com/2024/day/2
--  Copy of questions in: aoc_2024_02_questions.txt
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

procedure AoC_2024_02 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_02";

  r : array (Part_Type) of Integer;

  data : array (1 .. 100) of Integer;
  last : Natural := 0;

  function Check_with_Hole (hole : Natural) return Boolean is
    cols : Natural := 0;
    i, i1, d1, d2 : Integer := 0;
  begin
    for col in 1 .. last loop
      if col /= hole then
        cols := cols + 1;
        i1 := i;
        i := data (col);
        d2 := d1;
        d1 := i - i1;
        if cols >= 2 and then abs d1 not in 1 .. 3 then
          return False;
        end if;
        if cols >= 3 and then d1 * d2 < 0 then
          return False;
        end if;
      end if;
    end loop;
    return True;
  end Check_with_Hole;

  procedure Read_Data is
    f : File_Type;
    is_safe_part_2 : Boolean;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      last := last + 1;
      Get (f, data (last));

      if End_Of_Line (f) or End_Of_File (f) then
        if Check_with_Hole (0) then
          --  Report is safe without removing any level.
          r (part_1) := r (part_1) + 1;
          r (part_2) := r (part_2) + 1;
        else
          is_safe_part_2 := False;
          for c in 1 .. last loop
            if Check_with_Hole (c) then
              is_safe_part_2 := True;
              exit;
            end if;
          end loop;
          if is_safe_part_2 then
            r (part_2) := r (part_2) + 1;
          end if;
        end if;
        last := 0;  --  Clear data (report).
      end if;

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
    --  Part 1: validated by AoC: 332
    --  Part 2: validated by AoC: 398
  end if;
end AoC_2024_02;
