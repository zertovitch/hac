--  Solution to Advent of Code 2024, Day 5
------------------------------------------
--  Print Queue
--
--  https://adventofcode.com/2024/day/5
--  Copy of questions in: aoc_2024_05_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_05 is

  use AoC_Toolbox, HAT;

  type Rule_Line is record
    a, b : Integer;
  end record;

  rule : array (1 .. 2000) of Rule_Line;
  last : Natural := 0;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_05";

  r : array (Part_Type) of Integer;

  procedure Read_Data (part : Part_Type) is
    dummy_separator : Character;
    f : File_Type;
    update : array (1 .. 100) of Natural;
    lu : Natural;
    already_correct, correct : Boolean;
    tmp : Integer;
  begin
    Open (f, input_name & ".txt");
    last := 0;

    while not End_Of_File (f) loop
      last := last + 1;
      Get (f, rule (last).a);
      Get (f, dummy_separator);
      Get (f, rule (last).b);
      if End_Of_Line (f) then
        Skip_Line (f);
        exit when End_Of_Line (f);  --  Empty line
      end if;
    end loop;

    lu := 0;
    while not End_Of_File (f) loop
      lu := lu + 1;
      Get (f, update (lu));
      if End_Of_Line (f) or End_Of_File (f) then
        already_correct := True;
        loop
          correct := True;
          Scan :
          for i in 1 .. lu - 1 loop
            for j in i + 1 .. lu loop
              --  put(update (i));
              --  put(update (j));
              --  new_Line;
              for r in 1 .. last loop
                if rule (r).b = update (i) and then rule (r).a = update (j) then
                  correct := False;
                  already_correct := False;
                  exit Scan when part = part_1;
                  --  Swap the pages
                  tmp := update (i);
                  update (i) := update (j);
                  update (j) := tmp;
                  exit Scan;
                end if;
              end loop;
            end loop;
          end loop Scan;
          exit when correct or already_correct or part = part_1;
        end loop;
        --  Put_Line(ok);
        if already_correct then
          if part = part_1 then
            r (part_1) := r (part_1) + update ((lu + 1) / 2);
          end if;
        elsif correct and then part = part_2 then
          r (part_2) := r (part_2) + update ((lu + 1) / 2);
        end if;
        lu := 0;  --  Clear data (update).
      else
        Get (f, dummy_separator);
      end if;
    end loop;

    Close (f);
  end Read_Data;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;

  Read_Data (part_1);
  if not compiler_test_mode then
    Read_Data (part_2);
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 5166
    --  Part 2: validated by AoC: 4679
  end if;
end AoC_2024_05;
