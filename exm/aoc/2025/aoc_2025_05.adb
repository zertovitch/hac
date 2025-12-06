--  Solution to Advent of Code 2025, Day 5
------------------------------------------
--  Cafeteria
--
--  https://adventofcode.com/2025/day/5
--  Copy of questions in: aoc_2025_05_questions.txt
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

procedure AoC_2025_05 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; ranges : constant := 4; ingredients : constant := 6;
  input_name : constant VString := +"aoc_2025_05"; ranges : constant := 187; ingredients : constant := 1000;

  from, to : array (1 .. ranges) of Integer;
  id : array (1 .. ingredients) of Integer;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. ranges loop
      Get (f, from (i));  --  NB: replaced all '-' by ' '.
      Get (f, to (i));
    end loop;
    for i in 1 .. ingredients loop
      Get (f, id (i));
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    count : Natural := 0;
  begin
    for i in 1 .. ingredients loop
      for j in 1 .. ranges loop
        if id (i) in from (j) .. to (j) then
          count := count + 1;
          exit;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & count;
  end Do_Part_1;

  enabled : array (1 .. ranges) of Boolean;

  procedure Do_Part_2 is
    modified : Boolean;
    count : Natural := 0;
  begin
    for i in 1 .. ranges loop
      enabled (i) := True;
    end loop;

    Merging :
    loop
      modified := False;
      for i in 1 .. ranges loop
        for j in 1 .. ranges loop
          if i /= j and then enabled (i) and then enabled (j) then
            if from (j) in from (i) .. to (i) then
              to (i) := Max (to (i), to (j));
              enabled (j) := False;
              modified := True;
            end if;
            if to (j) in from (i) .. to (i) then
              from (i) := Min (from (i), from (j));
              enabled (j) := False;
              modified := True;
            end if;
          end if;
        end loop;
      end loop;
      exit Merging when not modified;
    end loop Merging;

    for i in 1 .. ranges loop
      if enabled (i) then
        count := count + to (i) - from (i) + 1;
      end if;
    end loop;

    r (part_2) := +"" & count;

  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
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
    --  Part 1: validated by AoC: 798.
    --  Part 2: validated by AoC: 366181852921027.
  end if;
end AoC_2025_05;
