--  Solution to Advent of Code 2025, Day 1
------------------------------------------
--  Secret Entrance
--
--  https://adventofcode.com/2025/day/1
--  Copy of questions in: aoc_2025_01_questions.txt
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

procedure AoC_2025_01 is

  use AoC_Toolbox, HAT;

  input_name : constant VString := +"aoc_2025_01";

  type Rotation_Instruction is record
    l_r    : Character;
    amount : Natural;
  end record;

  instr : array (1 .. 5000) of Rotation_Instruction;

  r : array (Part_Type) of VString;

  n : Natural := 0;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      n := n + 1;
      Get (f, instr (n).l_r);
      Get (f, instr (n).amount);
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    dial : Integer := 50;
    pwd  : Natural := 0;
  begin
    for i in 1 .. n loop
      if instr (i).l_r = 'L' then
        dial := dial - instr (i).amount;
      else
        dial := dial + instr (i).amount;
      end if;
      dial := dial rem 100;
      if dial = 0 then
        pwd := pwd + 1;
      end if;
    end loop;
    r (part_1) := +"" & pwd;
  end Do_Part_1;

  procedure Do_Part_2 is
    dial : Integer := 50;
    pwd  : Natural := 0;
  begin
    for i in 1 .. n loop
      --  Straightforward but unefficient method.
      --  However, who cares?
      --  Both part 1 & 2 complete in 0.7 second with HAC...
      for j in 1 .. instr (i).amount loop
        if instr (i).l_r = 'L' then
          dial := dial - 1;
        else
          dial := dial + 1;
        end if;
        dial := dial rem 100;
        if dial = 0 then
          pwd := pwd + 1;
        end if;
      end loop;
    end loop;
    r (part_2) := +"" & pwd;
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
    --  Part 1: validated by AoC: 989.
    --  Part 2: validated by AoC: 5941.
  end if;
end AoC_2025_01;
