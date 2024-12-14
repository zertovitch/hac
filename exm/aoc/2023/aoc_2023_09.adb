--  Solution to Advent of Code 2023, Day 9
------------------------------------------
--  Mirage Maintenance
--
--  https://adventofcode.com/2023/day/9
--  Copy of questions in: aoc_2023_09_questions.txt
--

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_09 is

  use AoC_Toolbox, HAT;
  --
  --  input_name : constant VString := +"mini.txt"; m : constant := 3; n : constant := 6;
  input_name : constant VString := +"aoc_2023_09.txt"; m : constant := 200; n : constant := 21;
  type Row_Type is array (1 .. n) of Integer;
  data : array (1 .. m) of Row_Type;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name);
    for i in 1 .. m loop
      for j in 1 .. n loop
        Get (f, data (i)(j));
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  verbose : constant Boolean := False;

  procedure Do_Parts is
    check, val, left, right : Integer;
    last : Positive;
    --   ____
    --  |   /
    --  |  /  <- diff
    --  | /
    --  |/
    diff : array (0 .. n) of Row_Type;
  begin
    r (part_1) := 0;
    r (part_2) := 0;
    for i_data in 1 .. m loop
      for j in 1 .. n loop
        diff (0)(j) := data (i_data)(j);
      end loop;
      for round in 1 .. n - 1 loop
        check := 0;
        for j in 1 .. n - round loop
          val := diff (round - 1)(j + 1) - diff (round - 1)(j);
          diff (round)(j) := val;
          check := check + abs (val);
        end loop;
        last := round;
        exit when check = 0;
      end loop;
      left  := 0;
      right := 0;
      for i in reverse 0 .. last loop
        right := right + diff (i)(n - i);
        left  := diff (i)(1) - left;
      end loop;
      if verbose then
        --  Out of curiosity: we want to plot the data with the extrapolated values.
        Put (left);
        for j in 1 .. n loop
          Put (data (i_data)(j));
        end loop;
        Put (right);
        New_Line;
      end if;
      r (part_1) := r (part_1) + right;
      r (part_2) := r (part_2) + left;
    end loop;
  end Do_Parts;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Parts;
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
    --  Part 1: validated by AoC: 1884768153
    --  Part 2: validated by AoC: 1031
  end if;
end AoC_2023_09;
