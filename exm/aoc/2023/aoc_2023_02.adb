--  Solution to Advent of Code 2023, Day 2
------------------------------------------
--  Cube Conundrum
--
--  https://adventofcode.com/2023/day/2
--  Copy of questions in: aoc_2023_02_questions.txt

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_02 is
  use HAT;

  r : array (1 .. 2) of Integer;

  procedure Read_Data is
    input : constant VString := +"aoc_2023_02.txt";
    --
    c, sep : Character;
    game : String (1 .. 4);  --  The string "Game".
    id, n : Integer;
    f : File_Type;
    s : VString;
    valid : Boolean;
    max_r, max_g, max_b : Integer;
  begin
    Open (f, input);
    while not End_Of_File (f) loop
      valid := True;
      max_r := 0;
      max_g := 0;
      max_b := 0;
      Get (f, game);
      Get (f, id);
      Get (f, sep);
      while not (End_Of_File (f) or else End_Of_Line (f)) loop
        Get (f, n);
        Get (f, sep);
        s := +"";
        while not End_Of_Line (f) loop
          Get (f, c);
          exit when c = ',' or c = ';';
          s := s & c;
        end loop;
        if s = "red" then
          valid := valid and n <= 12;
          max_r := Max (max_r, n);
        elsif s = "green" then
          valid := valid and n <= 13;
          max_g := Max (max_g, n);
        elsif s = "blue" then
          valid := valid and n <= 14;
          max_b := Max (max_b, n);
        end if;
      end loop;
      if valid then
        r (1) := r (1) + id;
      end if;
      r (2) := r (2) + max_r * max_g * max_b;
    end loop;
    Close (f);
  end Read_Data;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (1) := 0;
  r (2) := 0;
  Read_Data;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & r (1));
    Put_Line (+"Part 2: : " & r (2));
    --  Part 1: validated by AoC: 2176
    --  Part 2: validated by AoC: 63700
  end if;
end AoC_2023_02;
