--  Solution to Advent of Code 2021, Day 13
-------------------------------------------
--  Transparent Origami
--
--  https://adventofcode.com/2021/day/13
--  Copy of questions in: aoc_2021_13_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_13 is
  use HAT;
  --
  input : constant VString := +"aoc_2021_13.txt";
  --
  x_max : constant := 2000;
  y_max : constant := 1000;
  subtype X_Range is Integer range 0 .. x_max;
  subtype Y_Range is Integer range 0 .. y_max;
  x_last : X_Range := 0;
  y_last : Y_Range := 0;
  map : array (X_Range, Y_Range) of Boolean;
  --
  function Count return Natural is
    total : Natural := 0;
  begin
    for x in 0 .. x_last loop
      for y in 0 .. y_last loop
        if map (x, y) then
          total := total + 1;
        end if;
      end loop;
    end loop;
    return total;
  end Count;
  --
  procedure Show is
  begin
    for y in 0 .. y_last loop
      for x in 0 .. x_last loop
        if map (x, y) then
          Put ('#');
        else
          Put (' ');
        end if;
      end loop;
      New_Line;
    end loop;
  end Show;
  --
  f_max : constant := 20;
  subtype Fold_Range is Integer range 1 .. f_max;
  f_last : Natural := 0;
  type Fold_Instruction_Type is record
    x_axis : Boolean;
    line   : Natural;
  end record;
  fold_instruction : array (Fold_Range) of Fold_Instruction_Type;
  --
  procedure Read_Data is
    f : File_Type;
    c, sep : Character;
    xd : X_Range;
    yd : Y_Range;
    skip : String (1 .. 11) := "fold along ";
  begin
    for x in X_Range loop
      for y in Y_Range loop
        map (x, y) := False;
      end loop;
    end loop;
    Open (f, input);
    loop
      Get (f, xd);
      Get (f, sep);
      Get (f, yd);
      map (xd, yd) := True;
      x_last := Max (x_last, xd);
      y_last := Max (y_last, yd);
      if End_Of_Line (f) then
        Skip_Line (f);
      end if;
      exit when End_Of_Line (f);
    end loop;
    --  Folding instructions
    while not End_Of_File (f) loop
      f_last := f_last + 1;
      Get (f, skip);
      Get (f, c);
      fold_instruction (f_last).x_axis := c = 'x';
      Get (f, sep);
      Get (f, fold_instruction (f_last).line);
    end loop;
    Close (f);
  end Read_Data;
  --
  procedure Fold (f : Fold_Range) is
    xf, xe : X_Range;
    yf, ye : Y_Range;
  begin
    if fold_instruction (f).x_axis then
      xf := fold_instruction (f).line;
      xe := xf * 2;
      for x in 0 .. xf - 1 loop
        for y in 0 .. y_last loop
          map (x, y) := map (x, y) or map (xe - x, y);
        end loop;
      end loop;
      x_last := xf - 1;
    else
      yf := fold_instruction (f).line;
      ye := yf * 2;
      for x in 0 .. x_last loop
        for y in 0 .. yf - 1 loop
          map (x, y) := map (x, y) or map (x, ye - y);
        end loop;
      end loop;
      y_last := yf - 1;
    end if;
  end Fold;
  --
  r : Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
begin
  Read_Data;
  for f in 1 .. f_last loop
    Fold (f);
    if f = 1 then
      r := Count;
      exit when compiler_test_mode;
    end if;
  end loop;
  if compiler_test_mode then
    if r /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: Number of dots after 1st fold: " & r);
    Put_Line (+"Part 2: Height-letter code:");
    Show;
    --  Part 1: validated by AoC: 602
    --  Part 2: validated by AoC: CAFJHZCK
  end if;
end AoC_2021_13;
