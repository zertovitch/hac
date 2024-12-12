--  Solution to Advent of Code 2024, Day 1
------------------------------------------
--  Historian Hysteria
--
--  https://adventofcode.com/2024/day/1
--  Copy of questions in: aoc_2024_01_questions.txt
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

procedure AoC_2024_01 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 6;
  input_name : constant VString := +"aoc_2024_01"; n : constant := 1000;

  type List is array (1 .. n) of Natural;

  pair : array (1 .. 2) of List;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for i in List'Range loop
      Get (f, pair (1)(i));
      Get (f, pair (2)(i));
    end loop;
    Close (f);
  end Read_Data;

  procedure Shell_Sort (b : in out List) is
    i, j, step : Integer;
    step_size : array (1 .. 4) of Integer;
    stop : Boolean;
    temp : Integer;
  begin
    step_size (4) := 1;
    for pass in reverse 1 .. 3 loop
      step_size (pass) := 2 * step_size (pass + 1);
    end loop;
    for pass in 1 .. 4 loop
      step := step_size (pass);
      --  Do a straight insertion sort with 'step' as
      --  an increment instead of 1.
      i := step + 1;
      while i <= List'Last loop
        temp := b (i);
        j := i;
        stop := False;
        while j > step and not stop loop
          j := j - step;
          if b (j) > temp then
            b (j + step) := b (j);
          else
            b (j + step) := temp;
            stop := True;
          end if;
        end loop;
        if not stop then
          b (1) := temp;
        end if;
        i := i + step;
      end loop;
    end loop;
  end Shell_Sort;

  r : array (Part_Type) of Integer;

  procedure Do_Part_1 is
    score : Natural := 0;
  begin
    for i in List'Range loop
      --  Put (pair(1)(i)); Put_Line (pair(2)(i));
      score := score + abs (pair (1)(i) - pair (2)(i));
    end loop;
    r (part_1) := score;
  end Do_Part_1;

  procedure Do_Part_2 is
    score, count, n : Natural := 0;
    cache : array (1 .. 99_999) of Integer;
    not_counted : constant := -1;
  begin
    for i in List'Range loop
      cache (pair (1)(i)) := not_counted;
    end loop;
    for i in List'Range loop
      n := pair (1)(i);
      if cache (n) = not_counted then
        count := 0;
        for j in List'Range loop
          if pair (2)(j) = n then
            count := count + 1;
          end if;
        end loop;
        cache (n) := count;
      else
        count := cache (n);
      end if;
      score := score + n * count;
    end loop;
    r (part_2) := score;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Shell_Sort (pair (1));
  Shell_Sort (pair (2));
  Do_Part_1;
  Do_Part_2;
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
    --  Part 1: validated by AoC: 2086478.
    --  Part 2: validated by AoC: 24941624.
  end if;
end AoC_2024_01;
