--  Solution to Advent of Code 2022, Day $$
-------------------------------------------
--  $ puzzle title here! $
--
--  https://adventofcode.com/2022/day/$
--  Copy of questions in: aoc_2021_$$_questions.txt
--

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AOC_2022_01 is
  use HAT;
  --
  f : File_Type;
  s : VString;
  input : constant VString := +"aoc_2022_01.txt";
  --
  sum, m1, m2, m3 : Integer := 0;

  procedure Add_To_Top_3 is
  begin
    if sum > m1 then
      m3 := m2;
      m2 := m1;
      m1 := sum;
    elsif sum > m2 then
      m3 := m2;
      m2 := sum;
    elsif sum > m3 then
      m3 := sum;
    end if;
  end Add_To_Top_3;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  --  verbose : constant Boolean := True;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
begin
  Open (f, input);
  while not End_Of_File (f) loop
    Get_Line (f, s);
    if s = "" then
      Add_To_Top_3;
      sum := 0;
    else
      sum := sum + Integer_Value (s);
    end if;
  end loop;
  Add_To_Top_3;
  Close (f);
  r (1) := m1;
  r (2) := m1 + m2 + m3;
  if compiler_test_mode then
   if r (1) /= Integer'Value (To_String (Argument (1))) or
      r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line
      (+"Part 1: Calories carried by the Elf with the most calories:" &
       Integer'Image (r (1)));
    Put_Line
      (+"Part 2: Calories carried by the top 3 . . . . . . . . . . :" &
       Integer'Image (r (2)));
    --  Part 1: validated by AoC: 68442
    --  Part 2: validated by AoC: 204837
  end if;
end AOC_2022_01;
