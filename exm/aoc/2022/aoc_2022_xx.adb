--  Solution to Advent of Code 2022, Day $$
-------------------------------------------
--  $ puzzle title here! $
--
--  https://adventofcode.com/2022/day/$
--  Copy of questions in: aoc_2021_$$_questions.txt
--

--  For building this program with "full Ada":
--  files hat*.ad* are in ../../../src
with HAT;

--  Interfaces is needed for compiling on both HAC and
--  GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AOC_2022_XX is
  use HAT, Interfaces;

  c, sep : Character;
  asm : String (1 .. 3);
  i : Integer;
  f : File_Type;
  s : VString;
  bits : constant := 5;
  subtype Bit_Range is Integer range 1 .. bits;
  stat_ones : array (Bit_Range) of Natural;
  --
  function D2R (a : Real) return Real is
  begin
    return (Pi / 180.0) * a;
  end D2R;
  --
  procedure Rotate (x, y : in out Real; a : Real) is
    nx : Real;
  begin
    nx := Cos (a) * x - Sin (a) * y;
    y  := Sin (a) * x + Cos (a) * y;
    x  := nx;
  end Rotate;
  --
  input : constant VString := +"mini.txt";
  --  input : constant VString := +"aoc_2022_$$.txt";
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := True;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;
begin
  Open (f, input);
  while not End_Of_File (f) loop
    Get (f, asm);
    Get (f, i);
    Get (f, sep);
    Get (f, c);
    Get (f, sep);
    Get_Line (f, s);
  end loop;
  Close (f);
  if compiler_test_mode then
   if r (1) /= Integer_64'Value (To_String (Argument (1))) or
      r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & i);
    Put_Line (+"Part 2: : " & i);
    --  Part 1: validated by AoC:
    --  Part 2: validated by AoC:
  end if;
end AOC_2022_XX;
