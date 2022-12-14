--  Solution to Advent of Code 2022, Day $$
-------------------------------------------
--  $ puzzle title here!
--
--  https://adventofcode.com/2022/day/$
--  Copy of questions in: aoc_2022_$$_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

--  --  Interfaces is needed for compiling on both
--  --  HAC and GNAT (64-bit integer: Integer_64):
--  with Interfaces;

procedure AoC_2022_XX is
  --  use HAT, Interfaces;
  use HAT;

  verbose : constant Boolean := True;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  c, sep : Character;
  asm : String (1 .. 3);
  i : Integer;
  f : File_Type;
  s : VString;
  bits : constant := 5;
  subtype Bit_Range is Integer range 1 .. bits;
  stat_ones : array (Bit_Range) of Natural;
  
  type Set is array (Character) of Boolean;
  group : array (0 .. 2) of Set;

  procedure Reset is
  begin
    for i in group'Range loop
      for c in Character loop
        group (i)(c) := False;
      end loop;
    end loop;
  end Reset;

  type Storage is array (1..100) of Character;
  
  type Stack is record
    top : Natural;
    s   : Storage;
  end record;
  
  sT : array (1 .. 9) of Stack;

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

  type Point is record
    x, y : Integer;
  end record;

  function Dist_Max (a, b : Point) return Natural is
  begin
    return
      Max (abs (a.x - b.x), abs (a.y - b.y));
  end Dist_Max;

  function Dist_L1 (a, b : Point) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y);
  end Dist_L1;

  procedure Maximize (p : in out Point; using: Point) is
  begin
    p.x := Max (p.x, using.x);
    p.y := Max (p.y, using.y);
  end Maximize;

  procedure Minimize (p : in out Point; using: Point) is
  begin
    p.x := Min (p.x, using.x);
    p.y := Min (p.y, using.y);
  end Minimize;

  nx_max : constant := 500;
  ny_max : constant := 500;

  origin : constant := 0;

  subtype Range_x is Integer range origin .. nx_max;
  subtype Range_y is Integer range origin .. ny_max;

  map   : array (Range_x, Range_y) of Character;

  m, n : Point;

  procedure Show is
  begin
    for y in Range_y'First .. n.y loop
      for x in Range_x'First .. n.x loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

begin
  r (1) := 0;
  r (2) := 0;
Parts :
  for part in 1 .. 2 loop
    Open (f, "mini.txt");  --  "input.txt");  --  aoc_2022_$$.txt
  Read_Data :
    while not End_Of_File (f) loop
      Get (f, asm);
      Get (f, i);
      Get (f, sep);
      Get (f, c);
      Get (f, sep);
      Get_Line (f, s);
    end loop Read_Data;
    Close (f);
    r (part) := 0;
  end loop Parts;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: bla bla:" & Integer'Image (r (1)));
    Put_Line (+"Part 2: bli bli:" & Integer'Image (r (2)));
    --  Part 1: validated by AoC: 
    --  Part 2: validated by AoC: 
  end if;
end AoC_2022_XX;
