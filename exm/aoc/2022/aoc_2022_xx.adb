--  Solution to Advent of Code 2022, Day $$
-------------------------------------------
--  $ puzzle title here!
--
--  https://adventofcode.com/2022/day/$
--  Copy of questions in: aoc_2022_$$_questions.txt

with AoC_Toolbox;

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

--  --  Interfaces is needed for compiling on both
--  --  HAC and GNAT (64-bit integer: Integer_64):
--  with Interfaces;

procedure AoC_2022_XX is
  --  use HAT, Interfaces;
  use AoC_Toolbox, HAT;

  verbosity_level : constant Natural := 0;

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

  type Storage is array (1 .. 100) of Character;
  
  type Stack is record
    top : Natural;
    s   : Storage;
  end record;
  
  sT : array (1 .. 9) of Stack;

  --

  subtype Range_x is Integer range 0 .. 500;
  subtype Range_y is Integer range 0 .. 500;

  lowest, highest : Point;

  procedure Adapt_Lowest_Value_Point (using : Point) is
  begin
    lowest.x := Min (lowest.x, using.x);
    lowest.y := Min (lowest.y, using.y);
  end Adapt_Lowest_Value_Point;

  procedure Adapt_Highest_Value_Point (using : Point) is
  begin
    highest.x := Max (highest.x, using.x);
    highest.y := Max (highest.y, using.y);
  end Adapt_Highest_Value_Point;

  map   : array (Range_x, Range_y) of Character;

  procedure Show is
  begin
    for y in lowest.y .. highest.y loop  --  y axis appears top -> down.
      for x in lowest.x .. highest.x loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

  procedure Clear is
  begin
    for y in Range_y loop
      for x in Range_x loop
        map (x, y) := ' ';
      end loop;
    end loop;
  end Clear;

  T0 : constant Time := Clock;
  r : array (Part_Type) of Integer;

begin
  r (part_1) := 0;
  r (part_2) := 0;
Parts :
  for part in part_1 .. part_1 loop
    Clear;
    lowest.x := 1;
    lowest.y := 0;
    highest.x := 1;
    highest.y := 0;
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
    if verbosity_level > 0 then
      Put_Line (+"bzz bzz ");
    end if;
    r (part) := 0;
  end loop Parts;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (part_1) /= Integer'Value (To_String (Argument (1))) or
       r (part_2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: bla bla:" & r (part_1)'Image);
    Put_Line (+"Part 2: bli bli:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 
    --  Part 2: validated by AoC: 
  end if;
end AoC_2022_XX;
