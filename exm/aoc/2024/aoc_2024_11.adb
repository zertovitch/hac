--  Solution to Advent of Code 2024, Day 11
-------------------------------------------
--  Plutonian Pebbles
--
--  https://adventofcode.com/2024/day/11
--  Copy of questions in: aoc_2024_11_questions.txt
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

with Interfaces;

procedure AoC_2024_11 is

  use AoC_Toolbox, HAT, Interfaces;

  use Hash_Maps;

  h : Hash_Map_Type;  --  Hash table with stone counts.

  d : constant Data_Type := input;

  verbosity_level : constant := 0;

  procedure Set_Data is
    procedure Set (i : Integer) is
      dummy : Integer_64;
    begin
      Insert (h, Image (i), 1, True, dummy);
    end Set;
  begin
    Clear (h);
    --  First example: "0 1 10 99 999"
    --  Second example: "125 17"
    --  Input: "3028 78 973951 5146801 5 0 23533 857"
    case d is
      when mini =>
        Set (125);
        Set (17);
      when input =>
        Set (3028);
        Set (78);
        Set (973951);
        Set (5146801);
        Set (5);
        Set (0);
        Set (23533);
        Set (857);
    end case;
  end Set_Data;

  r : array (Part_Type) of Integer_64;

  procedure Show is
    total : Integer_64 := 0;
  begin
      for i in h'Range loop
        for j in 1 .. h (i).slots loop
          if h (i).slot (j).value > 0 then
            Put (h (i).slot (j).key);
            if h (i).slot (j).value = 1 then
              Put (' ');
            else
              Put (+":" & h (i).slot (j).value'Image & "x ");
            end if;
            total := total + h (i).slot (j).value;
          end if;
        end loop;
      end loop;
      New_Line;
      Put_Line (+"Stones: " & total'Image);
  end Show;

  procedure Do_Part (part : Part_Type; rounds : Integer) is
    h2 : Hash_Map_Type;  --  Hash table with stone counts, after transformation.

    procedure Apply_Rules (s : in out Hash_Slot_Type) is
      l : Integer;
      s1, s2 : VString;

      procedure Transform (from, to : VString; remove : Boolean) is
        old, dummy : Integer_64;
      begin
        Find (h2, from, 0, old);
        if remove then
          Insert (h2, from, old - s.value, True, dummy);
          if verbosity_level > 1 then
            Put_Line (+"From " & from & " stones: " & old'Image & " -> " & Integer_64'Image (old - s.value));
          end if;
        else
          if verbosity_level > 1 then
            Put_Line (+"From " & from & " stones: " & old'Image);
          end if;
        end if;
        Find (h2, to, 0, old);
        Insert (h2, to, old + s.value, True, dummy);
        if verbosity_level > 1 then
          Put_Line (+"  To " & to & " stones: " & old'Image & " -> " & Integer_64'Image (old + s.value) & dummy'Image);
        end if;
      end Transform;

    begin
      if s.value > 0 then
        l := Length (s.key);
        if s.key = "0" then
          Transform (s.key, +"1", True);
        elsif l mod 2 = 0 then
          s1 := Slice (s.key, 1, l / 2);
          s2 := Slice (s.key, l / 2 + 1, l);
          while Length (s2) > 1 and then Element (s2, 1) = '0' loop
            Delete (s2, 1, 1);
          end loop;
          Transform (s.key, s1, True);
          Transform (s.key, s2, False);
        else
          s1 := +Integer_64'Image (Integer_64'Value (To_String (s.key)) * 2024);
          Delete (s1, 1, 1);
          Transform (s.key, s1, True);
        end if;
      end if;
    end Apply_Rules;

  begin
    if verbosity_level > 0 then
      Show;
    end if;

    h2 := h;

    for iter in 1 .. rounds loop
      --  Traverse the hash table for applying the rules to
      --  all stones:
      for i in h'Range loop
        for j in 1 .. h (i).slots loop
          Apply_Rules (h (i).slot (j));
        end loop;
      end loop;
      h := h2;
      if verbosity_level > 0 then
        Show;
      end if;
    end loop;

    --  Traverse the hash table for counting the stones:
    for i in h'Range loop
      for j in 1 .. h (i).slots loop
        r (part) := r (part) + h (i).slot (j).value;
      end loop;
    end loop;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Set_Data;
  Do_Part (part_1, 25);
  Do_Part (part_2, 50);

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1)))  or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put (+"Part 1:"); Put (r (part_1)'Image); New_Line;
    Put (+"Part 2:"); Put (r (part_2)'Image); New_Line;
    --  Part 1: validated by AoC: 198089
    --  Part 2: validated by AoC: 236302670835517
  end if;
end AoC_2024_11;
