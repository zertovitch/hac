--  Solution to Advent of Code 2022, Day 20
-------------------------------------------
--  Grove Positioning System
--
--  https://adventofcode.com/2022/day/20
--  Copy of questions in: aoc_2022_20_questions.txt
--
--  This problem resembles to the Crab Cups puzzle (AoC_2020_23).

--
--  Note: this solution takes a large amount of time
--  with HAC (62 seconds).
--  Fortunately, you can compile it with GNAT and
--  the total run-time is there 0.29 seconds on an i7 machine.

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_20 is
  use HAT, Interfaces;

  verbose : constant Natural := 0;

  info_max : constant := 5000;

  subtype Extended_Info_Range is Integer range -1 .. info_max;
  subtype Info_Range is Integer range 0 .. info_max;

  type Info is record
    label : Integer;
    prev  : Extended_Info_Range;
    next  : Info_Range;
  end record;

  type Info_Array is array (Info_Range) of Info;

  data, o : Info_Array;
  last : Extended_Info_Range := -1;

  procedure Show is
    cursor : Info_Range;
  begin
    cursor := 0;
    for count in 0 .. last loop
      Put (o (cursor).label, 0);
      if count < last then
        Put (", ");
      end if;
      cursor := o (cursor).next;
    end loop;
    New_Line;
  end Show;

  f : File_Type;

  new_prev, new_next, total, zero_position, cursor : Info_Range;
  rounds : Positive;
  skip, decryption_key : Integer_64;

  r : array (1 .. 2) of Integer_64;
  T0 : constant Time := Clock;

begin
  Open (f, "aoc_2022_20.txt");
Read_Data :
  while not End_Of_File (f) loop
    last := last + 1;
    Get (f, data (last).label);
    if data (last).label = 0 then
      zero_position := last;
    end if;
    data (last).prev := last - 1;
    data (last).next := last + 1;
  end loop Read_Data;
  Close (f);
  data (0).prev := last;
  data (last).next := 0;
  total := last + 1;
  if verbose > 0 then
    Show;
  end if;

Parts :
  for part in 1 .. 2 loop
    o := data;
    --  Play:
    case part is
      when 1 =>
        rounds := 1;
        decryption_key := 1;
      when 2 =>
        rounds := 10;
        decryption_key := 811589153;
    end case;
  Multiple_Rounds :
    for round in 1 .. rounds loop
    Single_Round :
      for i in 0 .. last loop
        --  Remove node i:
        o (o (i).prev).next := o (i).next;
        o (o (i).next).prev := o (i).prev;
        --  Find the new neighbours for node i:
        new_prev := o (i).prev;
        skip := Integer_64 (o (i).label);
        if part = 2 then
          skip := skip * decryption_key;
        end if;
        skip := skip mod Integer_64 (total - 1);
        --  ^ Banana skin here: "mod (total - 1)" and not
        --    "mod total", because we have one node less!
        for count in 1 .. skip loop
          new_prev := o (new_prev).next;
        end loop;
        new_next := o (new_prev).next;
        --  Relink node i:
        o (i).next := new_next;
        o (i).prev := new_prev;
        --  Insert node i:
        o (new_prev).next := i;
        o (new_next).prev := i;
        --  Display:
        if verbose > 0 then
          Show;
        end if;
      end loop Single_Round;
    end loop Multiple_Rounds;
    cursor := zero_position;
    r (part) := 0;
    for thousands in 1 .. 3 loop
      for count in 1 .. 1000 mod total loop
        cursor := o (cursor).next;
      end loop;
      r (part) := r (part) + Integer_64 (o (cursor).label) * decryption_key;
    end loop;
  end loop Parts;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2:" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 2215
    --  Part 2: validated by AoC: 8927480683
  end if;
end AoC_2022_20;
