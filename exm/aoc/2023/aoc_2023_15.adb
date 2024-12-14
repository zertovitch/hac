--  Solution to Advent of Code 2023, Day 15
-------------------------------------------
--  Lens Library
--
--  https://adventofcode.com/2023/day/15
--  Copy of questions in: aoc_2023_15_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_15 is

  use AoC_Toolbox, HAT;

  function HASH (s : VString) return Natural is
    h : Natural := 0;
  begin
    for i in 1 .. Length (s) loop
      h := ((h + Ord (Element (s, i))) * 17) rem 256;
    end loop;
    return h;
  end HASH;

  r : array (Part_Type) of Integer;
  data : VString;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2023_15";
  --
  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    Get_Line (f, data);
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    c : Character;
    s : VString;
  begin
    r (part_1) := 0;
    for i in 1 .. Length (data) loop
      c := Element (data, i);
      if  c = ',' then
        r (part_1) := r (part_1) + HASH (s);
        s := Null_VString;
      else
        s := s & c;
      end if;
    end loop;
    r (part_1) := r (part_1) + HASH (s);
  end Do_Part_1;

  verbosity : constant Natural := 0;  --  0, 1, 2

  procedure Do_Part_2 is
    --  On our data, there are 4000 rules, 530 distinct labels and
    --  239 used boxes, so there is an average of ~2.22 labels per box.
    --  Consequently, we choose a rigid array structure for storing
    --  lenses. We would use a linked list if there were much longer
    --  sequences per box.

    type Lens_Type is record
      label        : VString;
      focal_length : Positive;
    end record;

    type Slot_Array_Type is array (1 .. 5) of Lens_Type;

    type Box_Type is record
      slot  : Slot_Array_Type;
      slots : Natural;
      used  : Boolean;  --  For statistics in verbose mode: has had slots > 0 at least once.
    end record;

    box : array (0 .. 255) of Box_Type;
    --  Just for statistics in verbose mode:
    rules      : Natural := 0;
    used_boxes : Natural;
    used_slots : Natural;

    rule : VString;

    procedure Process is
      c : Character;
      lens : Lens_Type;
      i : Positive := 1;
      b : Natural;
      found : Boolean;
    begin
      rules := rules + 1;
      lens.label := Null_VString;
      loop
        c := Element (rule, i);
        exit when c not in 'a' .. 'z';
        lens.label := lens.label & c;
        i := i + 1;
      end loop;
      b := HASH (lens.label);
      if c = '-' then
        found := False;
        for s in 1 .. box (b).slots loop
          if box (b).slot (s).label = lens.label then
            found := True;
            for t in s .. box (b).slots - 1 loop
              box (b).slot (t) := box (b).slot (t + 1);
            end loop;
            box (b).slots := box (b).slots - 1;
            --  Lens with label was removed, we can safely leave the loop.
            exit;
          end if;
        end loop;
        if verbosity > 1 then
          if found then
            Put_Line ("Removed");
          else
            Put_Line ("Nothing to remove");
          end if;
        end if;
      else
        --  "=n" case.
        lens.focal_length := Integer_Value (Slice (rule, i + 1, Length (rule)));
        found := False;
        for s in 1 .. box (b).slots loop
          if box (b).slot (s).label = lens.label then
            found := True;
            box (b).slot (s).focal_length := lens.focal_length;
          end if;
        end loop;
        if found then
          if verbosity > 1 then
            Put_Line ("Replaced");
          end if;
        else
          if verbosity > 1 then
            Put_Line ("Added");
          end if;
          --  Append new lens.
          box (b).slots := box (b).slots + 1;
          box (b).slot (box (b).slots) := lens;
          box (b).used := True;
        end if;
      end if;
    end Process;

    c : Character;

  begin
    for b in box'Range loop
      box (b).slots := 0;
      box (b).used  := False;
    end loop;
    for i in 1 .. Length (data) loop
      c := Element (data, i);
      if  c = ',' then
        Process;
        rule := Null_VString;
      else
        rule := rule & c;
      end if;
    end loop;
    Process;

    if verbosity > 0 then
      used_boxes := 0;
      used_slots := 0;
      for b in box'Range loop
        if box (b).used then
          used_boxes := used_boxes + 1;
          used_slots := used_slots + box (b).slots;
        end if;
      end loop;
      Put_Line (+"Rules: " & rules);
      Put_Line (+"Boxes used: " & used_boxes);
      Put_Line (+"Slots used (distinct keys) on final state: " & used_slots);
    end if;

    r (part_2) := 0;
    for b in box'Range loop
      for s in 1 .. box (b).slots loop
        r (part_2) := r (part_2) +
          --  "focusing power":
          (1 + b) * s * box (b).slot (s).focal_length;
      end loop;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  Read_Data;
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
    --  Part 1: validated by AoC: 513172
    --  Part 2: validated by AoC: 237806
  end if;
end AoC_2023_15;
