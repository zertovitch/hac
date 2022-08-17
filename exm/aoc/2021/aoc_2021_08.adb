--  Solution to Advent of Code 2021, Day 8
------------------------------------------
--  Seven Segment Search
--
--  https://adventofcode.com/2021/day/8
--  Copy of questions in: aoc_2021_08_questions.txt
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     Comparison (equality operators) "=", "/=" of
--            composite types (arrays and records)
--    *     Logical operators on arrays of Boolean
--
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_08 is
  use HAT;
  --
  input : constant VString := +"aoc_2021_08.txt";
  --
  r : array (1 .. 2) of Integer;
  --
  procedure Part_1 is
    c, sep : Character;
    f : File_Type;
    first_line : Boolean := True;
    size : Integer;
  begin
    --
    --  Part 1 : count unique patterns (2 segments for digit '1',
    --           3 for '7', 4 for '4', 7 for '8')
    --
    r (1) := 0;
    Open (f, input);
    first_line := True;
    while not End_Of_File (f) loop
      if first_line or End_Of_Line (f) then
        --  Skip the training message
        for i in 1 .. 61 loop
          Get (f, sep);
        end loop;
      end if;
      first_line := False;
      size := 0;
      loop
        Get (f, c);
        exit when c = ' ';
        size := size + 1;
        exit when End_Of_Line (f);
      end loop;
      if size = 2 or size = 3 or size = 4 or size = 7 then
        r (1) := r (1) + 1;
      end if;
    end loop;
    Close (f);
  end Part_1;
  --
  procedure Part_2 is
    --
    subtype Segment is Character range 'a' .. 'g';
    type Segment_Set is array (Segment) of Boolean;
    zero_set : Segment_Set;
    --
    f : File_Type;
    c, bar, spc : Character;
    --
    procedure Get_Segment (s : out Segment_Set) is
    begin
      s := zero_set;
      loop
        Get (f, c);
        exit when c = ' ';
        s (c) := True;
        exit when End_Of_Line (f);
      end loop;
    end Get_Segment;
    --
    function Count (s : Segment_Set) return Natural is
      total : Natural := 0;
    begin
      for c in Segment loop
        if s (c) then
          total := total + 1;
        end if;
      end loop;
      return total;
    end Count;
    --
    procedure Intersect (s, t : Segment_Set; s_and_t : out Segment_Set) is
    begin
      for c in Segment loop
        s_and_t (c) := s (c) and t (c);
      end loop;
    end Intersect;
    --
    function Equal (s, t : Segment_Set) return Boolean is
      res : Boolean := True;
    begin
      for c in Segment loop
        res := res and (s (c) = t (c));
        exit when not res;
      end loop;
      return res;
    end Equal;
    --
    subtype Digit_Range is Integer range 0 .. 9;
    subtype Output_Range is Integer range 1 .. 4;
    training : array (Digit_Range) of Segment_Set;
    temp, output   : Segment_Set;
    --  k = assoc (i) means: training (i) represents digit k.
    assoc : array (Digit_Range) of Integer;
    segs_to_index : array (1 .. 7) of Digit_Range;
    index_to_segs : array (Digit_Range) of Positive;
    first_line : Boolean := True;
    size : Integer;
    dig_it : Digit_Range;
    num : Natural;
  begin
    --
    --  Part 2 : decode the digits!
    --
    r (2) := 0;
    for s in Segment loop
      zero_set (s) := False;
    end loop;
    Open (f, input);
    first_line := True;
    while not End_Of_File (f) loop
      if first_line or End_Of_Line (f) then
        for i in Digit_Range loop
          Get_Segment (training (i));
          size := Count (training (i));
          segs_to_index (size) := i;
          index_to_segs (i) := size;
          case size is
            when 2 => assoc (i) := 1;
            when 3 => assoc (i) := 7;
            when 4 => assoc (i) := 4;
            when 7 => assoc (i) := 8;
            when 5 | 6 => assoc (i) := -1;
            when others => Put ("Error!");
          end case;
        end loop;
      end if;
      first_line := False;
      --
      --  Determine the 5- and 6-segment digits:
      --      _    _    _          _    _    _
      --      _|   _|  |_         | |  |_   |_|
      --     |_    _|   _|        |_|  |_|   _|
      --
      for i in Digit_Range loop
        case index_to_segs (i) is
          when 5 =>
            --  Intersect with the "4" (4 segments), and count the remaining segments.
            Intersect (training (i), training (segs_to_index (4)), temp);
            if Count (temp) = 2 then
              assoc (i) := 2;
            else
              --  Intersect with the "1" (2 segments), and count the remaining segments.
              Intersect (training (i), training (segs_to_index (2)), temp);
              if Count (temp) = 2 then
                assoc (i) := 3;
              else
                assoc (i) := 5;
              end if;
            end if;
          when 6 =>
            --  Intersect with the "4" (4 segments), and count the remaining segments.
            Intersect (training (i), training (segs_to_index (4)), temp);
            if Count (temp) = 4 then
              assoc (i) := 9;
            else
              --  Intersect with the "1" (2 segments), and count the remaining segments.
              Intersect (training (i), training (segs_to_index (2)), temp);
              if Count (temp) = 2 then
                assoc (i) := 0;
              else
                assoc (i) := 6;
              end if;
            end if;
          when others => null;
        end case;
      end loop;
      --
      Get (f, bar);  --  Absorb '|'
      Get (f, spc);  --  Absorb ' '
      num := 0;
      for i in Output_Range loop
        Get_Segment (output);
        size := Count (output);
        for i in Digit_Range loop
          if Equal (output, training (i)) then
            dig_it := assoc (i);
            exit;
          end if;
        end loop;
        num := num * 10 + dig_it;
      end loop;
      r (2) := r (2) + num;
    end loop;
    Close (f);
  end Part_2;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Part_1;
  Part_2;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: number of unique digit display patterns: " & r (1));
    Put_Line (+"Part 2: sum of decoded display numbers: " & r (2));
    --  Part 1: validated by AoC: 440
    --  Part 2: validated by AoC: 1046281
  end if;
end AoC_2021_08;
