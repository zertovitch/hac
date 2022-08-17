--  Solution to Advent of Code 2021, Day 12
-------------------------------------------
--  Passage Pathing
--
--  https://adventofcode.com/2021/day/12
--  Copy of questions in: aoc_2021_12_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_12 is
  use HAT;
  --
  dic_max : constant := 30;
  subtype Dic_Range is Integer range 1 .. dic_max;
  subtype Segment_Range is Dic_Range;
  top, segs : Natural := 0;
  dic : array (Dic_Range) of VString;
  start_idx, end_idx : Dic_Range;
  --
  type Segment is record
    from : Dic_Range;
    to   : Dic_Range;
  end record;
  --
  seg : array (Segment_Range) of Segment;
  small : array (Dic_Range) of Boolean;
  seen : array (Dic_Range) of Natural;
  --
  procedure Read_Data is
    procedure Check (word : VString; pos : out Dic_Range) is
    begin
      for i in 1 .. top loop
        if dic (i) = word then
          pos := i;
          return;
        end if;
      end loop;
      --  Word not yet in dictionary.
      top := top + 1;
      dic (top) := word;
      small (top) := True;
      seen (top) := 0;
      for i in 1 .. Length (word) loop
        small (top) := small (top) and then Element (word, i) in 'a' .. 'z';
      end loop;
      pos := top;
    end Check;
    --
    input : constant VString := +"aoc_2021_12.txt";
    --
    c    : Character;
    word : VString;
    f    : File_Type;
  begin
    Check (+"start", start_idx);
    Check (+"end", end_idx);
    small (start_idx) := True;
    seen (start_idx) := 2;  --  This prevents visiting again the "start" cave.
    --
    Open (f, input);
    while not End_Of_File (f) loop
      segs := segs + 1;
      word := +"";
      loop
        Get (f, c);
        exit when c = '-';
        word := word & c;
      end loop;
      Check (word, seg (segs).from);
      word := +"";
      loop
        Get (f, c);
        word := word & c;
        exit when End_Of_Line (f);
      end loop;
      Check (word, seg (segs).to);
    end loop;
    Close (f);
  end Read_Data;

  --
  --  `Visit` has a side effect on `seen` array.
  --
  function Visit (cave : Dic_Range; joker : Boolean) return Natural is
    result : Natural;  --  Number of paths from this cave reaching the end.
    procedure Test (to : Dic_Range) is
    begin
      if small (to) then
        case seen (to) is
          when 0 =>
            result := result + Visit (to, joker);
          when 1 =>
            if joker then
              --  Continue the visit, but joker is discarded.
              result := result + Visit (to, False);
            end if;
          when others =>
            --  We cannot enter this `to` cave.
            null;
        end case;
      else
        --  Large cave: no limit!
        result := result + Visit (to, joker);
      end if;
    end Test;
  begin
    if cave = end_idx then
      return 1;
    end if;
    seen (cave) := seen (cave) + 1;
    --  As long as we don't find a way to the end,
    --  there is no interesting path from that cave:
    result := 0;
    for s in 1 .. segs loop
      --  We can go through a segment many times,
      --  But small caves have to be visited only once.
      --  In all the data we've have got (3 examples & input),
      --  there is no segment between two large caves.
      --  Such a situation would lead to infinite recursion.
      if cave = seg (s).from then
        Test (seg (s).to);
      elsif cave = seg (s).to then
        Test (seg (s).from);
      end if;
    end loop;
    seen (cave) := seen (cave) - 1;
    return result;
  end Visit;

  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
begin
  Read_Data;
  r (1) := Visit (start_idx, False);
  if not compiler_test_mode then
    r (2) := Visit (start_idx, True);
  end if;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: Number of paths (normal rules): " & r (1));
    Put_Line (+"Part 2: Number of paths (special rule): " & r (2));
    --  Part 1: validated by AoC: 3497
    --  Part 2: validated by AoC: 93686
  end if;
end AoC_2021_12;
