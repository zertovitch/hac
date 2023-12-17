--  Solution to Advent of Code 2023, Day 7
------------------------------------------
--  Camel Cards
--
--  https://adventofcode.com/2023/day/7
--  Copy of questions in: aoc_2023_07_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_07 is
  use AoC_Toolbox, HAT;

  type Hand_Kind is (high, pair_1, pair_2, three, full, four, five);

  subtype Hand_Type is String (1 .. 5);

  type Hand_and_Bid_Type is record
    hand : Hand_Type;
    bid  : Positive;
    rank : Positive;
    kind : Hand_Kind;  --  Caching the result of the Kind function
    --                     on the hand field.
  end record;
  --
  row : array (1 .. 1000) of Hand_and_Bid_Type;
  top : Natural := 0;
  --
  procedure Read_Data is
    input : constant VString := +"aoc_2023_07.txt";
    f : File_Type;
  begin
    Open (f, input);
    while not End_Of_File (f) loop
      top := top + 1;
      Get (f, row (top).hand);
      Get (f, row (top).bid);
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  label : constant String (1 .. 13) := "AKQJT98765432";
  joker : constant := 4;  --  Just the position of 'J'.

  function Kind (h : Hand_Type; p : Part_Type) return Hand_Kind is
    count : array (1 .. 13) of Natural;
    sorts, count_max : Natural := 0;
  begin
    for i in count'Range loop
      count (i) := 0;
    end loop;
    for c in 1 .. 5 loop
      for i in count'Range loop
        if h (c) = label (i) then
          count (i) := count (i) + 1;
        end if;
      end loop;
    end loop;
    for i in count'Range loop
      if count (i) > 0 and then (p = part_1 or else i /= joker) then
        --  If the joker is used, we need *not* to count its sort!
        sorts := sorts + 1;
      end if;
      if p = part_2 and then i /= joker then
        count (i) := count (i) + count (joker);
      end if;
      count_max := Max (count_max, count (i));
    end loop;
    case count_max is
      when 5      => return five;
      when 4      => return four;
      when 3      => if sorts = 2 then return full; else return three; end if;
      when 2      => if sorts = 3 then return pair_2; else return pair_1; end if;
      when others => return high;
    end case;
  end Kind;

  --  Card labels for tie break of part 2, where the index' value matters.
  label_part_2 : constant String (1 .. 13) := "AKQT98765432J";

  function Stronger (h1, h2 : Hand_and_Bid_Type; p : Part_Type) return Boolean is
    --  NB: "not stronger" can mean "equal"...
    k1, k2 : Hand_Kind;
    index_1, index_2 : Positive;
    card : Character;
  begin
    k1 := h1.kind;
    k2 := h2.kind;
    if k1 > k2 then
      return True;
    elsif k1 < k2 then
      return False;
    else
      --  Tie: both hands are of the same kind.
      for c in 1 .. 5 loop
        for i in label'Range loop
          case p is
            when part_1 => card := label (i);
            when part_2 => card := label_part_2 (i);
          end case;
          if h1.hand (c) = card then
            index_1 := i;
          end if;
          if h2.hand (c) = card then
            index_2 := i;
          end if;
        end loop;
        if index_1 < index_2 then  --  The lower the index, the higher the value.
          return True;
        elsif index_1 > index_2 then
          return False;
        end if;
      end loop;
      Put ("Equal! ");  --  NB: equal hands don't happen on my data.
      return False;
    end if;
  end Stronger;

  procedure Do_Part (p : Part_Type) is
    t : Natural := 0;
  begin
    for i in 1 .. top loop
      row (i).kind := Kind (row (i).hand, p);
      row (i).rank := 1;
      for j in 1 .. i - 1 loop
        --  Compare to previous hands:
        if Stronger (row (i), row (j), p) then
          row (i).rank := row (i).rank + 1;
        else
          row (j).rank := row (j).rank + 1;
        end if;
      end loop;
    end loop;
    for i in 1 .. top loop
      t := t + row (i).rank * row (i).bid;
    end loop;
    r (p) := t;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  for part in Part_Type loop
    Do_Part (part);
  end loop;
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
    --  Part 1: validated by AoC: 252656917
    --  Part 2: validated by AoC: 253499763
  end if;
end AoC_2023_07;
