--  Solution to Advent of Code 2024, Day 19
-------------------------------------------
--  Linen Layout
--
--  https://adventofcode.com/2024/day/19
--  Copy of questions in: aoc_2024_19_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

with Interfaces;

procedure AoC_2024_19 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini"; nt : constant := 8; nd : constant := 8;
  input_name : constant VString := +"aoc_2024_19"; nt : constant := 447; nd : constant := 400;

  towel  : array (1 .. nt) of VString;
  design : array (1 .. nd) of VString;

  max_len_design : constant := 60;
  type Memo_Type is array (1 .. max_len_design) of Integer_64;
  memo_clear : Memo_Type;  --  memo_clear: emulate `others =>`.

  unknown : constant := -1;

  r : array (Part_Type) of Integer_64;

  procedure Read_Data is
    c : Character;
    f : File_Type;
    s : VString;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. nt loop
      s := +"";
      loop
        Get (f, c);
        exit when c = ',';
        s := s & c;
        exit when End_Of_Line (f);
      end loop;
      if c = ',' then
        Get (f, c);  --  Consume ' ' after the ','.
      end if;
      towel (i) := s;
    end loop;
    Skip_Line (f, 2);
    for i in 1 .. nd loop
      Get_Line (f, design (i));
    end loop;
    Close (f);

    for i in memo_clear'Range loop
      memo_clear (i) := unknown;
    end loop;

  end Read_Data;

  procedure Solve_Method_Memoization_by_Index is
    memo : Memo_Type;

    function Count_Options (design : VString; first, length_design : Positive) return Integer_64 is
      length_towel, last : Natural;
      match : Boolean;
      options : Integer_64;

    begin

      if memo (first) >= 0 then
        return memo (first);
      end if;

      --  Number is unknown -> we have to compute it...

      options := 0;

      for i in 1 .. nt loop
        length_towel := Length (towel (i));
        last := first + length_towel - 1;
        if last <= length_design then
          match := Slice (design, first, last) = towel (i);
          if match then
            if last = length_design then
              options := options + 1;
            else
              options := options + Count_Options (design, last + 1, length_design);
            end if;
          end if;
        end if;
      end loop;

      memo (first) := options;
      return options;

    end Count_Options;

    opt : Integer_64;

  begin
    r (part_1) := 0;
    r (part_2) := 0;

    for i in 1 .. nd loop
      memo := memo_clear;
      opt := Count_Options (design (i), 1, Length (design (i)));
      if opt > 0 then
        r (part_1) := r (part_1) + 1;
        r (part_2) := r (part_2) + opt;
      end if;
    end loop;

  end Solve_Method_Memoization_by_Index;

  procedure Solve_Method_Triage_by_Initial is
    --  Lists of towels, bucketed by their initial:
    towel_after_initial : array (Character, 1 .. nt) of VString;
    --  Number of towels starting by the character in question:
    towels_with_initial : array (Character) of Natural;

    memo : Memo_Type;  --  memo_clear: emulate `others =>`.

    function Count_Options (design : VString; first, length_design : Positive) return Integer_64 is
      length_towel, last : Natural;
      match : Boolean;
      options : Integer_64;
      c : Character;

    begin

      if memo (first) >= 0 then
        return memo (first);
      end if;

      --  Number is unknown -> we have to compute it...

      options := 0;
      c := Element (design, first);

      for i in 1 .. towels_with_initial (c) loop
        length_towel := Length (towel_after_initial (c, i)) + 1;
        last := first + length_towel - 1;
        if last <= length_design then
          match := Slice (design, first + 1, last) = towel_after_initial (c, i);
          if match then
            if last = length_design then
              options := options + 1;
            else
              options := options + Count_Options (design, last + 1, length_design);
            end if;
          end if;
        end if;
      end loop;

      memo (first) := options;
      return options;

    end Count_Options;

    opt : Integer_64;
    c : Character;
    s : VString;

  begin

    for ini in Character loop
      towels_with_initial (ini) := 0;
    end loop;

    --  Prepare hashing by the towel's first character.
    for i in 1 .. nt loop
      s := towel (i);
      c := Element (s, 1);
      towels_with_initial (c) := towels_with_initial (c) + 1;
      towel_after_initial (c, towels_with_initial (c)) := Slice (s, 2, Length (s));
    end loop;

    r (part_1) := 0;
    r (part_2) := 0;

    for i in 1 .. nd loop
      memo := memo_clear;
      opt := Count_Options (design (i), 1, Length (design (i)));
      if opt > 0 then
        r (part_1) := r (part_1) + 1;
        r (part_2) := r (part_2) + opt;
      end if;
    end loop;

  end Solve_Method_Triage_by_Initial;

  procedure Solve_Method_Full_Hashing is
    use Hash_Maps;
    memo : Hash_Map_Type;

    function Count_Options (design : VString) return Integer_64 is
      length_design, length_towel : Natural;
      match : Boolean;
      options : Integer_64;

    begin

      Find (memo, design, unknown, options);
      if options /= unknown then
        return options;
      end if;

      --  Number is unknown -> we have to compute it...

      options := 0;
      length_design := Length (design);

      for i in 1 .. nt loop
        length_towel := Length (towel (i));
        if length_towel <= length_design then
          match := Slice (design, 1, length_towel) = towel (i);
          if match then
            if length_towel = length_design then
              options := options + 1;
            else
              options := options + Count_Options (Slice (design, length_towel + 1, length_design));
            end if;
          end if;
        end if;
      end loop;

      Insert (memo, design, options, False, options);
      return options;

    end Count_Options;

    opt : Integer_64;

  begin
    Clear (memo);
    r (part_1) := 0;
    r (part_2) := 0;

    for i in 1 .. nd loop
      opt := Count_Options (design (i));
      if opt > 0 then
        r (part_1) := r (part_1) + 1;
        r (part_2) := r (part_2) + opt;
      end if;
    end loop;

  end Solve_Method_Full_Hashing;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

  type Method is (m1, m2, m3);
  choice : constant Method := m2;

begin
  Read_Data;

  case choice is
    when m1 =>
      --  Simple method with memoization (used for sending the answer to AoC).
      --  Time HAC: 17.7 seconds; time GNAT (mode: Fast_Unchecked): 0.85 seconds.
      Solve_Method_Memoization_by_Index;

    when m2 =>
      --  Method with memoization and bucket triage by the towel's initial.
      --  Time HAC:  4.2 seconds; time GNAT (mode: Fast_Unchecked): 0.17 seconds.
      Solve_Method_Triage_by_Initial;

    when m3 =>
      --  Method with memoization and hashing of the design string (idea: J.C. Moyer,
      --  https://github.com/jcmoyer/puzzles/blob/master/AdventOfCode2024/src/day19.adb).
      --  Adapted to our HAC-compatible Hash Maps (slower).
      --  Time HAC: 16.9 seconds; time GNAT (mode: Fast_Unchecked): 0.89 seconds.
      --  J.C. Moyer's code, Release mode: 0.09 seconds.
      Solve_Method_Full_Hashing;

  end case;

  if compiler_test_mode then
    if Trim_Left (+r (part_1)'Image) /= Argument (1) or
       Trim_Left (+r (part_2)'Image) /= Argument (2)
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 260
    --  Part 2: validated by AoC: 639963796864990
  end if;
end AoC_2024_19;
