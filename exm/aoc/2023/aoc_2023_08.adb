--  Solution to Advent of Code 2023, Day 8
------------------------------------------
--  Haunted Wasteland
--
--  https://adventofcode.com/2023/day/8
--  Copy of questions in: aoc_2023_08_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

with Interfaces;

procedure AoC_2023_08 is

  use AoC_Toolbox, HAT, Interfaces;

  subtype Pos_Type is String (1 .. 3);

  type Pair is record left, right : Pos_Type; end record;

  subtype Para_Range is Integer range 1 .. 10;
  type Multi_Pos is array (Para_Range) of Pos_Type;

  starter : Multi_Pos;
  para_top : Natural := 0;
  direction_list : VString;
  map : array (Upcase_Alpha, Upcase_Alpha, Upcase_Alpha) of Pair;

  procedure Read_Data is
  --  input : constant VString := +"mini.txt";
    input : constant VString := +"aoc_2023_08.txt";
    --
    c : Character;
    pos, l, r : Pos_Type;
    fill_1 :  String (1 .. 4);
    fill_2 :  String (1 .. 2);
    f : File_Type;
  begin
    Open (f, input);
    Get_Line (f, direction_list);
    Skip_Line (f);
    while not End_Of_File (f) loop
      Get (f, pos);
      if pos (3) = 'A' then
        para_top := para_top + 1;
        starter (para_top) := pos;
      end if;
      Get (f, fill_1);
      Get (f, l);
      Get (f, fill_2);
      Get (f, r);
      map (pos (1), pos (2), pos (3)).left  := l;
      map (pos (1), pos (2), pos (3)).right := r;
      Get (f, c);
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer_64;

  procedure Do_Part_1 is
    c : Integer_64 := 0;
    i : Positive := 1;
    pos : Pos_Type;
  begin
    pos := "AAA";
    while pos /= "ZZZ" loop
      if Element (direction_list, i) = 'L' then
        pos := map (pos (1), pos (2), pos (3)).left;
      else
        pos := map (pos (1), pos (2), pos (3)).right;
      end if;
      i := i + 1;
      if i > Length (direction_list) then
        i := 1;
      end if;
      c := c + 1;
    end loop;
    r (part_1) := c;
  end Do_Part_1;

  procedure Do_Part_2_Brute_Force is
    c : Integer_64 := 0;
    i : Positive := 1;
    pos : Multi_Pos := starter;
    goal : Boolean;
    is_left : Boolean;
  begin
    loop
      goal := True;
      for p in 1 .. para_top loop
        goal := goal and then pos (p)(3) = 'Z';
        exit when not goal;
      end loop;
      exit when goal;
      is_left := Element (direction_list, i) = 'L';
      for p in 1 .. para_top loop
        if is_left then
          pos (p) := map (pos (p)(1), pos (p)(2), pos (p)(3)).left;
        else
          pos (p) := map (pos (p)(1), pos (p)(2), pos (p)(3)).right;
        end if;
      end loop;
      i := i + 1;
      if i > Length (direction_list) then
        i := 1;
      end if;
      c := c + 1;
    end loop;
    r (part_2) := c;
  end Do_Part_2_Brute_Force;

  cycle_observation_max : constant := 100_000;

  procedure Do_Part_2 is
    i : Positive;
    pos : Pos_Type;
    cycle : Integer_64 := 1;
    overall_cycle : Integer_64 := 1;
  begin
    for p in 1 .. para_top loop
      i := 1;
      pos := starter (p);
      for count in 1 .. cycle_observation_max loop
        if Element (direction_list, i) = 'L' then
          pos := map (pos (1), pos (2), pos (3)).left;
        else
          pos := map (pos (1), pos (2), pos (3)).right;
        end if;
        i := i + 1;
        if i > Length (direction_list) then
          i := 1;
        end if;
        if pos (3) = 'Z' then
          cycle := Integer_64 (count);
          exit;
        end if;
      end loop;
      overall_cycle := LCM_64 (overall_cycle, cycle);
    end loop;
    r (part_2) := overall_cycle;
  end Do_Part_2;

  verbose : constant Boolean := False;
  brute   : constant Boolean := False;

  procedure Show_Cycle (p : Para_Range) is
    c, old_c : Natural := 0;
    i : Positive := 1;
    pos : Pos_Type := starter (p);
    is_left : Boolean;
  begin
    for count in 1 .. cycle_observation_max loop
      if pos (3) = 'A' or pos (3) = 'Z' then
        Put_Line (+pos & "  " & c & "  delta: " & (c - old_c));
        old_c := c;
      end if;
      is_left := Element (direction_list, i) = 'L';
      if is_left then
        pos := map (pos (1), pos (2), pos (3)).left;
      else
        pos := map (pos (1), pos (2), pos (3)).right;
      end if;
      i := i + 1;
      if i > Length (direction_list) then
        i := 1;
      end if;
      c := c + 1;
    end loop;
  end Show_Cycle;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  Read_Data;

  Do_Part_1;

  if verbose then
    for p in 1 .. para_top loop
      Show_Cycle (p);
    end loop;
  end if;

  if brute then
    Do_Part_2_Brute_Force;
  else
    Do_Part_2;
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 12737
    --  Part 2: validated by AoC: 9064949303801
  end if;
end AoC_2023_08;
