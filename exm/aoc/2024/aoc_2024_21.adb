--  Solution to Advent of Code 2024, Day 21
-------------------------------------------
--  Keypad Conundrum
--
--  https://adventofcode.com/2024/day/21
--  Copy of questions in: aoc_2024_21_questions.txt
--
--  HAC 0.40 "nice-to-have"'s detected in this exercise:
--
--    *     pass string literals directly!
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox, Interfaces;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_21 is

  use AoC_Toolbox, HAT, Interfaces;

  --  The following algorithm is adapted for HAC subset from J.C. Moyer's solution:
  --  https://github.com/jcmoyer/puzzles/blob/master/AdventOfCode2024/src/day21.adb
  --
  --  Differences from J.C.M.'s implementation:
  --     - Breadth First Search (BFS) algo for getting paths in keypads is
  --         replaced by a systematic search, "Manhattan-style" (there is no obstacle).
  --     - memoization (cache) is done via an array instead of a hash map.

  ----------------
  --  Commands  --
  ----------------

  --  A command is abstractly a single instruction for a robot to perform.
  --  Commands can be "lowered" in the code-gen sense to a sequence of
  --  commands for a robot controlling another robot.
  type Command is (c_west, c_east, c_north, c_south, c_press);

  ----------------------
  --  Numeric keypad  --
  ----------------------

  type Number_Pad_Key is (np_A, np_0, np_1, np_2, np_3, np_4, np_5, np_6, np_7, np_8, np_9);

  function Parse_Np_Key (c : Character) return Number_Pad_Key is
  begin
    case c is
      when '0' .. '9' =>
        return
          Number_Pad_Key'Val (Number_Pad_Key'Pos (np_0) + Character'Pos (c) - Character'Pos ('0'));
      when others =>
        return np_A;
    end case;
  end Parse_Np_Key;

  --  +---+---+---+
  --  | 7 | 8 | 9 |
  --  +---+---+---+
  --  | 4 | 5 | 6 |
  --  +---+---+---+
  --  | 1 | 2 | 3 |
  --  +---+---+---+
  --      | 0 | A |
  --      +---+---+

  procedure Position_Num (k : Number_Pad_Key; p : out Point) is
  begin
    case k is
      when np_7 => p.y := 1; p.x := 1;
      when np_8 => p.y := 1; p.x := 2;
      when np_9 => p.y := 1; p.x := 3;
      when np_4 => p.y := 2; p.x := 1;
      when np_5 => p.y := 2; p.x := 2;
      when np_6 => p.y := 2; p.x := 3;
      when np_1 => p.y := 3; p.x := 1;
      when np_2 => p.y := 3; p.x := 2;
      when np_3 => p.y := 3; p.x := 3;
      when np_0 => p.y := 4; p.x := 2;
      when np_A => p.y := 4; p.x := 3;
    end case;
  end Position_Num;

  --------------------------
  --  Directional keypad  --
  --------------------------

  type Directional_Pad_Key is (dp_left, dp_up, dp_down, dp_right, dp_A);

  function Command_To_Key (c : Command) return Directional_Pad_Key is
  begin
    case c is
      when c_east =>  return dp_right;
      when c_south => return dp_down;
      when c_west =>  return dp_left;
      when c_north => return dp_up;
      when c_press => return dp_A;
    end case;
  end Command_To_Key;

  --      +---+---+
  --      | ^ | A |
  --  +---+---+---+
  --  | < | v | > |
  --  +---+---+---+

  procedure Position_Dir (k : Directional_Pad_Key; p : out Point) is
  begin
    case k is
      when dp_up    => p.y := 1; p.x := 2;
      when dp_A     => p.y := 1; p.x := 3;
      when dp_left  => p.y := 2; p.x := 1;
      when dp_down  => p.y := 2; p.x := 2;
      when dp_right => p.y := 2; p.x := 3;
    end case;
  end Position_Dir;

  subtype Max_Pad_Range is Integer range 1 .. 4;

  gap_numpad, gap_dirpad : Point;

  max_robot_level : constant := 27;

  type Cache_Type is
    array
      (Max_Pad_Range,           --  Memoization of from.x
       Max_Pad_Range,           --  Memoization of from.y
       Max_Pad_Range,           --  Memoization of to.x
       Max_Pad_Range,           --  Memoization of to.y
       0 .. max_robot_level)    --  Memoization of recursion depth
    of Integer_64;

  cache, cache_clear : Cache_Type;

  function Lower_Commands
    (from, to, gap : Point;
     robot_count   : Integer;
     depth         : Integer)
  return Integer_64
  is
    --  Directional pad controlling us, which we will lower commands to.
    dpad_key, next_key : Directional_Pad_Key;

    sum : Integer_64 := 0;

    procedure Single_Press (new_cmd : Command) is
      p1, p2 : Point;
    begin
      Position_Dir (dpad_key, p1);
      next_key := Command_To_Key (new_cmd);
      Position_Dir (next_key, p2);
      sum := sum + Lower_Commands (p1, p2, gap_dirpad, robot_count, depth + 1);
      dpad_key := next_key;
    end Single_Press;

    cmd : Command;
    valid : Boolean;
    cur_pos : Point;
    min : Integer_64;
    dx, dy, lx, ly, l, n, hor : Integer;

    cached_val : constant Integer_64 := cache (from.x, from.y, to.x, to.y, depth);

  begin
    if cached_val >= 0 then
      return cached_val;
    end if;

    if depth = robot_count then
      --  Direct command
      min := 1;
    else
      min := Integer_64'Last;

      dx := Sgn (to.x - from.x);
      dy := Sgn (to.y - from.y);
      lx := abs (to.x - from.x);
      ly := abs (to.y - from.y);
      l := lx + ly;

      All_Paths :
      for comb in 0 .. 2 ** l - 1 loop
        n := comb;
        hor := 0;
        for count in 1 .. l loop
          if n mod 2 = 1 then
            --  Binary representation of `comb` has 1's for
            --  horizontal moves and 0's for vertical moves.
            hor := hor + 1;
          end if;
          n := n / 2;
        end loop;
        if hor = lx then
          --  Code `comb` represents the correct number of horizontal moves,
          --  then the number of vertical moves is also correct.
          sum  := 0;
          dpad_key := dp_A;
          n := comb;
          cur_pos := from;
          valid := True;

          Path :
          for count in 1 .. l loop
            if n mod 2 = 1 then
              --  Horizontal move
              if dx = 1 then
                cmd := c_east;
              else
                cmd := c_west;
              end if;
              cur_pos.x := cur_pos.x + dx;
            else
              --  Vertical move
              if dy = 1 then
                cmd := c_south;
              else
                cmd := c_north;
              end if;
              cur_pos.y := cur_pos.y + dy;
            end if;

            valid := Dist_L1 (cur_pos, gap) > 0;
            --  ^ "If a robot arm is ever aimed at a gap where no button is
            --     present on the keypad, even for an instant, the robot will
            --     panic unrecoverably. So, don't do that."

            exit Path when not valid;
            Single_Press (cmd);
            n := n / 2;
          end loop Path;
          if valid then
            --  Press "Press"!
            Single_Press (c_press);
            --  Take the minimum length of all lowered command sequences
            if sum < min then
              min := sum;
            end if;
          end if;

        end if;
      end loop All_Paths;
    end if;

    cache (from.x, from.y, to.x, to.y, depth) := min;

    return min;
  end Lower_Commands;

  subtype Digicode is String (1 .. 4);

  function Do_Code (s : Digicode; robot_count : Integer) return Integer_64 is
    npad_key, next_key : Number_Pad_Key := np_A;
    sum  : Integer_64 := 0;
    p1, p2 : Point;
    numeric_part : String (1 .. 3);
  begin
    for i in s'Range loop
      --  We start the search with numpad parameters, and it will recurse
      --  with dirpad parameters.
      Position_Num (npad_key, p1);
      next_key := Parse_Np_Key (s (i));
      Position_Num (next_key, p2);
      sum := sum + Lower_Commands (p1, p2, gap_numpad, robot_count, 0);
      npad_key := next_key;
    end loop;

    for i in numeric_part'Range loop
      numeric_part (i) := s (i);
    end loop;

    return Integer_64'Value (numeric_part) * sum;
  end Do_Code;

  r : array (Part_Type) of VString;

  data : constant Data_Type := input;

  procedure Do_Part (part : Part_Type) is
    score : Integer_64 := 0;
    robots : Positive;

    procedure Cumulate_Complexity (d : Digicode) is
    begin
      score := score + Do_Code (d, robots);
    end Cumulate_Complexity;

    d : Digicode;
  begin
    case part is
      when part_1 => robots := 3;
      when part_2 => robots := 26;
    end case;

    case data is
      when mini =>
        --  HAC 0.40 nice-to-have: pass string literals directly!
        d := "029A"; Cumulate_Complexity (d);
        d := "980A"; Cumulate_Complexity (d);
        d := "179A"; Cumulate_Complexity (d);
        d := "456A"; Cumulate_Complexity (d);
        d := "379A"; Cumulate_Complexity (d);

      when input =>
        d := "965A"; Cumulate_Complexity (d);
        d := "143A"; Cumulate_Complexity (d);
        d := "528A"; Cumulate_Complexity (d);
        d := "670A"; Cumulate_Complexity (d);
        d := "973A"; Cumulate_Complexity (d);
    end case;

    r (part) := +"" & Trim_Left (+score'Image);
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  gap_numpad.y := 4;
  gap_numpad.x := 1;

  gap_dirpad.y := 1;
  gap_dirpad.x := 1;

  --  "Full Ada 2012+" does the following in a single loop (for ... of).
  for i1 in Cache_Type'Range (1) loop
    for i2 in Cache_Type'Range (2) loop
      for i3 in Cache_Type'Range (3) loop
        for i4 in Cache_Type'Range (4) loop
          for i5 in Cache_Type'Range (5) loop
            cache_clear (i1, i2, i3, i4, i5) := -1;
          end loop;
        end loop;
      end loop;
    end loop;
  end loop;

  for part in Part_Type loop
    cache := cache_clear;
    Do_Part (part);
  end loop;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 222670
    --  Part 2: validated by AoC: 271397390297138
  end if;
end AoC_2024_21;
