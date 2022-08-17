--  Solution to Advent of Code 2021, Day 21
-------------------------------------------
--  Dirac Dice
--
--  https://adventofcode.com/2021/day/21
--  Copy of questions in: aoc_2021_21_questions.txt
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     Exiting multiple nested loops
--    *     aggregates, like ` hits := (others => (others => 0)); `
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2021_21 is

  use Interfaces;

  r : array (1 .. 2) of Integer_64;

  subtype Player_Range is Integer range 0 .. 1;

  cells : constant := 10;

  subtype Cell_Range is Integer range 0 .. 9;  --  "full Ada": cells - 1

  procedure Play_Part_1 (start_player_1, start_player_2 : Positive) is
    score : array (Player_Range) of Natural;
    start : array (Player_Range) of Cell_Range;
    space, rolls : Natural;
    done : Boolean := False;
    win_score : constant := 1000;
  begin
    score (0) := 0;
    score (1) := 0;
    start (0) := start_player_1 - 1;  -- Start position, 0-based
    start (1) := start_player_2 - 1;  -- Start position, 0-based
    for round in 1 .. win_score loop  --  Worst case for any player: +1 point on each round.
      for playing in Player_Range loop
        space := 1 + (start (playing) + 9 * round ** 2 + (9 * playing - 3) * round) mod cells;
        score (playing) := score (playing) + space;
        if score (playing) >= 1000 then
          done := True;
          rolls := 3 * (1 + (round - 1) * 2 + playing);
          r (1) := Interfaces.Integer_64 (score (1 - playing) * rolls);
          exit;
        end if;
      end loop;
      exit when done;
    end loop;
  end Play_Part_1;

  subtype Dirac_Dice_Range is Integer range 3 .. 9;

  --  Number of combinations of 3 dice rolls for each outcome.
  dice_counts : array (Dirac_Dice_Range) of Positive;
  --  "full Ada": ` dice_counts ... := (1,3,6,7,6,3,1) `

  procedure Init_Dirac is
  begin
    dice_counts (3) := 1;  --  One combination: 1,1,1
    dice_counts (4) := 3;  --  3 combinations: 1,1,2, 1,2,1, 2,1,1
    dice_counts (5) := 6;
    dice_counts (6) := 7;
    dice_counts (7) := 6;
    dice_counts (8) := 3;
    dice_counts (9) := 1;
  end Init_Dirac;

  --  Universes wins counts for player 1 and 2.
  type Univs_Pair is array (1 .. 2) of Integer_64;

  procedure Play_Part_2 (start_player_1, start_player_2 : Positive) is
    win_score : constant := 21;
    subtype Score_Range is Integer range 0 .. win_score;
    --  Memoization of intermediate results, depending
    --  on the possible scores and positions:
    cache : array (Score_Range, Score_Range, Cell_Range, Cell_Range) of Univs_Pair;
    not_seen : constant := -1;
    --
    --  We simulate player A (1 or 2) rolling the dice then
    --  winning, or giving the hand to player B (2 or 1).
    --
    procedure Winning_Universes (
      score_A, score_B : in  Natural;
      pos_A,   pos_B   : in  Cell_Range;
      univs            : out Univs_Pair
    )
    is
      new_pos_A   : Cell_Range;
      new_score_A : Natural;
      other_play  : Univs_Pair;
    begin
      if cache (score_A, score_B, pos_A, pos_B)(1) /= not_seen then
        univs := cache (score_A, score_B, pos_A, pos_B);
        return;
      end if;
      univs (1) := 0;
      univs (2) := 0;
      --  Player A rolls the Dirac dices 3 times.
      --  We go through the possible outcomes, after the 3 draws:
      --  the parallel universes become only different
      --  depending on the sum of the 3 draws.
      for dirac_dice_3 in 3 .. 9 loop
        new_pos_A   := (pos_A + dirac_dice_3) mod cells;
        new_score_A := score_A + 1 + new_pos_A;  --  Our positions are 0-based.
        if new_score_A >= win_score then
          --  In this case, player A wins when reaching new_pos_A.
          --  It happens once if the numbers diced were 1, 1, 1;
          --  3 times if the numbers diced were 1, 1, 2 (in any order), etc.
          univs (1) := univs (1) + Integer_64 (dice_counts (dirac_dice_3));
        else
          --  Player A doesn't win, so it's player B's turn to play.
          Winning_Universes (
            score_B,
            new_score_A,
            pos_B,
            new_pos_A,
            other_play  --  = (#universes where B wins, #universes where A wins)
          );
          univs (1) := univs (1) + Integer_64 (dice_counts (dirac_dice_3)) * other_play (2);
          univs (2) := univs (2) + Integer_64 (dice_counts (dirac_dice_3)) * other_play (1);
        end if;
      end loop;
      --
      cache (score_A, score_B, pos_A, pos_B) := univs;
    end Winning_Universes;
    res : Univs_Pair;
  begin
    --  "full Ada": the following could be done in `cache` declaration
    --  with a (others => (others => (others => (others => not_seen))))
    for s1 in Score_Range loop
      for s2 in Score_Range loop
        for c1 in Cell_Range loop
          for c2 in Cell_Range loop
            cache (s1, s2, c1, c2)(1) := not_seen;
          end loop;
        end loop;
      end loop;
    end loop;
    --
    Winning_Universes (0, 0, start_player_1 - 1, start_player_2 - 1, res);
    r (2) := res (1);
    if res (2) > res (1) then r (2) := res (2); end if;
  end Play_Part_2;

  use HAT;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Init_Dirac;
  Play_Part_1 (7, 1);
  Play_Part_2 (7, 1);
  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2:" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 684495
    --  Part 2: validated by AoC: 152587196649184
  end if;
end AoC_2021_21;
