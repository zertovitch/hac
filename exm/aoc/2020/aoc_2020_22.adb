--  Solution to Advent of Code 2020, Day 22
-------------------------------------------
--  Crab Combat
--
--  https://adventofcode.com/2020/day/22
--
--  HAC 0.083 "nice to have"'s detected in this exercise:
--
--    *     comparison (equality operators) "=", "/=" of composite types (arrays and records)

with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

procedure AoC_2020_22 is
  use HAT;

  max_deck : constant := 50;

  type Cards is array (1 .. max_deck) of Natural;

  type Deck is record
    top  : Natural;
    card : Cards;
  end record;

  type Deck_Pair is array (1 .. 2) of Deck;

  verbosity : constant := 0;

  procedure Play (
    g               : in out Deck_Pair;
    winner          :    out Positive;
    is_recursive    : in     Boolean;
    recursion_level : in     Positive  --  For information only
  )
  is
    --  "The winner keeps both cards, placing them on the bottom
    --   of their own deck so that the winner's card is above
    --   the other card."
    procedure Move_Top_Cards (winner, loser : in out Deck) is
      top_card_winner, top_card_loser : Positive;
    begin
      top_card_winner := winner.card (winner.top);
      top_card_loser  := loser.card (loser.top);
      loser.top := loser.top - 1;
      winner.top := winner.top + 1;
      for i in reverse 3 .. winner.top loop
        winner.card (i) := winner.card (i - 2);
      end loop;
      winner.card (1) := top_card_loser;
      winner.card (2) := top_card_winner;
    end Move_Top_Cards;
    --
    sub : Deck_Pair;
    top_card : array (1 .. 2) of Positive;
    round_win : Positive;
    round : Natural := 0;
    type Game_Mem is array (1 .. 1000) of Deck_Pair;
    --  A size 100_000 is needed for the "real" data, otherwise
    --  we would not be able to memorize enough the past decks.
    --  But the needed stack for HAC would be huge and would
    --  slow down the initialization part of the interpreter.
    --  Better to switch to "full Ada" at this point.
    --  See AoC_2020_22_full_Ada (file: aoc_2020_22_full_ada.adb).
    mem : Game_Mem;
    --
    function Equal (g, h : Deck_Pair) return Boolean is
    begin
      for player in 1 .. 2 loop
        if g (player).top /= h (player).top then
          return False;
        end if;
        for i in 1 .. g (player).top loop
          if g (player).card (i) /= h (player).card (i) then
            return False;
          end if;
        end loop;
      end loop;
      return True;
    end Equal;
    --
  begin
    if verbosity > 1 and recursion_level > 6 then
      Put_Line (+"level=" & recursion_level);
    end if;
    loop
      round := round + 1;
      mem (round) := g;
      --
      --  Recursion breaker (first rule of Recursive Combat).
      --
      if is_recursive then
        for i in 1 .. round - 1 loop
          if Equal (mem (i), g) then
            winner := 1;
            return;
          end if;
        end loop;
      end if;
      --
      --  Draw cards.
      --
      for p in 1 .. 2 loop
        top_card (p) := g (p).card (g (p).top);
      end loop;
      if is_recursive and
         g (1).top - 1 >= top_card (1) and
         g (2).top - 1 >= top_card (2)
      then
        --  Copy parts of the decks for the sub-game.
        --
        --  "To play a sub-game of Recursive Combat, each player creates
        --   a new deck by making a copy of the next cards in their deck
        --   (the quantity of cards copied is equal to the number on the
        --   card they drew to trigger the sub-game)."
        for p in 1 .. 2 loop
          for i in 1 .. top_card (p) loop
            sub (p).card (i) := g (p).card (g (p).top - 1 + i - top_card (p));
          end loop;
          sub (p).top := top_card (p);
        end loop;
        Play (sub, round_win, is_recursive, recursion_level + 1);
      elsif top_card (1) > top_card (2) then
        round_win := 1;
      else
        round_win := 2;
      end if;
      if round_win = 1 then
        Move_Top_Cards (g (1), g (2));
      else
        Move_Top_Cards (g (2), g (1));
      end if;
      exit when g (1).top = 0 or g (2).top = 0;
    end loop;
    if g (1).top > 0 then
      winner := 1;
    else
      winner := 2;
    end if;
  end Play;
  --
  procedure Show (a : Deck) is
  begin
    for i in 1 .. a.top loop
      Put_Line (+"    card " & i & ":  " & a.card (i));
    end loop;
  end Show;
  --
  function Score (a : Deck) return Natural is
    res : Natural := 0;
  begin
    for i in 1 .. a.top loop
      res := res + i * a.card (i);
    end loop;
    return res;
  end Score;
  --
  procedure Read_Data (g : out Deck_Pair) is
    data : Deck_Pair;
    f : File_Type;
    s : VString;
    p : Natural := 0;
  begin
    Open (f, "aoc_2020_22.txt");
    while not End_Of_File (f) loop
      Get_Line (f, s);
      if Head (s, 6) = "Player" then
        p := p + 1;
        data (p).top := 0;
      elsif s = "" then
        null;
      else
        data (p).top := data (p).top + 1;
        data (p).card (data (p).top) := Integer_Value (s);
      end if;
    end loop;
    Close (f);
    --  Invert the deck: top <-> bottom
    for p in 1 .. 2 loop
      g (p).top := data (p).top;
      for i in 1 .. g (p).top loop
        g (p).card (i) := data (p).card (1 + data (p).top - i);
      end loop;
    end loop;
  end Read_Data;
  --
  winner : Positive;
  g, g_start : Deck_Pair;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
begin
  Read_Data (g_start);
  g := g_start;
  Play (g, winner, False, 1);
  if verbosity > 0 then
    Put_Line (+"Winner is: " & winner);
    Show (g (winner));
  end if;
  if compiler_test_mode then
    if Score (g (winner)) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Score: " & Score (g (winner)));
  end if;
  --  Part 1: validated by AoC: 31957
  --  Part 2: validated by AoC: 33212. Part skipped (see remarks around Game_Mem).
end AoC_2020_22;
