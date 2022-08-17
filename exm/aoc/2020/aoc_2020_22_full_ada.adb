--  Solution to Advent of Code 2020, Day 22
-------------------------------------------
--  Crab Combat
--
--  https://adventofcode.com/2020/day/22
--
--  Full Ada version
--
--  Run-time with GNAT, AoC_Build_Mode = "Fast":
--    *  4.45  seconds for a i5-9400 @ 2.9 GHz using a heap-allocated
--               fixed (100,000) size array for memorizing the previous decks
--    *  1.25  seconds for a i5-9400 @ 2.9 GHz using a vector
--    *  0.36  seconds for a i5-9400 @ 2.9 GHz using a hashed set

with Ada.Containers.Hashed_Sets,
     Ada.Containers.Vectors;

with HAT;
--  ^ Files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

procedure AoC_2020_22_full_Ada is

  max_deck : constant := 50;

  type Cards is array (1 .. max_deck) of Natural;

  type Deck is record
    top  : Natural;
    card : Cards;
  end record;

  type Deck_Pair is array (1 .. 2) of Deck;

  verbosity : constant := 0;

  use HAT;

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
    --
    function Equal (g, h : Deck_Pair) return Boolean
    with Inline
    is
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
    function Hash (g : Deck_Pair) return Ada.Containers.Hash_Type
    with Inline
    is
      use Ada.Containers;
      knuth : constant := 2654435761;
      res : Hash_Type := 1;
    begin
      for player in 1 .. 2 loop
        res := knuth * res + Hash_Type (g (player).top);
        for i in 1 .. g (player).top loop
          res := knuth * res + Hash_Type (g (player).card (i));
        end loop;
      end loop;
      return res;
    end Hash;
    --
    --  We benchmark Vectors (linear search) vs. Hashed_Sets.
    --
    type Memory_Type is (use_vectors, use_sets);
    mem_type_choice : constant Memory_Type := use_sets;
    --
    package Game_Mem_Vectors is new Ada.Containers.Vectors (Positive, Deck_Pair);
    mem_vec : Game_Mem_Vectors.Vector;
    --
    package Game_Mem_Sets is new Ada.Containers.Hashed_Sets (Deck_Pair, Hash, Equal, Equal);
    mem_set : Game_Mem_Sets.Set;
    --
  begin
    if verbosity > 1 and recursion_level > 6 then
      Put_Line (+"level=" & recursion_level);
    end if;
    loop
      round := round + 1;
      --
      --  Recursion breaker (first rule of Recursive Combat).
      --
      if is_recursive then
        case mem_type_choice is
          when use_vectors =>
            for mem of mem_vec loop
              if Equal (mem, g) then
                --  NB: the test `mem (i) = g` happens to work but is incorrect
                --      (compares cards above top)
                winner := 1;
                return;
              end if;
            end loop;
            mem_vec.Append (g);
          when use_sets =>
            if mem_set.Contains (g) then
              winner := 1;
              return;
            end if;
            mem_set.Include (g);
        end case;
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
begin
  Read_Data (g_start);
  for is_recursive in Boolean loop
    g := g_start;
    Play (g, winner, is_recursive, 1);
    if verbosity > 0 then
      Put_Line (+"Winner is: " & winner);
      Show (g (winner));
    end if;
    Put_Line (+"Score: " & Score (g (winner)));
  end loop;
  --  Part 1: Validated by AoC: 31957
  --  Part 2: Validated by AoC: 33212
end AoC_2020_22_full_Ada;
