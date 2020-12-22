--  Solution to Advent of Code 2020, Day 22
-------------------------------------------
--  Crab Combat
--
--  https://adventofcode.com/2020/day/22
--
--  Full Ada version

with Ada.Unchecked_Deallocation;
with HAC_Pack;

procedure AoC_2020_22_full_Ada is

  max_deck : constant := 50;

  type Cards is array (1 .. max_deck) of Natural;

  type Deck is record
    top  : Natural;
    card : Cards;
  end record;

  type Deck_Pair is array (1 .. 2) of Deck;

  verbosity : constant := 0;

  use HAC_Pack;

  procedure Play (
    g               : in out Deck_Pair;
    winner          :    out Positive;
    is_recursive    : in     Boolean;
    recursion_level : in     Positive  --  For information only
  )
  is
    --
    procedure Get_Top (a, b : in out Deck) is  --  a gets from b
      top_a, top_b : Positive;
    begin
      top_a := a.card (a.top);
      top_b := b.card (b.top);
      b.top := b.top - 1;
      a.top := a.top + 1;
      for i in reverse 3 .. a.top loop
        a.card (i) := a.card (i - 2);
      end loop;
      a.card (1) := top_b;
      a.card (2) := top_a;
    end Get_Top;
    sub : Deck_Pair;
    top_card : array (1 .. 2) of Positive;
    round_win : Positive;
    round : Natural := 0;
    type Game_Mem is array (1 .. 100_000) of Deck_Pair;
    type p_Game_Mem is access Game_Mem;
    procedure Free is new Ada.Unchecked_Deallocation (Game_Mem, p_Game_Mem);
    mem : p_Game_Mem := new Game_Mem;
  begin
    if (verbosity > 1) and (recursion_level > 6) then
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
          if mem (i) = g then
            winner := 1;
            Free (mem);
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
         (g (1).top - 1 >= top_card (1)) and
         (g (2).top - 1 >= top_card (2))
      then
        --  Copy parts of the decks for the sub-game:
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
        Get_Top (g (1), g (2));
      else
        Get_Top (g (2), g (1));
      end if;
      exit when (g (1).top = 0) or (g (2).top = 0);
    end loop;
    if g (1).top > 0 then
      winner := 1;
    else
      winner := 2;
    end if;
    Free (mem);
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
