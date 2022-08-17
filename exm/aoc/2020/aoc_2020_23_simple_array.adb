--  Solution to Advent of Code 2020, Day 23
-------------------------------------------
--  Crab Cups
--
--  https://adventofcode.com/2020/day/23
--
--  Moving the cups to fill the gaps is very time-consuming
--  on large sizes (part 2 of the game).
--  See AoC_2020_23 for a much more efficient solution.
--
--  Total run time for both parts, example & input:
--  169 minutes (almost 3 hours) with
--  GNAT, AoC_Build_Mode_Type = "Fast", i5-9400 @ 2.9 GHz.
--
--  The linked list version takes 0.58 second on the same configuration.
--  Then, the linked list version is more than 17,000 times faster!

with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_23_Simple_Array is

  max : constant := 9;
  subtype Cup_Range is Positive range 1 .. max;
  type Cup_Array is array (Cup_Range) of Cup_Range;

  procedure Play (c_init : Cup_Array; part : Positive) is
    big_max : constant := 1_000_000;
    subtype Big_Cup_Range is Positive range 1 .. big_max;
    c : array (Big_Cup_Range) of Big_Cup_Range;
    c_cur : Big_Cup_Range;
    pick : array (1 .. 3) of Big_Cup_Range;
    dest, gap : Integer;
    found : Boolean;
    part_max, rounds : Positive;
  begin
    if part = 1 then
      part_max := max;
      rounds   := 100;
    else
      part_max := big_max;
      rounds   := 10_000_000;
    end if;
    for i in 1 .. max loop
      c (i) := c_init (i);
    end loop;
    for i in max + 1 .. part_max loop
      c (i) := i;
    end loop;
    --
    Game :
    for round in 1 .. rounds loop
      --  The crab picks up the three cups that are immediately
      --  clockwise of the current cup.
      for j in 1 .. 3 loop
        pick (j) := c (j + 1);
      end loop;
      --  They are removed from the circle; cup spacing is adjusted as
      --  necessary to maintain the circle.
      null;
      --  The crab selects a destination cup: the cup with a label equal
      --  to the current cup's label minus one.
      dest := c (1) - 1;
      loop
        --  If at any point in this process the value goes below the
        --  lowest value on any cup's label, it wraps around to the
        --  highest value on any cup's label instead.
        if dest < 1 then
          dest := part_max;
        end if;
        found := False;
        for i in 1 .. 3 loop
          if dest = pick (i) then
            found := True;
            exit;
          end if;
        end loop;
        exit when not found;
        --  If this would select one of the cups that was just picked up,
        --  the crab will keep subtracting one until it finds a cup that
        --  wasn't just picked up.
        dest := dest - 1;
      end loop;
      --  Remove & shift current focus by one (current slot is always 1)
      gap := 3;
      c_cur := c (1);
      for i in 1 .. part_max - 4 loop
        c (i + 3 - gap) := c (i + 4);
        if c (i) = dest then
          --  The crab places the cups it just picked up so that they are
          --  immediately clockwise of the destination cup. They keep the
          --  same order as when they were picked up.
          for j in 1 .. 3 loop
            c (i + j) := pick (j);
          end loop;
          gap := 0;
        end if;
      end loop;
      c (part_max) := c_cur;
    end loop Game;
    --
    if part = 1 then
      for i in 1 .. max loop
        if c (i) = 1 then
          for j in 1 .. max - 1 loop
            Put (c (1 + (i + j - 1) mod max), 0);
          end loop;
        end if;
      end loop;
    else
      for i in 1 .. big_max loop
        if c (i) = 1 then
          Put (c (1 + i mod big_max), 0);
          Put (", ");
          Put (c (1 + (i + 1) mod big_max), 0);
          exit;
        end if;
      end loop;
    end if;
    New_Line;
  end Play;

  exm, inp : Cup_Array;

begin
  --  example 389125467
  --  input   523764819
  --
  --  With full Ada we can write  ` exm := (3,8,9,1,2,5,4,6,7); `
  --
  exm (1) := 3;
  exm (2) := 8;
  exm (3) := 9;
  exm (4) := 1;
  exm (5) := 2;
  exm (6) := 5;
  exm (7) := 4;
  exm (8) := 6;
  exm (9) := 7;
  --
  inp (1) := 5;
  inp (2) := 2;
  inp (3) := 3;
  inp (4) := 7;
  inp (5) := 6;
  inp (6) := 4;
  inp (7) := 8;
  inp (8) := 1;
  inp (9) := 9;
  --
  for part in 1 .. 2 loop
    Put_Line (+"Part: " & part);
    Put ("  From example : "); Play (exm, part);
    --  Part 1: from AoC site:    67384529
    --  Part 2: from AoC site:    149245887792
    Put ("  From input   : "); Play (inp, part);
    --  Part 1: validated by AoC: 49576328
    --  Part 2: validated by AoC: 511780369955
  end loop;
end AoC_2020_23_Simple_Array;
