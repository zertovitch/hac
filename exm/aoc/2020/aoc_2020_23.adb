--  Solution to Advent of Code 2020, Day 23
-------------------------------------------
--  Crab Cups
--
--  https://adventofcode.com/2020/day/23
--
--  In this version, we don't move the cups at all.
--  We redefine which one is the next.
--
--  Total run time for both parts, example & input:
--      0.58 second (GNAT AoC_Build_Mode_Type = "Fast", i5-9400 @ 2.9 GHz).
--    324.75 seconds (HAC, fastest build, same machine...).

with HAT; use HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

procedure AoC_2020_23 is

  max : constant := 9;
  subtype Cup_Range is Positive range 1 .. max;
  type Cup_Array is array (Cup_Range) of Cup_Range;

  function Play (c_init : Cup_Array; part : Positive) return Integer is
    big_max : constant := 1_000_000;

    subtype Big_Cup_Range is Positive range 1 .. big_max;

    type Cup_Info is record
      label : Big_Cup_Range;
      next  : Big_Cup_Range;
    end record;

    index_from_label : array (Cup_Range) of Big_Cup_Range;

    c : array (Big_Cup_Range) of Cup_Info;

    pick_label : array (1 .. 3) of Big_Cup_Range;
    current_index, cursor,
    pick_index, last_pick_index, after_pick_index,
    dest_index, dest_next_index : Big_Cup_Range := 1;
    --  ^ Initialization is just for removing a
    --    warning issued by the ObjectAda compiler.
    dest_label : Integer;
    found : Boolean;
    part_max, rounds : Positive;
    res : Natural := 0;
  begin
    if part = 1 then
      part_max := max;
      rounds   := 100;
    else
      part_max := big_max;
      rounds   := 10_000_000;
    end if;
    for i in 1 .. part_max loop
      if i <= max then
        c (i).label := c_init (i);
        index_from_label (c (i).label) := i;
      else
        c (i).label := i;
      end if;
      c (i).next  := 1 + i mod part_max;
    end loop;
    --
    current_index := 1;
    Game :
    for round in 1 .. rounds loop
      --  The crab picks up the three cups that are immediately
      --  clockwise of the current cup.
      pick_index := c (current_index).next;
      cursor := pick_index;
      for j in 1 .. 3 loop
        pick_label (j) := c (cursor).label;
        last_pick_index := cursor;
        cursor := c (cursor).next;
      end loop;
      after_pick_index := cursor;
      --  They are removed from the circle; cup spacing is adjusted as
      --  necessary to maintain the circle.
      c (current_index).next := after_pick_index;
      --  The crab selects a destination cup: the cup with a label equal
      --  to the current cup's label minus one.
      dest_label := c (current_index).label - 1;
      --
      loop
        --  If at any point in this process the value goes below the
        --  lowest value on any cup's label, it wraps around to the
        --  highest value on any cup's label instead.
        if dest_label < 1 then
          dest_label := part_max;
        end if;
        found := False;
        for i in 1 .. 3 loop
          if dest_label = pick_label (i) then
            found := True;
            exit;
          end if;
        end loop;
        exit when not found;
        --  If this would select one of the cups that was just picked up,
        --  the crab will keep subtracting one until it finds a cup that
        --  wasn't just picked up.
        dest_label := dest_label - 1;
      end loop;
      --
      if dest_label <= max then
        dest_index := index_from_label (dest_label);
      else
        dest_index := dest_label;
      end if;
      --  The crab places the cups it just picked up so that they are
      --  immediately clockwise of the destination cup. They keep the
      --  same order as when they were picked up.
      dest_next_index := c (dest_index).next;
      c (dest_index).next := pick_index;
      c (last_pick_index).next := dest_next_index;
      --
      current_index := c (current_index).next;
    end loop Game;
    --
    if part = 1 then
      for i in 1 .. max loop
        if c (i).label = 1 then
          cursor := i;
          for count in 1 .. max - 1 loop
            cursor := c (cursor).next;
            res := 10 * res + c (cursor).label;
          end loop;
        end if;
      end loop;
    else
      for i in 1 .. big_max loop
        if c (i).label = 1 then
          cursor := i;
          cursor := c (cursor).next;
          Put (c (cursor).label, 0);
          Put (", ");
          cursor := c (cursor).next;
          Put (c (cursor).label, 0);
          res := 0;
          --  ^ We could put there the product if we
          --    could prove Integer is 64 bit like with HAC.
          exit;
        end if;
      end loop;
    end if;
    return res;
  end Play;

  exm, inp : Cup_Array;
  res : Integer;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;

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
    if compiler_test_mode then
      if Play (exm, part) /= Integer_Value (Argument (1)) or
         Play (inp, part) /= Integer_Value (Argument (2))
      then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
      exit;
      --  ^ This is for HAC & compiler testing: we skip part 2, takes too long.
    else
      Put (+"Part: " & part & ": labels on the ");
      if part = 1 then
        Put_Line ("cups after cup 1");
      else
        Put_Line ("two cups that will end up immediately clockwise of cup 1");
      end if;
      Put ("  From example : ");
      res := Play (exm, part);
      if res > 0 then Put (res, 0); end if;
      New_Line;
      --  Part 1: from AoC site:    67384529
      --  Part 2: from AoC site:    149245887792 = 934001 * 159792
      Put ("  From input   : ");
      res := Play (inp, part);
      if res > 0 then Put (res, 0); end if;
      New_Line;
      --  Part 1: validated by AoC: 49576328
      --  Part 2: validated by AoC: 511780369955 = 760147 * 673265
    end if;
  end loop;
end AoC_2020_23;
