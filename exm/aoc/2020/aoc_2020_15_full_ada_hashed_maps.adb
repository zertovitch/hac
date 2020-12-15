--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
--  Full Ada version with hashed maps.
--  Total run time: 2.46 seconds (i5-9400 @ 2.9 GHz).
--
--  *But* actually, in this context, a hashed map is overkill
--  since the keys (the numbers) are in the range [0 .. stop] ...
--
with Ada.Calendar, Ada.Containers.Hashed_Maps, Ada.Text_IO, Ada.Integer_Text_IO;

procedure AoC_2020_15_full_Ada_hashed_Maps is

  type Preamble is array (Positive range <>) of Natural;
  --
  procedure Play (pre : Preamble) is
    use Ada.Calendar, Ada.Text_IO, Ada.Integer_Text_IO;

    function Identity_Hash (key : in Natural) return Ada.Containers.Hash_Type
    is (Ada.Containers.Hash_Type (key))
    with Inline;

    function Simple_Hash (key : in Natural) return Ada.Containers.Hash_Type
    with Inline
    is
      use Ada.Containers;
    begin
      return 2654435761 * Hash_Type (key);
    end Simple_Hash;
    pragma Unreferenced (Simple_Hash);

    package Number_Map_Pkg is new
      Ada.Containers.Hashed_Maps (
        Natural,   --  Key is the number spoken
        Positive,  --  Element is the turn (the index in a simple array)
        Identity_Hash,
        "=");

    mem : Number_Map_Pkg.Map;
    stop : constant := 30_000_000;
    prev, curr : Natural;
    T1, T2 : Time;
  begin
    T1 := Clock;
    for i in 1 .. pre'Last - 1 loop
      mem.Include (pre (i), i);
    end loop;
    prev := pre (pre'Last);
    --
    for i in pre'Last + 1 .. stop loop
      if mem.Contains (prev) then
        curr := (i - 1) - mem.Element (prev);  --  "Age"
      else
        curr := 0;
      end if;
      if i = 2020 or else i = stop then
        Put (i); Put (" : "); Put (curr, 0); New_Line;
      end if;
      mem.Include (prev, i - 1);
      prev := curr;
    end loop;
    T2 := Clock;
    Put_Line ("----   Computation time: " & Duration'Image (T2 - T1));
    New_Line;
  end Play;

begin
  --  Examples shown on https://adventofcode.com/2020/day/15 :
  Play ((0, 3, 6));  --  2020th number is 436;  30m-th number is 175594
  Play ((1, 3, 2));  --  2020th number is 1;    30m-th number is 2578
  Play ((2, 1, 3));  --  2020th number is 10;   30m-th number is 3544142
  Play ((1, 2, 3));  --  2020th number is 27;   30m-th number is 261214
  Play ((2, 3, 1));  --  2020th number is 78;   30m-th number is 6895259
  Play ((3, 2, 1));  --  2020th number is 438;  30m-th number is 18
  Play ((3, 1, 2));  --  2020th number is 1836; 30m-th number is 362
  --  The "real" puzzle:
  Play ((15, 12, 0, 14, 3, 1));
  --  ^ 2020th number is 249; 30m-th number is 41687
end AoC_2020_15_full_Ada_hashed_Maps;
