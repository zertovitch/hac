--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
with Ada.Containers.Hashed_Maps, Ada.Text_IO, Ada.Integer_Text_IO;

procedure AoC_2020_15_full_Ada is

  function No_Hash (key : in Natural) return Ada.Containers.Hash_Type
  is (Ada.Containers.Hash_Type (key));

  package Addr_Map_Pkg is new
    Ada.Containers.Hashed_Maps (Natural, Positive, No_Hash, "=");

  mem : Addr_Map_Pkg.Map;
  start : constant := 7;
  last  : constant := 30000000;
  prev, hold : Natural;
  use Ada.Text_IO, Ada.Integer_Text_IO;
begin
  mem.Include (15, 1);
  mem.Include (12, 2);
  mem.Include  (0, 3);
  mem.Include (14, 4);
  mem.Include  (3, 5);
  prev       := 1;
  --
  for i in start .. last loop
    if mem.Contains (prev) then
      hold := (i - 1) - mem.Element (prev);
    else
      hold := 0;
    end if;
    if i = 2020 or else i = last then
      Put (i); Put (" : "); Put (hold, 0); New_Line;
    end if;
    mem.Include (prev, i - 1);
    prev := hold;
  end loop;
end AoC_2020_15_full_Ada;
