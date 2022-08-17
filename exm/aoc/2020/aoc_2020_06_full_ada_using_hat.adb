--  Solution to Advent of Code 2020, Day 6
------------------------------------------
--  Custom Customs
--
--  https://adventofcode.com/2020/day/6
--
--  Full Ada version, using the HAT package.
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_06_Full_Ada_using_HAT is
  total : Integer;
  new_group : Boolean;
  subtype Answer_Range is Character range 'a' .. 'z';
  type Yes_Answer is array (Answer_Range) of Boolean;
  r, rg : Yes_Answer;
  --
  procedure Collect_Group_Total is
    g : Natural := 0;
  begin
    for c in Answer_Range loop if rg (c) then g := g + 1; end if; end loop;
    total := total + g;
    new_group := True;
  end Collect_Group_Total;
  --
  f : File_Type;
  s : VString;
begin
  for part in 1 .. 2 loop
    Open (f, "aoc_2020_06.txt");
    total := 0;
    new_group := True;
    while not End_Of_File (f) loop
      Get_Line (f, s);
      if s = "" then
        Collect_Group_Total;
      else
        for c in Answer_Range loop
          r (c) := Index (s, c) > 0;
        end loop;
        if new_group then
          rg := r;
          new_group := False;
        elsif part = 1 then
          rg := rg or r;
        else
          rg := rg and r;
        end if;
      end if;
    end loop;
    Collect_Group_Total;
    Put_Line (+"Part " & part & ".  Total customs answers: " & total);
    --  Part 1: officially validated by AoC: 6532
    --  Part 2: officially validated by AoC: 3427
    Close (f);
  end loop;
end AoC_2020_06_Full_Ada_using_HAT;
