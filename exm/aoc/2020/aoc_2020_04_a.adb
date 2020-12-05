--  Solution to Advent of Code 2020, Day 04, Part One
-----------------------------------------------------
--  Passport Processing
--
--  https://adventofcode.com/2020/day/04
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_04_a is
  f : File_Type;
  s : VString;
  cats, total : Integer := 0;
begin
  Open (f, "aoc_2020_04.txt");
  while not End_Of_File (f) loop
    Get_Line (f, s);
    if s = "" then
      cats := 0;
    end if;
    if Index (s, "ecl:") > 0 then cats := cats + 1; end if;
    if Index (s, "pid:") > 0 then cats := cats + 1; end if;
    if Index (s, "eyr:") > 0 then cats := cats + 1; end if;
    if Index (s, "hcl:") > 0 then cats := cats + 1; end if;
    if Index (s, "byr:") > 0 then cats := cats + 1; end if;
    if Index (s, "iyr:") > 0 then cats := cats + 1; end if;
    if Index (s, "hgt:") > 0 then cats := cats + 1; end if;
    if cats = 7 then
      total := total + 1;
      --  Prevent incrementing total if there is garbage
      --  or a "cid:" until next blank line:
      cats := 0;
    end if;
  end loop;
  Close (f);
  Put_Line (total);
end AoC_2020_04_a;
