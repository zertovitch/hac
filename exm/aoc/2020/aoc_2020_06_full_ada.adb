--  Solution to Advent of Code 2020, Day 6
------------------------------------------
--  Custom Customs
--
--  https://adventofcode.com/2020/day/6
--
--  Full Ada version.
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_06_Full_Ada is
  total : Integer;
  new_group : Boolean;
  subtype Ans is Character range 'a' .. 'z';
  type AS is array (Ans) of Boolean;
  r, rg, clear : AS;
  --
  procedure Collect_Group_Total is
    g : Natural := 0;
  begin
    for i in Ans loop if rg (i) then g := g + 1; end if; end loop;
    rg := clear;
    total := total + g;
    new_group := True;
  end Collect_Group_Total;
  --
  f : File_Type;
  s : VString;
begin
  clear := (others => False);
  for part in 1 .. 2 loop
    Open (f, "aoc_2020_06.txt");
    r := clear;
    rg := clear;
    total := 0;
    new_group := True;
    while not End_Of_File (f) loop
      Get_Line (f, s);
      if s = "" then
        Collect_Group_Total;
      else
        r := clear;
        for i in Ans loop
          if Index (s, +i) > 0 then
            r (i) := True;
          end if;
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
    Put_Line (+"Part " & part & "  " & total);
    Close (f);
  end loop;
end AoC_2020_06_Full_Ada;
