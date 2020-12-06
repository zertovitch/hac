--  Solution to Advent of Code 2020, Day 6
------------------------------------------
--  Custom Customs
--
--  https://adventofcode.com/2020/day/6
--
-------------------------------------------------------------------------
--
--  HAC 0.08 version.
--
--  HAC 0.08 "nice to have"'s detected in this exercise:
--
--    *     ` subtype ... range ... ` like: `subtype Ans is Character range 'a' .. 'z';`
--    *     ` clear := (others => False); `
--    *     ` rg := rg and r ` for arrays of Boolean (i.e., sets)
--    *     ` aaa : constant Character := 'a';` compiled as a "Pascal" constant (like a number)
--                                              and not as a read-only variable
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_06 is
  total : Integer;
  new_group : Boolean;
  --  aaa : constant Character := 'a';
  --  ^ Avoid ambiguity of Wide_*Character when compiled with full Ada.
  --    Replace all `'a'` below by `aaa` for compiling with a full Ada compiler.
  type Yes_Answer is array ('a' .. 'z') of Boolean;
  r, rg : Yes_Answer;
  --
  procedure Collect_Group_Total is
    g : Natural := 0;
  begin
    for c in 'a' .. 'z' loop if rg (c) then g := g + 1; end if; end loop;
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
        for c in 'a' .. 'z' loop
          r (c) := Index (s, +c) > 0;
        end loop;
        if new_group then
          rg := r;
          new_group := False;
        elsif part = 1 then
          for c in 'a' .. 'z' loop rg (c) := rg (c) or r (c); end loop;
        else
          for c in 'a' .. 'z' loop rg (c) := rg (c) and r (c); end loop;
        end if;
      end if;
    end loop;
    Collect_Group_Total;
    Put_Line (+"Part " & part & "  " & total);
    Close (f);
  end loop;
end AoC_2020_06;
