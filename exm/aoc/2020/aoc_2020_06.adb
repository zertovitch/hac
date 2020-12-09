--  Solution to Advent of Code 2020, Day 6
------------------------------------------
--  Custom Customs
--
--  https://adventofcode.com/2020/day/6
--
-------------------------------------------------------------------------
--
--  HAC 0.081 version.
--
--  HAC 0.081 "nice to have"'s detected in this exercise:
--
--    *     ` clear := (others => False); `
--    *     ` rg := rg and r ` for arrays of Boolean (i.e., sets)
--    *     ` aaa : constant Character := 'a';`
--                       HAC should detect an expression as a static (compile-time-known) value
--    *     in HAC_Pack: Index (s, c) where c is a Character
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_06 is
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
  test_mode : constant Boolean := Argument_Count >= 2;
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
          r (c) := Index (s, +c) > 0;
        end loop;
        if new_group then
          rg := r;
          new_group := False;
        elsif part = 1 then
          for c in Answer_Range loop rg (c) := rg (c) or r (c); end loop;
        else
          for c in Answer_Range loop rg (c) := rg (c) and r (c); end loop;
        end if;
      end if;
    end loop;
    Collect_Group_Total;
    if test_mode then
      if total /= Integer_Value (Argument (part)) then
        Put ("*** Test FAILS ***");
      end if;
    else
      Put_Line (+"Part " & part & ".  Total custom answers: " & total);
    end if;
    Close (f);
  end loop;
end AoC_2020_06;
