--  Solution to Advent of Code 2020, Day 4
------------------------------------------
--  Passport Processing
--
--  https://adventofcode.com/2020/day/4
--
-------------------------------------------------------------------------
--
--  HAC 0.084 version.
--
--  HAC 0.084 "nice to have"'s detected in this exercise:
--    *   exception handling to catch invalid values
--    *   Index with From parameter
--    *   "and then", "or else" (solved in HAC 0.098)
--    *   'Image attribute for enumerated types
--
with HAT;  --  For a build with "full Ada": files HAT*.ad* are in ../../../src

procedure AoC_2020_04 is
  use HAT;
  --
  function Val_Num (s : VString; hexa : Boolean) return Integer is
    c : Character;
  begin
    if Length (s) = 0 then
      return -1;
    end if;
    for i in 1 .. Length (s) loop
      c := Element (s, i);
      if not (c in '0' .. '9' or else (hexa and then c in 'a' .. 'f'))
      then
        return -1;
      end if;
    end loop;
    if hexa then
      return Integer_Value (+"16#" & s & '#');
    else
      return Integer_Value (s);
    end if;
  end Val_Num;
  --
  function Val (s : VString) return Integer is
  begin
    return Val_Num (s, False);
  end Val;
  function Val_Hexa (s : VString) return Integer is
  begin
    return Val_Num (s, True);
  end Val_Hexa;
  --
  cats, total, cat_idx, tok_begin, tok_end, nb : Integer;
  s, tok, cat, un : VString;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  f : File_Type;
begin
  for criteria in 1 .. 2 loop
    Open (f, "aoc_2020_04.txt");
    cats := 0;
    total := 0;
    while not End_Of_File (f) loop
      Get_Line (f, s);
      if s = "" then
        cats := 0;
      end if;
      if criteria = 1 then
        if Index (s, "ecl:") > 0 then cats := cats + 1; end if;
        if Index (s, "pid:") > 0 then cats := cats + 1; end if;
        if Index (s, "eyr:") > 0 then cats := cats + 1; end if;
        if Index (s, "hcl:") > 0 then cats := cats + 1; end if;
        if Index (s, "byr:") > 0 then cats := cats + 1; end if;
        if Index (s, "iyr:") > 0 then cats := cats + 1; end if;
        if Index (s, "hgt:") > 0 then cats := cats + 1; end if;
      else
        cat_idx := 0;
        loop
          s := Slice (s, cat_idx + 1, Length (s));
          cat_idx := Index (s, ":");
          exit when cat_idx <= 3;
          tok_begin := cat_idx + 1;
          tok_end := tok_begin - 1 + Index (Slice (s, tok_begin, Length (s)), " ");
          if tok_end >= tok_begin then
            tok_end := tok_end - 1;
          else
            tok_end := Length (s);
          end if;
          tok := Slice (s, tok_begin, tok_end);
          cat := Slice (s, cat_idx - 3, cat_idx - 1);
          if cat = "byr" and then Val (tok) in 1920 .. 2002 then cats := cats + 1; end if;
          if cat = "iyr" and then Val (tok) in 2010 .. 2020 then cats := cats + 1; end if;
          if cat = "eyr" and then Val (tok) in 2020 .. 2030 then cats := cats + 1; end if;
          if cat = "hcl" and then Element (s, tok_begin) = '#'
                         and then Val_Hexa (Slice (s, tok_begin + 1, tok_end)) > 0
          then
            cats := cats + 1;
          end if;
          if cat = "pid" and then Length (tok) = 9 and then Val (tok) >= 0 then cats := cats + 1; end if;
          if cat = "ecl" and then Index (+"amb blu brn gry grn hzl oth", tok) > 0
                                --  ^ Idea: Maxim Reznik, replaces checking each value
          then
            cats := cats + 1;
          end if;
          if cat = "hgt" then
            nb := Val (Slice (s, tok_begin, tok_end - 2));
            un := Slice (s, tok_end - 1, tok_end);
            if (un = "cm" and then nb in 150 .. 193) or else
               (un = "in" and then nb in 59 .. 76)
            then
              cats := cats + 1;
            end if;
          end if;
        end loop;
      end if;
      if cats = 7 then
        total := total + 1;
        --  Prevent incrementing total if there is garbage
        --  or a "cid:" until next blank line:
        cats := 0;
      end if;
    end loop;
    Close (f);
    if compiler_test_mode then
      if total /= Integer_Value (Argument (criteria)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (+"Valid passports (criteria set #" & criteria & "): " & total);
      --  Part 1: validated by AoC: 228
      --  Part 2: validated by AoC: 175
    end if;
  end loop;
end AoC_2020_04;
