--  Solution to Advent of Code 2020, Day 04, Part Two
-----------------------------------------------------
--  Passport Processing
--
--  HAC 0.08 version.
--
--  HAC 0.08 nice to have's:
--    *   exception handling to catch invalid values...
--    *   "x [not] in a .. b"
--    *   removal of needless () with correct boolean priority.
--    *   Index with From parameter
--
--  https://adventofcode.com/2020/day/04
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_04_b is
  --
  function Val_Num (s : VString; hexa : Boolean) return Integer is
    c : Character;
  begin
    if Length (s) = 0 then
      return -1;
    end if;
    for i in 1 .. Length (s) loop
      c := Element (s, i);
      if not (
          ((c >= '0') and (c <= '9')) or
          (hexa and (c >= 'a') and (c <= 'f'))
        )
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
  f : File_Type;
  cats, total : Integer := 0;
  cat_idx, tok_begin, tok_end, nb : Integer;
  s, tok, cat, un : VString;
begin
  Open (f, "aoc_2020_04.txt");
  while not End_Of_File (f) loop
    Get_Line (f, s);
    if s = "" then
      cats := 0;
    end if;
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
      if (cat = "byr") and (Val (tok) >= 1920) and (Val (tok) <= 2002) then cats := cats + 1; end if;
      if (cat = "iyr") and (Val (tok) >= 2010) and (Val (tok) <= 2020) then cats := cats + 1; end if;
      if (cat = "eyr") and (Val (tok) >= 2020) and (Val (tok) <= 2030) then cats := cats + 1; end if;
      if (cat = "hcl") and (Element (s, tok_begin) = '#')
                       and (Val_Hexa (Slice (s, tok_begin + 1, tok_end)) > 0)
      then
        cats := cats + 1;
      end if;
      if (cat = "pid") and (Length (tok) = 9) and (Val (tok) > 0) then cats := cats + 1; end if;
      if (cat = "ecl") and
        ((tok = "amb") or (tok = "blu") or (tok = "brn") or (tok = "gry") or
         (tok = "grn") or (tok = "hzl") or (tok = "oth"))
      then
        cats := cats + 1;
      end if;
      if cat = "hgt" then
        nb := Val (Slice (s, tok_begin, tok_end - 2));
        un := Slice (s, tok_end - 1, tok_end);
        if ((nb >= 150) and (nb <= 193) and (un = "cm")) or
           ((nb >= 59) and (nb <= 76) and (un = "in"))
        then
          cats := cats + 1;
        end if;
      end if;
    end loop;
    if cats = 7 then
      total := total + 1;
      cats := 0;
    end if;
  end loop;
  Close (f);
  Put_Line (total);
end AoC_2020_04_b;
