--  Solution to Advent of Code 2020, Day 04, Part Two
-----------------------------------------------------
--  Passport Processing
--
--  Full Ada version.
--
--  https://adventofcode.com/2020/day/04
--
with Ada.Characters.Handling, Ada.Strings.Fixed, Ada.Text_IO;
with Interfaces;

procedure AoC_2020_04_b_Full_Ada is
  use Ada.Characters.Handling, Ada.Strings.Fixed, Ada.Text_IO, Interfaces;
  --
  function Val (s : String) return Integer_64 is
  begin
    return Integer_64'Value (s);
  exception
    when others => return -1;
  end Val;
  --
  type Cat is (byr, iyr, eyr, hgt, hcl, ecl, pid);
  ok : Boolean;
  f : File_Type;
  cats, cat_idx, total : Integer := 0;
  tok_begin, tok_end : Integer;
begin
  Open (f, In_File, "aoc_2020_04.txt");
  while not End_Of_File (f) loop
    declare
      s : constant String := Get_Line (f);
    begin
      if s = "" then
        cats := 0;
      end if;
      for c in Cat loop
        cat_idx := Index (s, To_Lower (Cat'Image (c)) & ':');
        if cat_idx > 0 then
          tok_begin := cat_idx + 4;
          tok_end := Index (s, " ", tok_begin);
          if tok_end > 0 then
            tok_end := tok_end - 1;
          else
            tok_end := s'Last;
          end if;
          declare
            tok : String renames s (tok_begin .. tok_end);
          begin
            case c is
              when byr => ok := Val (tok) in 1920 .. 2002;
              when iyr => ok := Val (tok) in 2010 .. 2020;
              when eyr => ok := Val (tok) in 2020 .. 2030;
              when hcl => ok := Val ("16" & tok & '#') > 0;
              when pid => ok := tok'Length = 9 and then Val (tok) > 0;
              when ecl => ok := Index ("amb blu brn gry grn hzl oth", tok) > 0;
                                --  ^ Idea: Maxim Reznik, replaces checking each value
              when hgt =>
                ok :=
                    ((Val (s (tok_begin .. tok_end - 2)) in 150 .. 193
                                 and then s (tok_end - 1 .. tok_end) = "cm")
                      or else
                     (Val (s (tok_begin .. tok_end - 2)) in 59 .. 76
                                 and then s (tok_end - 1 .. tok_end) = "in")
                    );
            end case;
          end;
          if ok then cats := cats + 1; end if;
        end if;
      end loop;
      if cats = 7 then
        total := total + 1;
        --  Prevent incrementing total if there is garbage
        --  or a "cid:" until next blank line:
        cats := 0;
      end if;
    end;
  end loop;
  Close (f);
  Put_Line ("Valid passports (criteria #2):" & Integer'Image (total));
end AoC_2020_04_b_Full_Ada;
