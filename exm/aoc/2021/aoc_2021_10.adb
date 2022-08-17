--  Solution to Advent of Code 2021, Day 10
-------------------------------------------
--  Syntax Scoring
--
--  https://adventofcode.com/2021/day/10
--  Copy of questions in: aoc_2021_10_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2021_10 is
  use HAT, Interfaces;
  --
  input : constant VString := +"aoc_2021_10.txt";
  --
  stack : String (1 .. 200);
  top : Natural;
  --
  procedure Push (c : Character) is
  begin
    top := top + 1;
    stack (top) := c;
  end Push;
  --
  function Pop_OK (c : Character) return Boolean is
  begin
    if top = 0 then
      return False;
    end if;
    top := top - 1;
    return stack (top + 1) = c;
  end Pop_OK;
  --
  scores : array (1 .. 100) of Integer_64;
  top_score_array : Natural := 0;
  ins : Natural;
  line_score : Integer_64 := 0;
  f : File_Type;
  c : Character;
  points : Integer_64;
  r : array (1 .. 2) of Integer_64;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  r (1) := 0;
  r (2) := 0;
  scores (1) := 1;  --  Just for calming down a GNAT warning.
  Open (f, input);
  while not End_Of_File (f) loop
    --  Analyse a line's syntax:
    top := 0;
    loop
      Get (f, c);
      case c is
        when '(' => Push (')');
        when '[' => Push (']');
        when '{' => Push ('}');
        when '<' => Push ('>');
        when ')' | ']' | '}' | '>' =>
          if not Pop_OK (c) then  --  Incorrect closing character.
            case c is
              when ')' => points := 3;
              when ']' => points := 57;
              when '}' => points := 1197;
              when '>' => points := 25137;
              when others => null;
            end case;
            r (1) := r (1) + points;
            Skip_Line (f);  --  We count only the first error of a line.
            exit;
          end if;
        when others => null;
      end case;
      if End_Of_Line (f) then
        if top > 0 then --  Incomplete line
          line_score := 0;
          for i in reverse 1 .. top loop
            line_score := line_score * 5;
            case stack (i) is
              when ')' => points := 1;
              when ']' => points := 2;
              when '}' => points := 3;
              when '>' => points := 4;
              when others => null;
            end case;
            line_score := line_score + points;
          end loop;
          ins := 0;
          for i in reverse 1 .. top_score_array loop
            if line_score <= scores (i) then
              ins := i;
            end if;
          end loop;
          if ins = 0 then
            scores (top_score_array + 1) := line_score;
          else
            for i in reverse ins .. top_score_array loop
              scores (i + 1) := scores (i);
            end loop;
            scores (ins) := line_score;
          end if;
          top_score_array := top_score_array + 1;
        end if;
        exit;
      end if;
    end loop;
  end loop;
  r (2) := scores (top_score_array / 2 + 1);
  --  ^ "There will always be an odd number of scores to consider."
  Close (f);
  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: invalid closing delimiter points:" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2: autocomplete points:             " & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 388713
    --  Part 2: validated by AoC: 3539961434
  end if;
end AoC_2021_10;
