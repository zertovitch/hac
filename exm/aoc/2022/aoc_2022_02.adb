--  Solution to Advent of Code 2022, Day 2
------------------------------------------
--  Rock Paper Scissors
--
--  https://adventofcode.com/2022/day/2
--  Copy of questions in: aoc_2022_02_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_02 is
  use HAT;

  c, sep : Character;
  f : File_Type;
  --
  input : constant VString := +"aoc_2022_02.txt";
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  --
  type T is (Rock, Paper, Scissors);
  them, you : T;

  function A_Defeats_B (a, b : T) return Boolean is
  begin
    return (a = Rock and then b = Scissors)
      or else (a = Paper and then b = Rock)
      or else (a = Scissors and then b = Paper);
  end A_Defeats_B;

  function Defeats (opponent : T) return T is
  begin
    case opponent is
      when Rock     => return Paper;
      when Paper    => return Scissors;
      when Scissors => return Rock;
    end case;
  end Defeats;

  function Is_Defeated_By (opponent : T) return T is
  begin
    case opponent is
      when Paper    => return Rock;
      when Scissors => return Paper;
      when Rock     => return Scissors;
    end case;
  end Is_Defeated_By;

  score, total : Integer;

begin
  for round in 1 .. 2 loop
    total := 0;
    Open (f, input);
    while not End_Of_File (f) loop
      Get (f, c);
      them := T'Val (Character'Pos (c) - Character'Pos ('A'));
      Get (f, sep);
      Get (f, c);
      case round is
        when 1 =>
          you := T'Val (Character'Pos (c) - Character'Pos ('X'));
        when 2 =>
          case c is
            when 'X' =>  --  You lose
              you := Is_Defeated_By (them);
            when 'Y' =>  --  Draw
              you := them;
            when 'Z' =>  --  You win
              you := Defeats (them);
            when others => null;
          end case;
      end case;
      score := T'Pos (you) + 1;
      if them = you then
        score := score + 3;
      end if;
      if A_Defeats_B (you, them) then
        score := score + 6;
      end if;
      if verbose then
        Put_Line (+them'Image & ' ' & you'Image & ' ' & score);
      end if;
      total := total + score;
    end loop;
    Close (f);
    r (round) := total;
  end loop;
  if compiler_test_mode then
   if r (1) /= Integer'Value (To_String (Argument (1))) or
      r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: total score, rule #1: " & Integer'Image (r (1)));
    Put_Line (+"Part 2: total score, rule #2: " & Integer'Image (r (2)));
    --  Part 1: validated by AoC: 14531
    --  Part 2: validated by AoC: 11258
  end if;
end AoC_2022_02;
