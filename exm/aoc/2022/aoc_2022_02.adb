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
  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;
  --
  type Object is (Rock, Paper, Scissors);
  them, you : Object;

  function Defeats (opponent : Object) return Object is
  begin
    case opponent is
      when Rock     => return Paper;
      when Paper    => return Scissors;
      when Scissors => return Rock;
    end case;
  end Defeats;

  function Is_Defeated_By (opponent : Object) return Object is
  begin
    case opponent is
      when Paper    => return Rock;
      when Scissors => return Paper;
      when Rock     => return Scissors;
    end case;
  end Is_Defeated_By;

  score, total : Integer;

begin
Parts :
  for part in 1 .. 2 loop
    total := 0;
    Open (f, "aoc_2022_02.txt");
  Read_Data :
    while not End_Of_File (f) loop
      Get (f, c);
      them := Object'Val (Character'Pos (c) - Character'Pos ('A'));
      Get (f, sep);
      Get (f, c);
      case part is
        when 1 =>
          --  In the puzzle's part 1, the second column with X, Y or Z
          --  seems to represent what your move should be.
          you := Object'Val (Character'Pos (c) - Character'Pos ('X'));
        when 2 =>
          --  Actually the Elf explains that the second column
          --  contains what the outcome of your move should be.
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
      score := Object'Pos (you) + 1;
      if them = you then
        --  Draw
        score := score + 3;
      end if;
      if you = Defeats (them) then
        --  You win
        score := score + 6;
      end if;
      if verbose then
        Put_Line (+them'Image & ' ' & you'Image & ' ' & score);
      end if;
      total := total + score;
    end loop Read_Data;
    Close (f);
    --
    r (part) := total;
  end loop Parts;
  --
  if Argument_Count >= 2 then
    --  Compiler test mode.
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
