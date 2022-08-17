--  Solution to Advent of Code 2021, Day 04
-------------------------------------------
--  Giant Squid
--
--  https://adventofcode.com/2021/day/4
--  Copy of questions in: aoc_2021_04_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_04 is
  use HAT;
  res_1, res_2 : Integer;
  sep : Character;
  f : File_Type;
  --
  input : constant VString := +"aoc_2021_04.txt";
  number_of_boards : constant := 100;
  subtype Boards_Range is Integer range 1 .. number_of_boards;
  first_winner, last_winner : Boards_Range;
  any_winner_yet : Boolean := False;
  --
  list_max : constant := 2500;
  list : array (1 .. list_max) of Natural;
  top : Natural := 0;
  number_last_bingo, number_first_winner : Natural;

  board_size : constant := 5;
  subtype Board_Side_Range is Integer range 1 .. board_size;

  type Cell is record
    value  : Natural;
    marked : Boolean;
  end record;

  type Board_Type is array (Board_Side_Range, Board_Side_Range) of Cell;
  board : array (Boards_Range) of Board_Type;
  won : array (Boards_Range) of Boolean;
  bingo : Boolean;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
begin
  --
  --  Get and prepare data.
  --
  Open (f, input);
  loop
    top := top + 1;
    Get (f, list (top));
    exit when End_Of_Line (f);
    Get (f, sep);  --  Absorb the ',' separator
  end loop;
  for b in Boards_Range loop
    won (b) := False;
    for i in Board_Side_Range loop
      for j in Board_Side_Range loop
        Get (f, board (b)(i, j).value);
        board (b)(i, j).marked := False;
      end loop;
    end loop;
  end loop;
  Close (f);
  --
  numbers_loop :
  for n in 1 .. top loop
    --  Mark all boards with the number in list (n).
    boards_loop :
    for b in 1 .. number_of_boards loop
      if not won (b) then
        --  Mark and check rows:
        for i in Board_Side_Range loop
          bingo := True;
          for j in Board_Side_Range loop
            if list (n) = board (b)(i, j).value then
              board (b)(i, j).marked := True;
            else
              bingo := bingo and then board (b)(i, j).marked;
            end if;
          end loop;
          exit when bingo;
        end loop;
        if not bingo then
          --  Check columns:
          for j in Board_Side_Range loop
            bingo := True;
            for i in Board_Side_Range loop
              if not board (b)(i, j).marked then
                bingo := False;
                exit;
              end if;
            end loop;
            exit when bingo;
          end loop;
        end if;
        --  Process Bingo case:
        if bingo then
          won (b) := True;
          last_winner := b;
          number_last_bingo := list (n);
          if not any_winner_yet then
            any_winner_yet := True;
            first_winner := b;
            number_first_winner := number_last_bingo;
          end if;
        end if;
      end if;
    end loop boards_loop;
  end loop numbers_loop;
  --
  if any_winner_yet then
    res_1 := 0;
    res_2 := 0;
    for i in Board_Side_Range loop
      for j in Board_Side_Range loop
        if not board (first_winner)(i, j).marked then
          res_1 := res_1 + board (first_winner)(i, j).value;
        end if;
        if not board (last_winner)(i, j).marked then
          res_2 := res_2 + board (last_winner)(i, j).value;
        end if;
      end loop;
    end loop;
    if verbose then
      Put_Line
        (+"First winner " & first_winner &
           "; score 1: " & res_1 & " * " & number_first_winner);
      Put_Line
        (+"Last winner  " & last_winner  &
           "; score 2: " & res_2 & " * " & number_last_bingo);
    end if;
    res_1 := res_1 * number_first_winner;
    res_2 := res_2 * number_last_bingo;
  else
    Put_Line ("Uh? No Bingo?");
  end if;
  --
  if compiler_test_mode then
    if res_1 /= Integer_Value (Argument (1)) or
       res_2 /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Part 1: " & res_1);
    Put_Line (+"Part 2: " & res_2);
    --  Part 1: validated by AoC: 39984
    --  Part 2: validated by AoC: 8486
  end if;
end AoC_2021_04;
