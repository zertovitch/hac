with Sudokus;
with HAT;

procedure Sudoku is

  use Sudokus;

  procedure Solve (s : Sudostrings; name : HAT.VString; verbosity_level : Natural) is
    use HAT;

    p : Sudopack;

    stalling : Boolean := False;

    procedure Do_Round (round : Positive) is
      naked_singles_found, hidden_single_found,
      locked_cells_outside_boxes, locked_cells_inside_boxes : Natural := 0;
    begin
      Handle_Naked_Singles (p.u, naked_singles_found);
      --  We search more complicated possibilities,
      --  only when none for the less complicated was found.
      if naked_singles_found = 0 then
        Handle_Hidden_Singles (p.u, hidden_single_found);
        if hidden_single_found = 0 then
          Handle_Locked_Cells_Outside_Boxes (p.u, locked_cells_outside_boxes);
          if locked_cells_outside_boxes = 0 then
            Handle_Locked_Cells_Inside_Boxes (p.u, locked_cells_inside_boxes);
          end if;
        end if;
      end if;
      if verbosity_level > 0 then
        if verbosity_level > 1 or else Is_Solved (p.u) then
          Show (p.u, +"After round " & round & " for " & name);
        end if;
        if verbosity_level > 1 then
          if naked_singles_found + hidden_single_found +
            locked_cells_outside_boxes + locked_cells_inside_boxes = 0
          then
            Put_Line ("Stalling");
            stalling := True;
          end if;
          if naked_singles_found > 0 then
            Put_Line (+"Found: " & naked_singles_found & " naked singles");
          end if;
          if hidden_single_found > 0 then
            Put_Line (+"       " & hidden_single_found & " hidden singles");
          end if;
          if locked_cells_outside_boxes > 0 then
            Put_Line (+"       " & locked_cells_outside_boxes & " locked cells, outside boxes");
          end if;
          if locked_cells_inside_boxes > 0 then
            Put_Line (+"       " & locked_cells_inside_boxes & " locked cells, inside boxes");
          end if;
          New_Line;
        end if;
      end if;
    end Do_Round;

  begin
    Convert_Data (s, p);
    Adapt_All_Sets (p.u);
    New_Line;
    Show (p.u, ">>>>> Initial board for: " & name);
    if not Is_Valid (p.u) then
      Put_Line ("Initial board is invalid!");
      return;
    end if;

    for round in 1 .. Sudigit'Last * Sudigit'Last loop
      Do_Round (round);
      exit when stalling or else Is_Solved (p.u);
    end loop;
    if stalling then
      Show_Detailed_Possibilities (p.u);
    end if;
    if not Is_Valid (p.u) then
      Put_Line ("Solution or current state is invalid!");
      return;
    end if;
  end Solve;

  easy_1, easy_2, less_easy_1 : Sudostrings;

  use HAT;

begin
  --  Spotting only naked singles is sufficient on this one.
  easy_1 (1) := +"1   83 57";
  easy_1 (2) := +"9 27     ";
  easy_1 (3) := +"  5   34 ";
  easy_1 (4) := +"5   7  8 ";
  easy_1 (5) := +"4  9 1  5";
  easy_1 (6) := +" 1  5   6";
  easy_1 (7) := +" 57   8  ";
  easy_1 (8) := +"     87 4";
  easy_1 (9) := +"24 16   9";

  --  One round needs to spot only hidden singles (9);
  --  the rest is solved by handling naked singles.
  easy_2 (1) := +"1459  2 8";
  easy_2 (2) := +" 3   5 91";
  easy_2 (3) := +"2  3    7";
  easy_2 (4) := +" 2    1 6";
  easy_2 (5) := +"    6    ";
  easy_2 (6) := +"4 1    8 ";
  easy_2 (7) := +"3    4  2";
  easy_2 (8) := +"65 1   7 ";
  easy_2 (9) := +"9 4  7513";

  less_easy_1 (1) := +"7 8  4  3";
  less_easy_1 (2) := +"   8   9 ";
  less_easy_1 (3) := +"   7  8 2";
  less_easy_1 (4) := +"     7941";
  less_easy_1 (5) := +"         ";
  less_easy_1 (6) := +"1745     ";
  less_easy_1 (7) := +"9 6  3   ";
  less_easy_1 (8) := +" 4   6   ";
  less_easy_1 (9) := +"8  4  5 9";

  --  Same but filled a bit more...
  less_easy_1 (1) := +"7 8  4 53";

  --
  Solve (easy_1, +"Easy 1", 1);
  Solve (easy_2, +"Easy 2", 1);
  Solve (less_easy_1, +"Less easy 1", 2);
end Sudoku;
