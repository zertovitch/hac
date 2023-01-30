with Sudokus;
with HAT;

procedure Sudoku is

  use Sudokus;

  procedure Solve_from_Strings (s : Sudo_Strings; name : HAT.VString; verbosity_level : Natural) is
    p : Sudo_Pack;
    h : Sudo_Help;
  begin
    Convert_Data (s, p, h);
    Solve (p, h, name, verbosity_level);
  end Solve_from_Strings;

  easy_1, easy_2,
  less_easy_1, less_easy_2 : Sudo_Strings;

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
  Solve_from_Strings (easy_1, +"Easy 1      ", 1);

  --  One round needs to spot only hidden singles;
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
  Solve_from_Strings (easy_2, +"Easy 2      ", 1);

  less_easy_1 (1) := +"7 8  4  3";
  less_easy_1 (2) := +"   8   9 ";
  less_easy_1 (3) := +"   7  8 2";
  less_easy_1 (4) := +"     7941";
  less_easy_1 (5) := +"         ";
  less_easy_1 (6) := +"1745     ";
  less_easy_1 (7) := +"9 6  3   ";
  less_easy_1 (8) := +" 4   6   ";
  less_easy_1 (9) := +"8  4  5 9";
  Solve_from_Strings (less_easy_1, +"Less easy 1a", 1);

  --  Same but filled a bit more...
  less_easy_1 (1) := +"7 8  4 53";
  Solve_from_Strings (less_easy_1, +"Less easy 1b", 1);

  less_easy_2 (1) := +" 8  2    ";
  less_easy_2 (2) := +"4 7 536  ";
  less_easy_2 (3) := +" 6    4  ";
  less_easy_2 (4) := +"  49    2";
  less_easy_2 (5) := +"2       7";
  less_easy_2 (6) := +"7    59  ";
  less_easy_2 (7) := +"  6    9 ";
  less_easy_2 (8) := +"  547 3 1";
  less_easy_2 (9) := +"    1  4 ";

  --
  Solve_from_Strings (less_easy_2, +"Less easy 2 ", 1);
end Sudoku;
