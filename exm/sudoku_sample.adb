with Sudokus;
with HAT;

procedure Sudoku_Sample is

  use Sudokus;

  grand_total : Technique_Count;

  procedure Solve_from_Strings (s : Sudo_Strings; name : HAT.VString; verbosity_level : Natural) is
    p : Sudo_Pack;
    h : Sudo_Help;
  begin
    Convert_Data (s, p, h);
    Solve (p, h, name, verbosity_level);
    Cumulate (grand_total, p.total);
  end Solve_from_Strings;

  easy_1, easy_2,
  less_easy_1, less_easy_2, less_easy_3,
  hard_1, hard_2, hard_3,
  hidden_double_1,
  very_hard_1 : Sudo_Strings;

  use HAT;

begin
  Put_Line ("Increase verbosity_level for getting more details.");
  New_Line;

  Zero (grand_total);

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
  Solve_from_Strings (easy_1, +"Easy 1         ", 1);

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
  Solve_from_Strings (easy_2, +"Easy 2         ", 1);

  --  One round needs to spot doubles on rows.
  less_easy_1 (1) := +"7 8  4  3";
  less_easy_1 (2) := +"   8   9 ";
  less_easy_1 (3) := +"   7  8 2";
  less_easy_1 (4) := +"     7941";
  less_easy_1 (5) := +"         ";
  less_easy_1 (6) := +"1745     ";
  less_easy_1 (7) := +"9 6  3   ";
  less_easy_1 (8) := +" 4   6   ";
  less_easy_1 (9) := +"8  4  5 9";
  Solve_from_Strings (less_easy_1, +"Less easy 1a   ", 1);

  --  Same puzzle, but first row is filled a bit more...
  --  No multiple spotting is needed for solving it.
  less_easy_1 (1) := +"7 8  4 53";
  Solve_from_Strings (less_easy_1, +"Less easy 1b   ", 1);

  less_easy_2 (1) := +" 8  2    ";
  less_easy_2 (2) := +"4 7 536  ";
  less_easy_2 (3) := +" 6    4  ";
  less_easy_2 (4) := +"  49    2";
  less_easy_2 (5) := +"2       7";
  less_easy_2 (6) := +"7    59  ";
  less_easy_2 (7) := +"  6    9 ";
  less_easy_2 (8) := +"  547 3 1";
  less_easy_2 (9) := +"    1  4 ";
  Solve_from_Strings (less_easy_2, +"Less easy 2    ", 1);

  less_easy_3 (1) := +"8  9     ";
  less_easy_3 (2) := +"   5  18 ";
  less_easy_3 (3) := +"      236";
  less_easy_3 (4) := +"  7 31   ";
  less_easy_3 (5) := +" 1     4 ";
  less_easy_3 (6) := +"   74 9  ";
  less_easy_3 (7) := +"729      ";
  less_easy_3 (8) := +" 64  8   ";
  less_easy_3 (9) := +"     6  7";
  Solve_from_Strings (less_easy_3, +"Less easy 3    ", 1);

  --  https://www.youtube.com/watch?v=8dNHOyzH-gc
  hard_1 (1) := +" 2      5";
  hard_1 (2) := +"  4 7   1";
  hard_1 (3) := +"    3    ";
  hard_1 (4) := +" 7  2 9  ";
  hard_1 (5) := +"4     3  ";
  hard_1 (6) := +"   6    8";
  hard_1 (7) := +" 56    1 ";
  hard_1 (8) := +"   3  7 2";
  hard_1 (9) := +"9  8     ";
  Solve_from_Strings (hard_1, +"Hard 1         ", 1);

  --  https://www.youtube.com/watch?v=9LiOg4BnmVU
  hard_2 (1) := +"56 1   3 ";
  hard_2 (2) := +"9   2 6  ";
  hard_2 (3) := +" 1      2";
  hard_2 (4) := +"  36  7 9";
  hard_2 (5) := +"    8   4";
  hard_2 (6) := +" 5       ";
  hard_2 (7) := +"    3    ";
  hard_2 (8) := +"    4    ";
  hard_2 (9) := +" 27 6  13";
  Solve_from_Strings (hard_2, +"Hard 2         ", 1);

  --  https://www.youtube.com/watch?v=4GVyBiFUNws
  hard_3 (1) := +" 8  2 56 ";
  hard_3 (2) := +"   1    7";
  hard_3 (3) := +"         ";
  hard_3 (4) := +" 5  9 4 8";
  hard_3 (5) := +"  78    3";
  hard_3 (6) := +" 9  1  5 ";
  hard_3 (7) := +"2 4   8  ";
  hard_3 (8) := +" 6  85   ";
  hard_3 (9) := +"   2  1  ";
  Solve_from_Strings (hard_3, +"Hard 3         ", 1);
  
  hidden_double_1 (1) := +"    6    ";  --  {47} {47}:  hidden double
  hidden_double_1 (2) := +"    42736";  --  in the first two cells
  hidden_double_1 (3) := +"  673  4 ";
  hidden_double_1 (4) := +" 94    68";
  hidden_double_1 (5) := +"    964 7";
  hidden_double_1 (6) := +"6 7 5 923";
  hidden_double_1 (7) := +"1      85";
  hidden_double_1 (8) := +" 6  8 271";
  hidden_double_1 (9) := +"  5 1  94";
  Solve_from_Strings (hidden_double_1, +"Hidden double 1", 1);

  --  https://www.youtube.com/watch?v=T4OdkQMmyu8
  --  Stalls at round 7, equivalent to t = 7:37
  --  From comment at t= 8:30: "a computer has
  --  to go on a brute force way".
  very_hard_1 (1) := +" 8 1    7";
  very_hard_1 (2) := +"   5   26";
  very_hard_1 (3) := +"  27 4  3";
  very_hard_1 (4) := +"     1  4";
  very_hard_1 (5) := +"1        ";
  very_hard_1 (6) := +"  42     ";
  very_hard_1 (7) := +"      6 8";
  very_hard_1 (8) := +"7 1  3   ";
  very_hard_1 (9) := +"  54  9  ";
  Solve_from_Strings (very_hard_1, +"Very hard 1    ", 1);

  Show_Total (grand_total, +"Techniques used for all puzzles:");

end Sudoku_Sample;
