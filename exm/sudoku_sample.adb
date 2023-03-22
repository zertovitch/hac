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

  easy,
  less_easy,
  hard,
  very_hard,
  hidden_double_1 : Sudo_Strings;

  use HAT;

begin
  Put_Line ("Increase verbosity_level for getting more details.");
  New_Line;

  Zero (grand_total);

  --  Spotting only naked singles is sufficient on this one.
  easy (1) := +"1   83 57";
  easy (2) := +"9 27     ";
  easy (3) := +"  5   34 ";
  easy (4) := +"5   7  8 ";
  easy (5) := +"4  9 1  5";
  easy (6) := +" 1  5   6";
  easy (7) := +" 57   8  ";
  easy (8) := +"     87 4";
  easy (9) := +"24 16   9";
  Solve_from_Strings (easy, +"Easy 1         ", 1);

  --  One round needs to spot only hidden singles;
  --  the rest is solved by handling naked singles.
  easy (1) := +"1459  2 8";
  easy (2) := +" 3   5 91";
  easy (3) := +"2  3    7";
  easy (4) := +" 2    1 6";
  easy (5) := +"    6    ";
  easy (6) := +"4 1    8 ";
  easy (7) := +"3    4  2";
  easy (8) := +"65 1   7 ";
  easy (9) := +"9 4  7513";
  Solve_from_Strings (easy, +"Easy 2         ", 1);

  --  One round needs to spot doubles on rows.
  less_easy (1) := +"7 8  4  3";
  less_easy (2) := +"   8   9 ";
  less_easy (3) := +"   7  8 2";
  less_easy (4) := +"     7941";
  less_easy (5) := +"         ";
  less_easy (6) := +"1745     ";
  less_easy (7) := +"9 6  3   ";
  less_easy (8) := +" 4   6   ";
  less_easy (9) := +"8  4  5 9";
  Solve_from_Strings (less_easy, +"Less easy 1a   ", 1);

  --  Same puzzle, but first row is filled a bit more...
  --  No multiple spotting is needed for solving it.
  less_easy (1) := +"7 8  4 53";
  Solve_from_Strings (less_easy, +"Less easy 1b   ", 1);

  less_easy (1) := +" 8  2    ";
  less_easy (2) := +"4 7 536  ";
  less_easy (3) := +" 6    4  ";
  less_easy (4) := +"  49    2";
  less_easy (5) := +"2       7";
  less_easy (6) := +"7    59  ";
  less_easy (7) := +"  6    9 ";
  less_easy (8) := +"  547 3 1";
  less_easy (9) := +"    1  4 ";
  Solve_from_Strings (less_easy, +"Less easy 2    ", 1);

  less_easy (1) := +"8  9     ";
  less_easy (2) := +"   5  18 ";
  less_easy (3) := +"      236";
  less_easy (4) := +"  7 31   ";
  less_easy (5) := +" 1     4 ";
  less_easy (6) := +"   74 9  ";
  less_easy (7) := +"729      ";
  less_easy (8) := +" 64  8   ";
  less_easy (9) := +"     6  7";
  Solve_from_Strings (less_easy, +"Less easy 3    ", 1);

  --  This one has a {13} {13} hidden
  --  double on first row at round 6.
  less_easy (1) := +" 7    2 6";
  less_easy (2) := +" 9  16  4";
  less_easy (3) := +"5  3    7";
  less_easy (4) := +"      4  ";
  less_easy (5) := +"9  6 1  8";
  less_easy (6) := +"  3      ";
  less_easy (7) := +"6    7  1";
  less_easy (8) := +"7  58  4 ";
  less_easy (9) := +"2 9    7 ";
  Solve_from_Strings (less_easy, +"Less easy 4    ", 1);

  less_easy (1) := +"6  1     ";
  less_easy (2) := +"  869  2 ";
  less_easy (3) := +"  23   5 ";
  less_easy (4) := +"3     9  ";
  less_easy (5) := +"8   7   3";
  less_easy (6) := +"  1     8";
  less_easy (7) := +" 7   18  ";
  less_easy (8) := +" 4  862  ";
  less_easy (9) := +"     5  6";
  Solve_from_Strings (less_easy, +"Less easy 5    ", 1);

  less_easy (1) := +"1  4 7 9 ";
  less_easy (2) := +"3 8 51   ";
  less_easy (3) := +" 6       ";
  less_easy (4) := +"4 6    3 ";
  less_easy (5) := +"7       1";
  less_easy (6) := +" 3    4 7";
  less_easy (7) := +"       1 ";
  less_easy (8) := +"   38 5 4";
  less_easy (9) := +" 7 5 9  8";
  Solve_from_Strings (less_easy, +"Less easy 6    ", 1);

  --  https://www.youtube.com/watch?v=8dNHOyzH-gc
  hard (1) := +" 2      5";
  hard (2) := +"  4 7   1";
  hard (3) := +"    3    ";
  hard (4) := +" 7  2 9  ";
  hard (5) := +"4     3  ";
  hard (6) := +"   6    8";
  hard (7) := +" 56    1 ";
  hard (8) := +"   3  7 2";
  hard (9) := +"9  8     ";
  Solve_from_Strings (hard, +"Hard 1         ", 1);

  --  https://www.youtube.com/watch?v=9LiOg4BnmVU
  hard (1) := +"56 1   3 ";
  hard (2) := +"9   2 6  ";
  hard (3) := +" 1      2";
  hard (4) := +"  36  7 9";
  hard (5) := +"    8   4";
  hard (6) := +" 5       ";
  hard (7) := +"    3    ";
  hard (8) := +"    4    ";
  hard (9) := +" 27 6  13";
  Solve_from_Strings (hard, +"Hard 2         ", 1);

  --  https://www.youtube.com/watch?v=4GVyBiFUNws
  hard (1) := +" 8  2 56 ";
  hard (2) := +"   1    7";
  hard (3) := +"         ";
  hard (4) := +" 5  9 4 8";
  hard (5) := +"  78    3";
  hard (6) := +" 9  1  5 ";
  hard (7) := +"2 4   8  ";
  hard (8) := +" 6  85   ";
  hard (9) := +"   2  1  ";
  Solve_from_Strings (hard, +"Hard 3         ", 1);

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
  very_hard (1) := +" 8 1    7";
  very_hard (2) := +"   5   26";
  very_hard (3) := +"  27 4  3";
  very_hard (4) := +"     1  4";
  very_hard (5) := +"1        ";
  very_hard (6) := +"  42     ";
  very_hard (7) := +"      6 8";
  very_hard (8) := +"7 1  3   ";
  very_hard (9) := +"  54  9  ";
  Solve_from_Strings (very_hard, +"Very hard 1    ", 1);

  Show_Total (grand_total, +"Techniques used for all puzzles:");

end Sudoku_Sample;
