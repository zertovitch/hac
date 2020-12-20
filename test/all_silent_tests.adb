--  We launch new instances of HAC (possibly from HAC itself, too).
--  Usage:  hac all_silent_tests.adb

with HAC_Pack;  use HAC_Pack;

procedure All_Silent_Tests is

  procedure Launch_Tests is

    procedure Shell (command : VString; echo : Boolean; success : out Boolean) is
      r : Integer;
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      Shell_Execute (command, r);
      success := r = 0;
      if r /= 0 then
        Put_Line ("*** Command: [" & command & "] FAILED ***.");
      end if;
    end Shell;

    successes, failures : Natural := 0;

    procedure Launch_HAC (Ada_file_name : VString; ups : Positive) is
      success : Boolean;
    begin
      Shell (
        ups * (+".." & Directory_Separator) & "hac -v1 " & Ada_file_name,
        False,
        success
      );
      if success then
        successes := successes + 1;
      else
        failures := failures + 1;
      end if;
    end Launch_HAC;

    procedure Build_HAC is
      success : Boolean;
    begin
      if Get_Env("hacbuild") = "done" then
        return;
      end if;   
      Put_Line ("(Re-)building HAC, in case the present program isn't run from HAC...");
      Shell (+"gprbuild -P .." & Directory_Separator & "hac", True, success);
    end Build_HAC;

  begin
    Put_Line( "    ___________      ____________________________________________________________");
    Put_Line( "   / *  HAC  * \    /  ""Silent tests"": when there is zero output, no compilation \");
    Put_Line( "   |  Testing  |    |   error and no run-time error, then it's all fine.         |");
    Put_Line( "   \___________/    \____________________________________________________________/");
    New_Line;
    Build_HAC;  --  Redundant if this program is itself run through HAC.
    --
    Put_Line( "----> Launching tests (one instance of HAC each)...");
    Launch_HAC (+"case_statement.adb", 1);
    Launch_HAC (+"constants.adb", 1);
    Launch_HAC (+"declarations.adb", 1);
    Launch_HAC (+"enumerations.adb", 1);
    Launch_HAC (+"floats.adb", 1);
    Launch_HAC (+"integers.adb", 1);
    Launch_HAC (+"loops.adb", 1);
    Launch_HAC (+"recursion.adb", 1);
    Launch_HAC (+"sorting_tests.adb", 1);
    Launch_HAC (+"strings.adb", 1);
    Launch_HAC (+"type_conversion.adb", 1);
    Launch_HAC (+"var_init.adb", 1);
    --
    --  Advent of Code 2020
    Set_Directory (+".." & Directory_Separator &
                   "exm" & Directory_Separator &
                   "aoc" & Directory_Separator &
                   "2020");
    Launch_HAC (+"aoc_2020_02.adb 607 321",                     3);  --  Password Philosophy
    Launch_HAC (+"aoc_2020_03.adb 218 3847183340",              3);  --  Toboggan Trajectory
    Launch_HAC (+"aoc_2020_04.adb 228 175",                     3);  --  Passport Processing
    Launch_HAC (+"aoc_2020_05.adb 835",                         3);  --  Binary Boarding
    Launch_HAC (+"aoc_2020_06.adb 6532 3427",                   3);  --  Custom Customs
    Launch_HAC (+"aoc_2020_07.adb 169 82372",                   3);  --  Handy Haversacks
    Launch_HAC (+"aoc_2020_08.adb 1394 1626",                   3);  --  Handheld Halting
    Launch_HAC (+"aoc_2020_09.adb 138879426 23761694",          3);  --  Encoding Error
    Launch_HAC (+"aoc_2020_10.adb 2277 37024595836928",         3);  --  Adapter Array
    Launch_HAC (+"aoc_2020_11.adb 37 26",                       3);  --  Seating System
    Launch_HAC (+"aoc_2020_12.adb 1631 58606",                  3);  --  Rain Risk
    Launch_HAC (+"aoc_2020_13.adb 222 408270049879073",         3);  --  Shuttle Search
    Launch_HAC (+"aoc_2020_15.adb 436 1 10 27 78 438 1836 249", 3);  --  Rambunctious Recitation
    Launch_HAC (+"aoc_2020_16.adb 23954 453459307723",          3);  --  Ticket Translation
    Launch_HAC (+"aoc_2020_17.adb 207",                         3);  --  Conway Cubes
    Launch_HAC (+"aoc_2020_20.adb 83775126454273",              3);  --  Jurassic Jigsaw
    Put_Line ("----> Done.");
    if failures = 0 then
      Put_Line ("All tests passed.");
    else
      Put_Line (+"*** There are FAILED tests ***");
    end if;
    Put_Line ("Summary:");
    Put_Line (+"         " & successes & " successes");
    Put_Line (+"         " & failures & " failures");
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
