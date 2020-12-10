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

    failures : Natural := 0;

    procedure Launch_HAC (Ada_file_name : VString; ups : Positive) is
      up_dir: VString;
      success : Boolean;
    begin
      for i in 1 .. ups loop
        up_dir := up_dir & ".." & Directory_Separator;
      end loop;
      Shell (
        up_dir & "hac -v1 " & Ada_file_name,
        False,
        success
      );
      if not success then
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
    Launch_HAC (+"aoc_2020_02_a.adb 607",                 3);  --  Password Philosophy
    Launch_HAC (+"aoc_2020_02_b.adb 321",                 3);  --  Password Philosophy
    Launch_HAC (+"aoc_2020_03.adb   218 3847183340",      3);  --  Toboggan Trajectory
    Launch_HAC (+"aoc_2020_04_a.adb 228",                 3);  --  Passport Processing
    Launch_HAC (+"aoc_2020_04_b.adb 175",                 3);  --  Passport Processing
    Launch_HAC (+"aoc_2020_05.adb   835",                 3);  --  Binary Boarding
    Launch_HAC (+"aoc_2020_06.adb   6532 3427",           3);  --  Custom Customs
    Launch_HAC (+"aoc_2020_07.adb   169 82372",           3);  --  Handy Haversacks
    Launch_HAC (+"aoc_2020_08.adb   1394 1626",           3);  --  Handheld Halting
    Launch_HAC (+"aoc_2020_09.adb   138879426 23761694",  3);  --  Encoding Error
    Launch_HAC (+"aoc_2020_10.adb   2277 37024595836928", 3);  --  Adapter Array
    Put_Line ("----> Done.");
    if failures = 0 then
      Put_Line ("All tests passed.");
    else
      Put_Line (+"*** Failed tests: " & failures & " ***");
    end if;
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
