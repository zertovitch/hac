--  We launch new instances of HAC (possibly from HAC itself, too).
--  Usage:  hac all_silent_tests.adb

with HAL;

procedure All_Silent_Tests is

  use HAL;

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

    procedure Build_HAC is
      success : Boolean;
    begin
      if Get_Env("hacbuild") = "done" then
        return;
      end if;   
      Put_Line ("(Re-)building HAC, in case the present program isn't run from HAC...");
      Shell (+"gprbuild -P .." & Directory_Separator & "hac", True, success);
    end Build_HAC;

    successes, failures : Natural := 0;

    procedure Launch_HAC (Ada_file_name, args : VString; ups : Positive) is
      success : Boolean;
    begin
      Shell (
        ups * (+".." & Directory_Separator) & "hac " & Ada_file_name & ' ' & args,
        False,
        success
      );
      if success then
        successes := successes + 1;
      else
        failures := failures + 1;
      end if;
    end Launch_HAC;

    procedure Normal_Test (Ada_file_name : VString) is
    begin
      Put_Line (+"      " & Ada_file_name);
      Launch_HAC (Ada_file_name, +"", 1);
    end Normal_Test;

    --  Advent of Code

    subtype Day_Code is VString;  --  !!  Should be ok for "&": String (1..2);

    procedure Launch_AoC (Year : Positive; Day : Day_Code; Solutions : VString) is
    begin
      Put (Day & ' ');
      Launch_HAC (+"aoc_" & Year & '_' & Day & ".adb ", Solutions, 3);
    end Launch_AoC;

    procedure Launch_AoC_2020 (Day : Day_Code; Solutions : VString) is
    begin
      Launch_AoC (2020, Day, Solutions);
    end Launch_AoC_2020;

  begin
    Put_Line( "    ___________      _____________________________________________________________________");
    Put_Line( "   / *  HAC  * \    /  ""Silent tests"": when there is zero output, no compilation error,   \");
    Put_Line( "   |  Testing  |    |  no run-time error, and 0 failure, then the test suite is all fine. |");
    Put_Line( "   \___________/    \_____________________________________________________________________/");
    New_Line;
    Build_HAC;  --  Redundant if this program is itself run through HAC.
    --
    Put_Line( "----> Launching tests.");
    Put_Line( "  One instance of HAC is called each time, with compilation and execution...");
    Put_Line( +"    Normal tests in " & Current_Directory & ':');
    Normal_Test (+"case_statement.adb");
    Normal_Test (+"constants.adb");
    Normal_Test (+"declarations.adb");
    Normal_Test (+"enumerations.adb");
    Normal_Test (+"floats.adb");
    Normal_Test (+"init_var.adb");
    Normal_Test (+"integers.adb");
    Normal_Test (+"loops.adb");
    Normal_Test (+"recursion.adb");
    Normal_Test (+"sorting_tests.adb");
    Normal_Test (+"strings.adb");
    Normal_Test (+"type_conversion.adb");
    Normal_Test (+"var_init.adb");
    --
    Set_Directory (+".." & Directory_Separator &
                   "exm" & Directory_Separator &
                   "aoc" & Directory_Separator &
                   "2020");
    Put_Line ("    Advent of Code 2020 in " & Current_Directory & ':');
    Put ("      ");
    Launch_AoC_2020 (+"02", +"607 321"                    );  --  Password Philosophy
    Launch_AoC_2020 (+"03", +"218 3847183340"             );  --  Toboggan Trajectory
    Launch_AoC_2020 (+"04", +"228 175"                    );  --  Passport Processing
    Launch_AoC_2020 (+"05", +"835"                        );  --  Binary Boarding
    Launch_AoC_2020 (+"06", +"6532 3427"                  );  --  Custom Customs
    Launch_AoC_2020 (+"07", +"169 82372"                  );  --  Handy Haversacks
    Launch_AoC_2020 (+"08", +"1394 1626"                  );  --  Handheld Halting
    Launch_AoC_2020 (+"09", +"138879426 23761694"         );  --  Encoding Error
    Launch_AoC_2020 (+"10", +"2277 37024595836928"        );  --  Adapter Array
    Launch_AoC_2020 (+"11", +"37 26"                      );  --  Seating System
    Launch_AoC_2020 (+"12", +"1631 58606"                 );  --  Rain Risk
    Launch_AoC_2020 (+"13", +"222 408270049879073"        );  --  Shuttle Search
    Launch_AoC_2020 (+"15", +"436 1 10 27 78 438 1836 249");  --  Rambunctious Recitation
    Launch_AoC_2020 (+"16", +"23954 453459307723"         );  --  Ticket Translation
    Launch_AoC_2020 (+"17", +"207"                        );  --  Conway Cubes
    Launch_AoC_2020 (+"20", +"83775126454273"             );  --  Jurassic Jigsaw
    Launch_AoC_2020 (+"22", +"31957"                      );  --  Crab Combat
    Launch_AoC_2020 (+"23", +"67384529 49576328"          );  --  Crab Cups
    Launch_AoC_2020 (+"24", +"341 332"                    );  --  Lobby Layout
    --
    New_Line;
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
