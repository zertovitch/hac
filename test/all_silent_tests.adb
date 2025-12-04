--  This program runs a series of more than 115 regression tests to check
--  the correct operation of the HAC compiler.
--
--  The failures and success are accounted and the sums are shown at the end.
--
--  NB: the individual tests, or this program itself, can actually be used
--  for testing any Ada compiler. For that, all you have to do is to
--  customize the procedure Launch_HAC below.
--
--  For each test we launch, possibly from HAC itself, a new instance of
--  the HAC command-line tool (`hac` or `hac.exe`), which will build the
--  test sources as a virtual machine executable and run it.
--
--  Usage (if run from `hac` or `hac.exe`):  hac all_silent_tests.adb

with HAT;
with Testing_Utilities;

procedure All_Silent_Tests is

  use HAT;

  procedure Launch_Tests is

    procedure Shell (command : VString; echo : Boolean; success : out Boolean) is
      r : Integer;
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      Shell_Execute (command, r);
      success := r = Testing_Utilities.OK_Code;
      if not success then
        Put_Line (+"*** Command: [" & command & "] FAILED ***. Return code: " & r);
      end if;
    end Shell;

    procedure Build_HAC (success : out Boolean) is
    begin
      if Get_Env ("hacbuild") = "done" then
        success := True;
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
      short : VString;
      sep : constant Natural := Index_Backward (Ada_file_name, Directory_Separator);
      spc : constant Natural := Index_Backward (Ada_file_name, ' ');
    begin
      if spc > 0 then
        short := Slice (Ada_file_name, sep + 1, spc - 1);
      else
        short := Slice (Ada_file_name, sep + 1, Length (Ada_file_name));
      end if;
      Put_Line (+"      " & short);
      Launch_HAC (Ada_file_name, +"", 1);
    end Normal_Test;

    --  Advent of Code

    procedure Launch_AoC (Year, Day, Solutions : VString) is
    begin
      if Index (Current_Directory, Year) = 0 then
        New_Line;
        Set_Directory (+".." & Directory_Separator & Year);
        Put_Line ("    Advent of Code " & Year & " in " & Current_Directory & ':');
        Put ("      ");
      end if;
      Put (Day & ' ');
      Launch_HAC
        (+"aoc_" & Year & '_' & Day & ".adb ",
         Solutions,
         --  ^ Solutions are validated by https://adventofcode.com/ .
         --    It's the pair of answers, each appearing after
         --   "Your puzzle answer was ", separated by a space.
         3);
    end Launch_AoC;

    hac_build_success : Boolean;

    examples_dir : constant VString :=
      +".." & Directory_Separator &
       "exm" & Directory_Separator;

    generate : constant VString :=
      +".." & Directory_Separator &
       "hac " & examples_dir & "pkg_demo_gen.adb";

  begin
    Put_Line ("    ______________________      _____________________________________________");
    Put_Line ("   /    *   H A C   *     \    /  ""Silent tests"": when the failure count     \");
    Put_Line ("   |  Regression Testing  |    |  is zero, then the test suite is all fine.  |");
    Put_Line ("   \______________________/    \_____________________________________________/");
    New_Line;
    Build_HAC (hac_build_success);  --  Redundant if this program is itself run through HAC.
    if not hac_build_success then
      Put_Line ("--- HAC build failed (called from all_silent_tests.adb) ---");
      return;
    end if;
    --
    Put_Line ("----> Launching tests.");
    Put_Line ("  One instance of HAC is called each time, with compilation and execution...");
    New_Line;
    Put_Line (+"    Normal tests in " & Current_Directory & ':');
    Normal_Test (+"attributes_test.adb");
    Normal_Test (examples_dir & "barnes.adb 3816547290");
    Normal_Test (+"case_statement.adb");
    Normal_Test (+"constants.adb");
    Normal_Test (+"declarations.adb");
    Normal_Test (+"enumerations.adb");
    Normal_Test (+"floats.adb");
    Normal_Test (+"forward.adb");
    Normal_Test (+"integers.adb");
    Normal_Test (+"loops.adb");
    Normal_Test (+"object_init.adb");
    --  Create the X_* packages, run pkg_demo and finally delete the X_* packages .
    Shell_Execute (generate);
    Normal_Test (+"-I. .." & Directory_Separator & "exm" & Directory_Separator & "pkg_demo.adb test_mode");
    Shell_Execute (generate & " delete");
    --
    Normal_Test (+"recursion.adb");
    Normal_Test (+"remarks_check.adb");
    Normal_Test (+"sorting_tests.adb");
    Normal_Test (+"strings.adb");
    Normal_Test (examples_dir & "tasking" & Directory_Separator & "tasks_02.adb x");
    Normal_Test (+"type_conversion.adb");
    --
    Set_Directory (+".." & Directory_Separator &
                   "exm" & Directory_Separator &
                   "aoc" & Directory_Separator &
                   "2021");  --  <- We put on purpose the wrong starting year.
    --
    Launch_AoC (+"2020", +"02", +"607 321");                      --  Password Philosophy
    Launch_AoC (+"2020", +"03", +"218 3847183340");               --  Toboggan Trajectory
    Launch_AoC (+"2020", +"04", +"228 175");                      --  Passport Processing
    Launch_AoC (+"2020", +"05", +"835");                          --  Binary Boarding
    Launch_AoC (+"2020", +"06", +"6532 3427");                    --  Custom Customs
    Launch_AoC (+"2020", +"07", +"169 82372");                    --  Handy Haversacks
    Launch_AoC (+"2020", +"08", +"1394 1626");                    --  Handheld Halting
    Launch_AoC (+"2020", +"09", +"138879426 23761694");           --  Encoding Error
    Launch_AoC (+"2020", +"10", +"2277 37024595836928");          --  Adapter Array
    Launch_AoC (+"2020", +"11", +"37 26");                        --  Seating System
    Launch_AoC (+"2020", +"12", +"1631 58606");                   --  Rain Risk
    Launch_AoC (+"2020", +"13", +"222 408270049879073");          --  Shuttle Search
    Launch_AoC (+"2020", +"15", +"436 1 10 27 78 438 1836 249");  --  Rambunctious Recitation
    Launch_AoC (+"2020", +"16", +"23954 453459307723");           --  Ticket Translation
    Launch_AoC (+"2020", +"17", +"207");                          --  Conway Cubes
    Launch_AoC (+"2020", +"20", +"83775126454273");               --  Jurassic Jigsaw
    Launch_AoC (+"2020", +"22", +"31957");                        --  Crab Combat
    Launch_AoC (+"2020", +"23", +"67384529 49576328");            --  Crab Cups
    Launch_AoC (+"2020", +"24", +"341 285");                      --  Lobby Layout
    --
    Launch_AoC (+"2021", +"01", +"1154 1127");               --  Sonar Sweep
    Launch_AoC (+"2021", +"02", +"2187380 2086357770");      --  Dive!
    Launch_AoC (+"2021", +"03", +"3549854 3765399");         --  Binary Diagnostic
    Launch_AoC (+"2021", +"04", +"39984 8468");              --  Giant Squid
    Launch_AoC (+"2021", +"05", +"6225 22116");              --  Hydrothermal Venture
    Launch_AoC (+"2021", +"06", +"388419 1740449478328");    --  Lanternfish
    Launch_AoC (+"2021", +"07", +"340052 92948968");         --  The Treachery of Whales
    Launch_AoC (+"2021", +"08", +"440 1046281");             --  Seven Segment Search
    Launch_AoC (+"2021", +"09", +"423 1198704");             --  Smoke Basin
    Launch_AoC (+"2021", +"10", +"388713 3539961434");       --  Syntax Scoring
    Launch_AoC (+"2021", +"11", +"1679 519");                --  Dumbo Octopus
    Launch_AoC (+"2021", +"12", +"3497");                    --  Passage Pathing
    Launch_AoC (+"2021", +"13", +"602");                     --  Transparent Origami
    Launch_AoC (+"2021", +"14", +"2345 2432786807053");      --  Extended Polymerization
    Launch_AoC (+"2021", +"15", +"656");                     --  Chiton
    Launch_AoC (+"2021", +"16", +"927 1725277876501");       --  Packet Decoder
    Launch_AoC (+"2021", +"21", +"684495 152587196649184");  --  Dirac Dice
    --
    Launch_AoC (+"2022", +"01", +"68442 204837");                   --  Calorie Counting
    Launch_AoC (+"2022", +"02", +"14531 11258");                    --  Rock Paper Scissors
    Launch_AoC (+"2022", +"03", +"8185 2817");                      --  Rucksack Reorganization
    Launch_AoC (+"2022", +"04", +"657 938");                        --  Camp Cleanup
    Launch_AoC (+"2022", +"05", +"VQZNJMWTR NLCDCLVMQ");            --  Supply Stacks
    Launch_AoC (+"2022", +"06", +"1802 3551");                      --  Tuning Trouble
    Launch_AoC (+"2022", +"07", +"1749646 1498966");                --  No Space Left On Device
    Launch_AoC (+"2022", +"08", +"1843 180000");                    --  Treetop Tree House
    Launch_AoC (+"2022", +"09", +"6314 2504");                      --  Rope Bridge
    Launch_AoC (+"2022", +"10", +"12880");                          --  Cathode-Ray Tube
    Launch_AoC (+"2022", +"11", +"102399 23641658401");             --  Monkey in the Middle
    Launch_AoC (+"2022", +"12", +"440 439");                        --  Hill Climbing Algorithm
    Launch_AoC (+"2022", +"13", +"6568 19493");                     --  Distress Signal
    Launch_AoC (+"2022", +"14", +"964 32041");                      --  Regolith Reservoir
    Launch_AoC (+"2022", +"17", +"3193 1577650429835");             --  Pyroclastic Flow
    Launch_AoC (+"2022", +"18", +"3374 2010");                      --  Boiling Boulders
    Launch_AoC (+"2022", +"21", +"286698846151845 3759566892642");  --  Monkey Math
    Launch_AoC (+"2022", +"22", +"26558 110400");                   --  Monkey Map
    Launch_AoC (+"2022", +"23", +"3689 965");                       --  Unstable Diffusion
    --
    Launch_AoC (+"2023", +"01", +"52974 53340");                    --  Trebuchet?!
    Launch_AoC (+"2023", +"02", +"2176 63700");                     --  Cube Conundrum
    Launch_AoC (+"2023", +"03", +"539590 80703636");                --  Gear Ratios
    Launch_AoC (+"2023", +"04", +"23750 13261850");                 --  Scratchcards
    Launch_AoC (+"2023", +"05", +"261668924 24261545");             --  If You Give A Seed A Fertilizer
    Launch_AoC (+"2023", +"06", +"393120 36872656");                --  Wait For It
    Launch_AoC (+"2023", +"07", +"252656917 253499763");            --  Camel Cards
    Launch_AoC (+"2023", +"08", +"12737 9064949303801");            --  Haunted Wasteland
    Launch_AoC (+"2023", +"09", +"1884768153 1031");                --  Mirage Maintenance
    Launch_AoC (+"2023", +"10", +"6897 367");                       --  Pipe Maze
    Launch_AoC (+"2023", +"11", +"10173804 634324905172");          --  Cosmic Expansion
    Launch_AoC (+"2023", +"12", +"7007 3476169006222");             --  Hot Springs
    Launch_AoC (+"2023", +"13", +"33728 28235");                    --  Point of Incidence
    Launch_AoC (+"2023", +"14", +"113078 94255");                   --  Parabolic Reflector Dish
    Launch_AoC (+"2023", +"15", +"513172 237806");                  --  Lens Library
    Launch_AoC (+"2023", +"16", +"7996");                           --  The Floor Will Be Lava
    Launch_AoC (+"2023", +"17", +"102 94");                         --  Clumsy Crucible (example's data)
    Launch_AoC (+"2023", +"18", +"53300 64294334780659");           --  Lavaduct Lagoon
    Launch_AoC (+"2023", +"19", +"19114 167409079868000");          --  Aplenty
    Launch_AoC (+"2023", +"20", +"806332748 228060006554227");      --  Pulse Propagation
    Launch_AoC (+"2023", +"21", +"3677");                           --  Step Counter
    Launch_AoC (+"2023", +"22", +"5 7");                            --  Sand Slabs (example's data)
    Launch_AoC (+"2023", +"23", +"94 154");                         --  A Long Walk (example's data)
    --  Launch_AoC (+"2023", +"24", +"24192 664822352550558");      --  Never Tell Me The Odds (depends on floating-point accuracy of HAT.Real)
    Launch_AoC (+"2023", +"25", +"54");                             --  Snowverload (example's data)
    --
    Launch_AoC (+"2024", +"01", +"2086478 24941624");                             --  Historian Hysteria (sorting)
    Launch_AoC (+"2024", +"02", +"332 398");                                      --  Red-Nosed Reports
    Launch_AoC (+"2024", +"03", +"173529487 99532691");                           --  Mull It Over
    Launch_AoC (+"2024", +"04", +"2483 1925");                                    --  Ceres Search
    Launch_AoC (+"2024", +"05", +"5166");                                         --  Print Queue
    Launch_AoC (+"2024", +"06", +"5461");                                         --  Guard Gallivant (part 2 takes 4 minutes on HAC!)
    Launch_AoC (+"2024", +"07", +"850435817339");                                 --  Bridge Repair (part 2 takes 3 minutes on HAC!)
    Launch_AoC (+"2024", +"08", +"409 1308");                                     --  Resonant Collinearity
    Launch_AoC (+"2024", +"09", +"6340197768906");                                --  Disk Fragmenter (part 2 takes 34 seconds on HAC!)
    Launch_AoC (+"2024", +"10", +"737 1619");                                     --  Hoof It
    Launch_AoC (+"2024", +"11", +"198089 236302670835517");                       --  Plutonian Pebbles
    Launch_AoC (+"2024", +"12", +"1319878 784982");                               --  Garden Groups
    Launch_AoC (+"2024", +"13", +"36954 79352015273424");                         --  Claw Contraption
    Launch_AoC (+"2024", +"14", +"226236192");                                    --  Restroom Redoubt (part 2 is visual)
    Launch_AoC (+"2024", +"15", +"1515788 1516544");                              --  Warehouse Woes
    Launch_AoC (+"2024", +"16", +"74392");                                        --  Reindeer Maze (part 2 takes long even on GNAT)
    Launch_AoC (+"2024", +"17", +"4635635210");                                   --  Chronospatial Computer (example's data)
    Launch_AoC (+"2024", +"18", +"298 52,32");                                    --  RAM Run
    Launch_AoC (+"2024", +"19", +"260 639963796864990");                          --  Linen Layout
    Launch_AoC (+"2024", +"20", +"44 285");                                       --  Race Condition (example's data)
    Launch_AoC (+"2024", +"21", +"222670 271397390297138");                       --  Keypad Conundrum
    Launch_AoC (+"2024", +"22", +"37990510 23");                                  --  Monkey Market (example's data)
    Launch_AoC (+"2024", +"23", +"1227 cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy");  --  LAN Party
    Launch_AoC (+"2024", +"24", +"57270694330992");                               --  Crossed Wires
    Launch_AoC (+"2024", +"25", +"3395");                                         --  Code Chronicle
    --
    Launch_AoC (+"2025", +"01", +"989 5941");               --  Secret Entrance
    Launch_AoC (+"2025", +"02", +"1227775554 4174379265");  --  Gift Shop
    Launch_AoC (+"2025", +"03", +"17613 175304218462560");  --  Lobby
    Launch_AoC (+"2025", +"04", +"1320 8354");              --  Printing Department
    --
    New_Line (2);
    New_Line;
    Put_Line ("----> Done.");
    New_Line;
    if failures = 0 then
      Put_Line ("All tests passed successfully.");
    else
      Put_Line (+"*** There are FAILED tests ***");
    end if;
    Put_Line ("Summary:");
    Put_Line (+"        " & successes & " successes");
    Put_Line (+"          " & failures & " failures");
    New_Line;
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
