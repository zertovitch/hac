--  We launch new instances of HAC (possibly from HAC itself, too).
--  Usage:  hac all_silent_tests.adb

with HAC_Pack;  use HAC_Pack;

procedure All_Silent_Tests is

  procedure Launch_Tests is

    procedure Shell (command : VString; echo : Boolean) is
      dummy : Integer;
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      dummy := Shell_Execute (command);
    end Shell;

    procedure Launch_HAC (Ada_file_name : VString) is
    begin
      Shell (
        +".." & Directory_Separator & "hac -v1 " & Ada_file_name,
        False
      );
    end Launch_HAC;

    procedure Build_HAC is
    begin
      if Get_Env("hacbuild") = "done" then
        return;
      end if;   
      Put_Line ("(Re-)building HAC, in case the present program isn't run from HAC...");
      Shell (+"gprbuild -P .." & Directory_Separator & "hac", True);
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
    Launch_HAC (+"case_statement.adb");
    Launch_HAC (+"constants.adb");
    Launch_HAC (+"declarations.adb");
    Launch_HAC (+"enumerations.adb");
    Launch_HAC (+"floats.adb");
    Launch_HAC (+"integers.adb");
    Launch_HAC (+"loops.adb");
    Launch_HAC (+"recursion.adb");
    Launch_HAC (+"sorting_tests.adb");
    Launch_HAC (+"strings.adb");
    Launch_HAC (+"type_conversion.adb");
    Launch_HAC (+"var_init.adb");
    Put_Line ("----> Done.");
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
