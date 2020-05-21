--  We launch new instances of hax (possibly from hax too).
--  Usage: hax all_silent_tests.adb

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

    procedure Launch_HAX (Ada_file_name : VString) is
    begin
      Shell (
        +".." & Directory_Separator & "hax -v1 " & Ada_file_name,
        False
      );
    end Launch_HAX;

    procedure Build_HAX is
    begin
      Put_Line ("(Re-)building HAX, in case the present program isn't run from HAX...");
      Shell (+"gprbuild -P .." & Directory_Separator & "hac", True);
    end Build_HAX;

  begin
    Build_HAX;  --  Redundant if this program is run through HAX.
    --
    Launch_HAX (+"case_statement.adb");
    Launch_HAX (+"constants.adb");
    Launch_HAX (+"declarations.adb");
    Launch_HAX (+"enumerations.adb");
    Launch_HAX (+"floats.adb");
    Launch_HAX (+"integers.adb");
    Launch_HAX (+"loops.adb");
    Launch_HAX (+"recursion.adb");
    Launch_HAX (+"sorting_tests.adb");
    Launch_HAX (+"strings.adb");
    Launch_HAX (+"type_conversion.adb");
    Launch_HAX (+"var_init.adb");
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
