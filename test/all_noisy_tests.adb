--  We launch new instances of hax (possibly from hax too).
--  Usage: hax all_silent_tests.adb

with HAC_Pack;  use HAC_Pack;

procedure All_Noisy_Tests is

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
      if Get_Env("haxbuild") = "done" then
        return;
      end if;   
      Put_Line ("(Re-)building HAX, in case the present program isn't run from HAX...");
      Shell (+"gprbuild -p -P .." & Directory_Separator & "hac", True);
    end Build_HAX;

    procedure Pause is
      dummy : Character;
    begin
      Put ("--- Press any key to continue in the HAC noisy test suite...");
      Get_Immediate (dummy);
      New_Line;
    end Pause;

  begin
    Put_Line( "    ___________      _____________________________________");
    Put_Line( "   / *  HAC  * \    /  ""Noisy tests"": a human is        \");
    Put_Line( "   \__Testing__/    \__required to check the output and.__/");
    New_Line;
    Build_HAX;  --  Redundant if this program is itself run through HAX.
    --
    Put_Line( "----> Launching tests (one instance of HAX each)...");
    for e in 1 .. 4 loop
      Launch_HAX (+"exception_0" & e & ".adb");
    end loop;
    Pause;
    Launch_HAX (+"if_then_elsif_else.adb");
    Put_Line ("----> Done.");
  end Launch_Tests;

begin
  Launch_Tests;
end All_Noisy_Tests;
