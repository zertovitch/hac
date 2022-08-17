--  We launch new instances of HAC (possibly from HAC itself, too).
--  Usage:  hac all_noisy_tests.adb

with HAT;

procedure All_Noisy_Tests is

  use HAT;

  procedure Launch_Tests is

    procedure Shell (command : VString; echo : Boolean) is
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      Shell_Execute (command);
    end Shell;

    procedure Launch_HAC (Ada_file_name : VString) is
    begin
      Put_Line ("-------------------------------------------------------");
      Shell (
        +".." & Directory_Separator & "hac -v1 " & Ada_file_name,
        False
      );
    end Launch_HAC;

    procedure Build_HAC is
    begin
      if Get_Env ("hacbuild") = "done" then
        return;
      end if;
      Put_Line ("(Re-)building HAC, in case the present program isn't run from HAC...");
      Shell (+"gprbuild -P .." & Directory_Separator & "hac", True);
    end Build_HAC;

    procedure Pause is
      dummy : Character;
    begin
      Put ("--- Press any key to continue in the HAC noisy test suite...");
      Get_Immediate (dummy);
      New_Line;
    end Pause;

  begin
    Put_Line ("    ___________      _________________________________");
    Put_Line ("   / *  HAC  * \    /  ""Noisy tests"": a human is      \");
    Put_Line ("   \__Testing__/    \__required to check the output.__/");
    New_Line;
    Build_HAC;  --  Redundant if this program is itself run through HAC.
    --
    Put_Line ("----> Launching tests (one instance of HAC each)...");
    for e in 1 .. 4 loop
      Launch_HAC (+"exception_0" & e & ".adb");
    end loop;
    Pause;
    Launch_HAC (+"if_then_elsif_else.adb");
    Pause;
    Launch_HAC (+"digitz.adb");
    Put_Line ("----> Done.");
  end Launch_Tests;

begin
  Launch_Tests;
end All_Noisy_Tests;
