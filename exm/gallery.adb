--  We launch new instances of HAC (possibly from HAC itself, too).
--  Usage: hac gallery.adb

with HAT; use HAT;

procedure Gallery is

  procedure Launch_Demos is

    procedure Shell (command : VString; echo : Boolean) is
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      Shell_Execute (command);
    end Shell;

    procedure Launch_HAC (Ada_file_name : VString) is
      dummy : Character;
    begin
      Shell (
        +".." & Directory_Separator & "hac -v2 " & Ada_file_name,
        False
      );
      Put ("--- Press any key to continue in the HAC gallery...");
      Get_Immediate (dummy);
      New_Line;
    end Launch_HAC;

    procedure Build_HAC is
    begin
      if Get_Env ("hacbuild") = "done" then
        return;
      end if;
      Put_Line ("(Re-)building HAC, in case the present program isn't run from HAC...");
      Shell (+"gprbuild -p -P .." & Directory_Separator & "hac", True);
    end Build_HAC;

  begin
    Build_HAC;  --  Redundant if this program is itself run through HAC.
    --
    Launch_HAC (+"hello.adb");
    Launch_HAC (+"attributes.adb");
    Launch_HAC (+"maze_gen.adb");
    Launch_HAC (+"strings_demo.adb");
    Launch_HAC (+"env.adb");
    Launch_HAC (+"arguments.adb arg1 arg2 ""arg 3 ..."" arg4");
    Launch_HAC (+"ackermann.adb");
    Launch_HAC (+"anti_primes.adb");
    Launch_HAC (+"doors.adb");
    Launch_HAC (+"hofstadter.adb");
    Launch_HAC (+"mandelbrot.adb");
    Launch_HAC (+"shell_sort.adb");
    Launch_HAC (+"merge_sort.adb");
    Launch_HAC (+"days_1901.adb");
    Launch_HAC (+"shell.adb");
    Launch_HAC (+"file_read.adb");
    Launch_HAC (+"existence.adb");
    Launch_HAC (+"timing.adb");
    Launch_HAC (+"bwt.adb");
    Launch_HAC (+"unit_a.adb");
    --  The following demos write files.
    Launch_HAC (+"file_copy.adb");
    Launch_HAC (+"three_lakes_s.adb");
    Launch_HAC (+"covid_19_s.adb");
  end Launch_Demos;

begin
  Launch_Demos;
end Gallery;
