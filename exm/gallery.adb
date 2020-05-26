--  We launch new instances of hax (possibly from hax too).
--  Usage: hax gallery.adb

with HAC_Pack;  use HAC_Pack;

procedure Gallery is

  procedure Launch_Demos is

    procedure Shell (command : VString; echo : Boolean) is
      dummy : Integer;
    begin
      if echo then
        Put_Line ("Executing: [" & command & ']');
      end if;
      dummy := Shell_Execute (command);
    end Shell;

    procedure Launch_HAX (Ada_file_name : VString) is
      dummy : Character;
    begin
      Shell (
        +".." & Directory_Separator & "hax -v2 " & Ada_file_name,
        False
      );
      Put ("--- Press any key to continue in the HAC gallery...");
      Get_Immediate (dummy);
      New_Line;
    end Launch_HAX;

    procedure Build_HAX is
    begin
      if Get_Env("haxbuild") = "done" then
        return;
      end if;   
      Put_Line ("(Re-)building HAX, in case the present program isn't run from HAX...");
      Shell (+"gprbuild -p -P .." & Directory_Separator & "hac", True);
    end Build_HAX;

  begin
    Build_HAX;  --  Redundant if this program is itself run through HAX.
    --
    Launch_HAX (+"hello.adb");
    Launch_HAX (+"strings_demo.adb");
    Launch_HAX (+"env.adb");
    Launch_HAX (+"arguments.adb arg1 arg2 ""arg 3 ..."" arg4");
    Launch_HAX (+"ackermann.adb");
    Launch_HAX (+"anti_primes.adb");
    Launch_HAX (+"doors.adb");
    Launch_HAX (+"mandelbrot.adb");
    Launch_HAX (+"test.adb");
    Launch_HAX (+"test1.adb");
    Launch_HAX (+"shell_sort.adb");
    Launch_HAX (+"merge_sort.adb");
    Launch_HAX (+"days_1901.adb");
    Launch_HAX (+"shell.adb");
    Launch_HAX (+"file_read.adb");
    --  The following demos write files.
    Launch_HAX (+"file_copy.adb");
    Launch_HAX (+"three_lakes_s.adb");
  end Launch_Demos;

begin
  Launch_Demos;
end Gallery;
