--  We launch new instances of hax (possibly from hax too).
--  Usage: hax gallery.adb

with HAC_Pack;  use HAC_Pack;

procedure Gallery is

  type OS_Kind is (Nixux, Windoze);

  function Determine_OS_Kind return OS_Kind is
  begin
    if Index (Get_Env ("OS"), "Windows") > 0 then
      return Windoze;
    else
      return Nixux;
    end if;
  end Determine_OS_Kind;

  function Directory_Separator (k : OS_Kind) return Character is
  begin
    case k is
      when Nixux   => return '/';
      when Windoze => return '\';
    end case;
  end Directory_Separator;

  procedure Launch_Tests is
    k : OS_Kind;

    procedure Launch_HAX (Ada_file_name : VString) is
      dummy : Integer;
    begin
      dummy := Shell_Execute (
        +".." & Directory_Separator (k) &
        "hax -v2 " & Ada_file_name
      );
      Put ("--- Press Return to continue in the HAC gallery...");
      Skip_Line;
    end Launch_HAX;

    procedure Build_HAX is
      dummy : Integer;
    begin
      Put_Line ("(Re-)building HAX, in case the present program isn't run from HAX...");
      dummy :=
        Shell_Execute (+"gprbuild -P .." & Directory_Separator (k) & "hac");
    end Build_HAX;

  begin
    k := Determine_OS_Kind;
    Build_HAX;  --  Redundant if this program is run through HAX.
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
  end Launch_Tests;

begin
  Launch_Tests;
end Gallery;
