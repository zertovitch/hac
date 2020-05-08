--  We launch new instances of hax (possibly from hax too).
--  Usage: hax all_silent_tests.adb

with HAC_Pack;  use HAC_Pack;

procedure All_Silent_Tests is

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
      dummy := Shell_Execute ("..\hax -v1 " & Ada_file_name);
    end Launch_HAX;

    procedure Build_HAX is
      dummy : Integer;
    begin
      dummy :=
        Shell_Execute (+"gprbuild -P .." & Directory_Separator (k) & "hac");
    end Build_HAX;

  begin
    k := Determine_OS_Kind;
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
  end Launch_Tests;

begin
  Launch_Tests;
end All_Silent_Tests;
