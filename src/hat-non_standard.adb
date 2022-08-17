--  GNAT version.

with Interfaces.C;

separate (HAT) package body Non_Standard is

  function GNAT_Sys (Arg : Interfaces.C.char_array) return Interfaces.C.int;
  pragma Import (C, GNAT_Sys, "system");

  procedure Sys (Command : String; Result : out Integer) is
  --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
  begin
    Result := Integer (GNAT_Sys (Interfaces.C.To_C (Command)));
  end Sys;

  GNAT_Directory_Separator : constant Character;
  pragma Import (C, GNAT_Directory_Separator, "__gnat_dir_separator");

  function Directory_Separator return Character is
  begin
    return GNAT_Directory_Separator;
  end Directory_Separator;

end Non_Standard;
