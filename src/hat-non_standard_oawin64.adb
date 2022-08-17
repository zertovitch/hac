--  ObjectAda64 for Windows version.

--  with Ada.Unchecked_Conversion;
--  with Win32.Crt.Process;

with Interfaces.C;

separate (HAT) package body Non_Standard is

  function MS_Sys (Arg : Interfaces.C.char_array) return Interfaces.C.int;
  pragma Import (C, MS_Sys, "system");

  --  You may (or not) need to add the following search path to the OA .prj project:
  --    C:\Program Files (x86)\PTC\ObjectAda64\win32ada\binding\lib\
  --
  --  In ADA.LIB (in, e.g., the hac_objectada-Win32(Intel)-Debug directory):
  --
  --         PATH
  --           PATHNAME: ..\..\..\program files (x86)\ptc\objectada64\lib\rts
  --  --->     PATHNAME: ..\..\..\program files (x86)\ptc\objectada64\win32ada\binding\lib
  --         ENDPATH

  procedure Sys (Command : String; Result : out Integer) is
    --  cmd_nul : aliased Interfaces.C.char_array := Interfaces.C.To_C (Command);
    --  type p_Char is access all Interfaces.C.char_array;
    --  p_cmd_nul : p_Char := cmd_nul'Access;
    --  function Convert is new Ada.Unchecked_Conversion (p_Char, Win32.PCSTR);
  begin
    Result := Integer (MS_Sys (Interfaces.C.To_C (Command)));
    --  Result := Integer (Win32.Crt.Process.System (Convert (p_cmd_nul)));
  end Sys;
  
  function Directory_Separator return Character is
  begin
    return '\';
  end Directory_Separator;

end Non_Standard;
