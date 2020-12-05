--  ObjectAda64 for Windows version.

separate (HAC_Pack)

package body Non_Standard is

  procedure Sys (Command : String; Result : out Integer) is
  begin
    raise Program_Error with "System call not yet implemented";
  end Sys;

  function Directory_Separator return Character is
  begin
    return '\';
  end Directory_Separator;

end Non_Standard;
