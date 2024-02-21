--  Algo for auto-complete (e.g. in LEA):
--  Find_Possible_Declarations in HAC_Sys.Targets.Semantics
--
--  Identifier table: hac -d auto_complete.adb
--
procedure Auto_Complete is
  a : Integer := 0;
  procedure Sub (b: Boolean) is
    c : Integer := a;
  begin
    --  Here a, c, b are visible
    c := c + a;
  end;
  d : Integer;
begin
  --  Here a, d are visible
  d := a;
end Auto_Complete;
