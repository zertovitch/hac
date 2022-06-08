--  Run-time Constraint_Error following a array Range Check

procedure CE_c_4 is

  subtype Alpha is Character range 'A' .. 'Z';
  key : array (Alpha) of Boolean;
  i : Character := 'a';
  
begin
  key (i) := True;
end CE_c_4;