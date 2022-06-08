--  Run-time Constraint_Error following a subtype Range Check

procedure CE_c_2 is
  subtype Alpha is Character range 'A' .. 'Z';
  i : Alpha;
  j : Character := 'a';
begin
  i := j;
end CE_c_2;