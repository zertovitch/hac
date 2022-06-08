--  Compile-time Constraint_Error following an array Range Check

procedure CE_i_3 is
  a : array (1 .. 5) of Integer;
begin
  a (7) := 2;
end CE_i_3;
