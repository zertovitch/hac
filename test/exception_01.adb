with HAL; use HAL;

procedure Exception_01 is
  a : array (1 .. 3) of Integer;
begin
  a (4) := 5;  --  4 is out-of-range -> raises Constraint_Error.
end;
