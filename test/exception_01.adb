procedure Exception_01 is
  a : array (1 .. 3) of Integer;
begin
  a (4) := 1234;  --  4 is out-of-range -> raises Constraint_Error.
end Exception_01;
