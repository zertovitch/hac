procedure Exception_01 is
  a : array (1 .. 3) of Integer;
  i : Integer := 4;
begin
  --  a (4) := 1234;  --  Compile-time error: "error in range constraint:
                      --  value of index (4) is out of the array's range, 1 .. 3"
  a (i) := 1234;  --  4 is out-of-range -> raises Constraint_Error.
end Exception_01;
