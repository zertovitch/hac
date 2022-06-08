--  Run-time Constraint_Error following a array Range Check

procedure CE_i_4 is

  subtype Zero_to_Nine is Integer range 0 .. 9;

  key : array (Zero_to_Nine) of Boolean;
 
  i : Integer := -3;
 
begin
  key (i) := True;
end CE_i_4;