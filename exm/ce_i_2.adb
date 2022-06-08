--  Run-time Constraint_Error following a subtype Range Check

procedure CE_i_2 is

  subtype Zero_to_Nine is Integer range 0 .. 9;

  subtype Negative_Temp is Integer range -273 .. -1;

  i : Positive;
  j : Integer := 1;
  
begin
  i := -j;
end CE_i_2; 