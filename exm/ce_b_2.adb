--  Run-time Constraint_Error following a subtype Range Check

procedure CE_b_2 is
  subtype Yes_Men is Boolean range True .. True;
  i : Yes_Men;
  j : Boolean := False;
begin
  i := j;
end CE_b_2; 