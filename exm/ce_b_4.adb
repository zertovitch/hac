--  Run-time Constraint_Error following a array Range Check

procedure CE_b_4 is

  subtype Yes_Men is Boolean range True .. True;

  key : array (Yes_Men) of Boolean;

  i : Boolean := False;

begin
  key (i) := True;
end CE_b_4;