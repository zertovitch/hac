with Unit_C;
with HAT;

procedure Unit_E is
  use HAT;
begin
  Put (Unit_C
    (+"Unit_C called from Unit_E",
     +"Said otherwise: Unit_E calls Unit_C",
     123));
end Unit_E;
