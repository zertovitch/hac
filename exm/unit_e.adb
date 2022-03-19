with Unit_C;
with HAL;

procedure Unit_E is
  use HAL;
begin
  Put (Unit_C
    (+"Unit_C called from Unit_E",
     +"Said otherwise: Unit_E calls Unit_C",
     123));
end Unit_E;
