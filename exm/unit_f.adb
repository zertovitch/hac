with Unit_B;

procedure Unit_F is
  x : Integer := 777;
  use HAT;  --  WITH'ed in unit_f.ads.
begin
  Unit_B (x, +" {Unit_B called from Unit_F} ", +" {Value: 777} ");
  Unit_G;  --  WITH'ed in unit_f.ads.
end Unit_F;
