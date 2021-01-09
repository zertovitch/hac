with HAL; use HAL;

procedure Digitz is
  --  Check accuracy of decimal image without exponent ("E...")
  --  Was a problem till rev. 241.
begin
  Put_Line (Image (0.0));
  Put_Line (Image (1.0E-10));
  Put_Line (Image (0.0000000001));
  Put_Line (Image (0.00000000012));
  Put_Line (Image (0.000000000123));
  Put_Line (Image (0.0000000001234));
  Put_Line (Image (0.00000000012345));
  Put_Line (Image (0.000000000123456));
  Put_Line (Image (0.0000000001234567));
  Put_Line (Image (0.00000000012345678));
  Put_Line (Image (0.000000000123456789));
  Put_Line (Image (0.00000000012345678901));
  Put_Line (Image (0.00000000012345678901234));
  Put_Line (Image (0.00000000012345678901234567));
  Put_Line (Image (0.00000100012345678901234567));
  Put_Line (Image (0.0000010001));
  Put_Line (Image (0.00000100012345));
  Put_Line (Image (0.000001000123456));
  --
  Put_Line (Image_Attribute (7.77e-5));
  Put_Line (Image_Attribute (7.77e-15));
  Put_Line (Image_Attribute (7.77e-100));
end;
