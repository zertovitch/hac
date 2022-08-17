with HAT;

procedure Digitz is
  use HAT;

  procedure Smalls is
    --  Check accuracy of decimal image without exponent ("E...")
    --  Was a problem till rev. 241.
  begin
    Put_Line ("[00] " & Image (Real (0.0)));
    Put_Line ("[01] " & Image (Real (1.0E-10)));
    Put_Line ("[02] " & Image (Real (0.0000000001)));
    Put_Line ("[03] " & Image (Real (0.00000000012)));
    Put_Line ("[04] " & Image (Real (0.000000000123)));
    Put_Line ("[05] " & Image (Real (0.0000000001234)));
    Put_Line ("[06] " & Image (Real (0.00000000012345)));
    Put_Line ("[07] " & Image (Real (0.000000000123456)));
    Put_Line ("[08] " & Image (Real (0.0000000001234567)));
    Put_Line ("[09] " & Image (Real (0.00000000012345678)));
    Put_Line ("[10] " & Image (Real (0.000000000123456789)));
    Put_Line ("[11] " & Image (Real (0.00000000012345678901)));
    Put_Line ("[12] " & Image (Real (0.00000000012345678901234)));
    Put_Line ("[13] " & Image (Real (0.00000000012345678901234567)));
    Put_Line ("[14] " & Image (Real (0.00000100012345678901234567)));
    Put_Line ("[15] " & Image (Real (0.0000010001)));
    Put_Line ("[16] " & Image (Real (0.00000100012345)));
    Put_Line ("[17] " & Image (Real (0.000001000123456)));
    --
    Put_Line ("[18] " & Real'Image (7.77e-5));
    Put_Line ("[19] " & Real'Image (7.77e-15));
    Put_Line ("[20] " & Real'Image (7.77e-100));
  end Smalls;

  procedure Larges is
    function RImage (r : Real) return VString is
    --  Solve Real vs. Duration ambiguity with literals.
    begin
      return Image (r);
    end RImage;
  begin
    Put_Line ("[50] " & RImage (1.0));
    Put_Line ("[51] " & RImage (12.0));
    Put_Line ("[52] " & RImage (123.0));
    Put_Line ("[53] " & RImage (1234.0));
    Put_Line ("[54] " & RImage (12345.0));
    Put_Line ("[55] " & RImage (123456.0));
    Put_Line ("[56] " & RImage (1234567.0));
    Put_Line ("[57] " & RImage (12345678.0));
    Put_Line ("[58] " & RImage (123456789.0));
    Put_Line ("[59] " & RImage (1234567890.0));
    Put_Line ("[60] " & RImage (12345678901.0));
    Put_Line ("[61] " & RImage (123456789012.0));
    Put_Line ("[62] " & RImage (1234567890123.0));
    Put_Line ("[63] " & RImage (12345678901234.0));
    Put_Line ("[64] " & RImage (123456789012345.0));
    Put_Line ("[65] " & RImage (1234567890123456.0));
    Put_Line ("[66] " & RImage (12345678901234567.0));
    Put_Line ("[67] " & RImage (123456789012345678.0));
    Put_Line ("[68] " & RImage (123456789012345678.0 * 10.0));
    Put_Line ("[69] " & RImage (123456789012345678.0 * 100.0));
    Put_Line ("[70] " & RImage (-12345678901.0));
    Put_Line ("[71] " & RImage (-123456789012.0));
    Put_Line ("[72] " & RImage (-1234567890123.0));
    Put_Line ("[73] " & RImage (-12345678901234.0));
    Put_Line ("[74] " & RImage (-123456789012345.0));
    Put_Line ("[75] " & RImage (-1234567890123456.0));
    Put_Line ("[76] " & RImage (-12345678901234567.0));
    Put_Line ("[77] " & RImage (-123456789012345678.0));
    Put_Line ("[78] " & RImage (-123456789012345678.0 * 10.0));
    Put_Line ("[79] " & RImage (-123456789012345678.0 * 100.0));
  end Larges;

begin
  Smalls;
  Larges;
end Digitz;
