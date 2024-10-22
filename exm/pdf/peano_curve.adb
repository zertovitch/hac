--  https://rosettacode.org/wiki/Peano_curve#Ada
--  Author: Jesper Quorning
--
--  This version is adapted to HAC's Ada subset.

with HAT;
with HAC_PDF_Out;

procedure Peano_Curve is

   use HAT, HAC_PDF_Out;

   Filename   : constant String (1 .. 15) := "peano-curve.pdf";
   Scale      : constant := 2.1;
   Line_W     : constant := 2.5;
   Corner_X   : constant := 150.0;
   Corner_Y   : constant := 50.0;
   Background : Color_Type;
   Frame      : Rectangle;
   PDF        : PDF_Out_File;

   type Coord is record
      X, Y : Natural;
   end record;

   procedure Transform (left, right : Coord; scale_right : Natural; result : out Coord) is
   begin
      result.X := left.X + scale_right * right.X;
      result.Y := left.Y + scale_right * right.Y;
   end Transform;

   procedure Peano (Pos : Coord; Length : Positive; I1, I2 : Integer) is
      len : constant Integer := Length / 3;
      pt, new_pos : Coord;
      pdf_pt : Point;
   begin
      if Length = 1 then
         pdf_pt.x := Corner_X + Scale * (HAC_PDF_Out.Real (3 * Pos.X));
         pdf_pt.y := Corner_Y + Scale * (HAC_PDF_Out.Real (3 * Pos.Y));
         Line_To (PDF, pdf_pt);
      else
         pt.X := 2 * I1;      pt.Y := 2 * I1;      Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     I2);
         pt.X := I1 - I2 + 1; pt.Y := I1 + I2;     Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     1 - I2);
         pt.X := 1;           pt.Y := 1;           Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     1 - I2);
         pt.X := I1 + I2;     pt.Y := I1 - I2 + 1; Transform (Pos, pt, len, new_pos); Peano (new_pos, len, 1 - I1, 1 - I2);
         pt.X := 2 * I2;      pt.Y := 2 - 2 * I2;  Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     I2);
         pt.X := 1 + I2 - I1; pt.Y := 2 - I1 - I2; Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     I2);
         pt.X := 2 - 2 * I1;  pt.Y := 2 - 2 * I1;  Transform (Pos, pt, len, new_pos); Peano (new_pos, len, I1,     I2);
         pt.X := 2 - I1 - I2; pt.Y := 1 + I2 - I1; Transform (Pos, pt, len, new_pos); Peano (new_pos, len, 1 - I1, I2);
         pt.X := 2 - 2 * I2;  pt.Y := 2 * I2;      Transform (Pos, pt, len, new_pos); Peano (new_pos, len, 1 - I1, I2);
      end if;
   end Peano;

   procedure Draw_Peano (order : Natural) is
      black : Color_Type;
      pt : Coord;
      pdf_pt : Point;
   begin
      black.red   := 0.0;
      black.green := 0.0;
      black.blue  := 0.0;
      Stroking_Color (PDF, black);
      Line_Width (PDF, Line_W);
      pdf_pt.x := Corner_X;
      pdf_pt.y := Corner_Y;
      Move (PDF, pdf_pt);
      pt.X := 0;
      pt.Y := 0;
      Peano (pt, 3**order, 0, 0);
      Finish_Path (PDF, False,
                        stroke,
                        nonzero_winding_number);
   end Draw_Peano;

   a4p : Rectangle;

begin
   Background.red   := 0.827;
   Background.green := 0.816;
   Background.blue  := 0.016;

   Frame.x_min  := 10.0;
   Frame.y_min  := 10.0;
   Frame.width  := 820.0;
   Frame.height := 575.0;

   HAC_PDF_Out.Create (PDF, +Filename);
   Set_A4_Landscape (a4p);
   Page_Setup (PDF, a4p);

   for order in reverse 1 .. 4 loop
      Color (PDF, Background);
      Draw (PDF, Frame, fill);

      Draw_Peano (order);
      if order > 1 then
         New_Page (PDF);
      end if;
   end loop;

   HAC_PDF_Out.Close (PDF);
end Peano_Curve;
