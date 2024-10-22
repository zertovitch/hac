--  https://rosettacode.org/wiki/Hilbert_curve#Ada
--  Author: Jesper Quorning
--
--  This version is adapted to HAC's Ada subset.

with HAT;
with HAC_PDF_Out;

procedure Hilbert_Curve is

   use HAT, HAC_PDF_Out;

   Page_Length : constant := 500.0;
   Corner_X    : constant := 50.0;
   Corner_Y    : constant := 300.0;

   type Rule_Type is (A, B, C, D);

   PDF   : HAC_PDF_Out.PDF_Out_File;
   First : Boolean;

   procedure Hilbert (Order  : in Natural;
                      Rule   : in Rule_Type;
                      Length : in HAC_PDF_Out.Real;
                      X, Y   : in HAC_PDF_Out.Real)
   is
      L : constant HAC_PDF_Out.Real := Length / 4.0;
      P : Point;
   begin
      if Order = 0 then
         P.x := Corner_X + X;
         P.y := Corner_Y + Y;
         if First then
            First := False;
            Move (PDF, P);
         else
            Line_To (PDF, P);
         end if;
      else
         case Rule is
            when A =>
               Hilbert (Order - 1, D, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, A, 2.0 * L, X - L, Y - L);
               Hilbert (Order - 1, A, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X + L, Y + L);
            when B =>
               Hilbert (Order - 1, C, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X - L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, A, 2.0 * L, X + L, Y + L);
            when C =>
               Hilbert (Order - 1, B, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, C, 2.0 * L, X + L, Y + L);
               Hilbert (Order - 1, C, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X - L, Y - L);
            when D =>
               Hilbert (Order - 1, A, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X + L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, C, 2.0 * L, X - L, Y - L);
         end case;
      end if;
   end Hilbert;

   procedure Hilbert_Page (Order : Natural; Color : Color_Type) is
   begin
      First := True;
      Stroking_Color (PDF, Color);
      Hilbert (Order, A, Page_Length, Page_Length / 2.0, Page_Length / 2.0);
      Finish_Path (PDF, False,
                        stroke,
                        nonzero_winding_number);
   end Hilbert_Page;

   a4p, back : Rectangle;
   black, violet, green : Color_Type;

begin
   HAC_PDF_Out.Create (PDF, +"hilbert-curve.pdf");
   Set_A4_Portrait (a4p);
   Page_Setup (PDF, a4p);
   black.red   := 0.0;
   black.green := 0.0;
   black.blue  := 0.0;
   violet.red   := 0.9;
   violet.green := 0.1;
   violet.blue  := 0.8;
   green.red   := 0.0;
   green.green := 0.9;
   green.blue  := 0.0;

   for depth in reverse 1 .. 7 loop
      Color (PDF, black);
      back.x_min  := Corner_X;
      back.y_min  := Corner_Y;
      back.width  := Page_Length;
      back.height := Page_Length;
      Draw (PDF, back, fill);

      if depth = 7 then
         Line_Width (PDF, 1.0);
      else
         Line_Width (PDF, 2.0);
      end if;
      Hilbert_Page (depth,     violet);
      Hilbert_Page (depth - 1, green);

      if depth > 1 then
         New_Page (PDF);
      end if;
   end loop;

   HAC_PDF_Out.Close (PDF);
end Hilbert_Curve;
