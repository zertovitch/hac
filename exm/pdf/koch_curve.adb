--  http://www.rosettacode.org/wiki/Koch_curve#Ada
--  Author: Jesper Quorning
--
--  This version is adapted to HAC's Ada subset.

with HAC_PDF_Out;
with HAT;

procedure Koch_Curve is

   use HAC_PDF_Out, HAT;

   subtype My_Real is HAC_PDF_Out.Real;
   subtype Angle_Deg  is My_Real;
   subtype Level_Type is Integer range 0 .. 7;

   Vertex_Length : constant := 400.0;
   Purple : Color_Type;
   Corner : Point;

   procedure Draw_Image (Level : Level_Type) is

      Current   : Point;
      Direction : Angle_Deg  := 60.0;
      Doc       : PDF_Out_File;

      My_Layout  : Rectangle;
      My_Margins : Margins_Type;

      Deg_To_Rad : constant := 0.01745329251994329576923690768489;  --  Pi / 180

      procedure Koch (Level : Level_Type; Vertex_Length : My_Real) is
         Move, Abs_Point : Point;
      begin
         if Level = 0 then
            Move.x := Vertex_Length * Sin (Direction * Deg_To_Rad);
            Move.y := Vertex_Length * Cos (Direction * Deg_To_Rad);
            Current.x := Current.x + Move.x;
            Current.y := Current.y + Move.y;
            Abs_Point.x := Corner.x + Current.x;
            Abs_Point.y := Corner.y + Current.y;
            Line_To (Doc, Abs_Point);
         else
            Koch (Level - 1, Vertex_Length / 3.0);  Direction := Direction -  60.0;
            Koch (Level - 1, Vertex_Length / 3.0);  Direction := Direction + 120.0;
            Koch (Level - 1, Vertex_Length / 3.0);  Direction := Direction -  60.0;
            Koch (Level - 1, Vertex_Length / 3.0);
         end if;
      end Koch;

   begin
      Current.x := 0.0;
      Current.y := 0.0;
      --
      Create (Doc, +"koch_" & Character'Val (Character'Pos ('0') + Level) & ".pdf");
      Creator_Application (Doc, +"Koch_Curve");
      Title (Doc, +"Koch snowflake curve drawn with HAC_PDF_Out");
      Author (Doc, +"Niels Fabian Helge von Koch");
      Subject (Doc, +"Koch snowflake curve with recursion level" & Level_Type'Image (Level));
      Keywords (Doc, +"Koch, snowflake, curve, fractal");
      Set_A4_portrait (My_Layout);
      Page_Setup (Doc, My_Layout);
      My_Margins.left   := cm_2_5;
      My_Margins.right  := one_cm;
      My_Margins.top    := one_cm;
      My_Margins.bottom := one_cm;
      Set_Margins (Doc, My_Margins);
      Color (Doc, Purple);
      Move (Doc, Corner);
      for Count in 1 .. 3 loop
         Koch (Level, Vertex_Length);
         Direction := Direction + 120.0;
      end loop;
      Finish_Path (Doc, True, fill, even_odd);
      Close (Doc);
   end Draw_Image;

begin
   Purple.red   := 0.35;
   Purple.green := 0.0;
   Purple.blue  := 0.25;
   --
   Corner.x := 90.0;
   Corner.y := 580.0;
   --
   for Level in Level_Type loop
      Draw_Image (Level);
   end loop;
end Koch_Curve;
