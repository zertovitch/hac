with HAC_PDF_Out;
with HAT;

procedure PDF_Hello is
  use HAC_PDF_Out, HAT;

  pdf : PDF_Out_File;

  c1, c2, black : Color_Type;

begin
  Create (pdf, +"hello.pdf");
  Creator_Application (pdf, +"PDF_Hello");
  Keywords (pdf, +"Hello, World, HAC, PDF");
  Put_Line (pdf, +"Hello world !");
  New_Line (pdf);
  Put_Line (pdf, +"This PDF document was created by...");
  Font (pdf, Helvetica);
  Text_Rendering_Mode (pdf, fill_then_stroke);
  --  The following can be done in a more compact way with
  --  a "full Ada" compiler: pdf.Stroking_Color ((0.0, 0.3, 0.1));
  c1.red   := 0.0;
  c1.green := 0.3;
  c1.blue  := 0.1;
  Stroking_Color (pdf, c1);
  c2.red   := 0.1;
  c2.green := 0.5;
  c2.blue  := 0.2;
  Color (pdf, c2);
  Font_Size (pdf, 36.0);
  New_Line (pdf);
  Put_Line (pdf, +"HAC");
  Font_Size (pdf, 18.0);
  Put_Line (pdf, +"HAC Ada Compiler");
  Close (pdf);
end PDF_Hello;
