package body HAC_PDF_Out is

  use HAT, Interfaces;

  procedure Set_A4_portrait (r : out Rectangle) is
  begin
    r.x_min  := 0.0;
    r.y_min  := 0.0;
    r.width  := 21.0 * one_cm;
    r.height := 29.7 * one_cm;
  end Set_A4_portrait;

  procedure Init (pdf : in out PDF_Out_File) is
    A4_portrait : Rectangle;
    cm_2_5_margins : Margins_Type;
  begin
    Set_A4_portrait (A4_portrait);
    --
    cm_2_5_margins.left   := cm_2_5;
    cm_2_5_margins.right  := cm_2_5;
    cm_2_5_margins.top    := cm_2_5;
    cm_2_5_margins.bottom := cm_2_5;
    --
    pdf.is_created     := False;
    pdf.is_closed      := False;
    pdf.file_index     := 1;
    pdf.file_name      := Null_VString;
    pdf.format         := PDF_1_3;
    pdf.zone           := nowhere;
    pdf.text_switch    := graphics;
    pdf.last_page      := 0;
    pdf.current_line   := 1;  --  Mostly for Ada.Text_IO compatibility
    pdf.current_col    := 1;  --  Mostly for Ada.Text_IO compatibility
    pdf.page_box       := A4_portrait;
    pdf.maximum_box    := A4_portrait;
    pdf.page_margins   := cm_2_5_margins;
    pdf.objects        := 1;
    pdf.current_font   := Helvetica;
    pdf.font_size      := 11.0;
    pdf.line_spacing   := 1.2;
    pdf.stream_obj_buf := Null_VString;
    pdf.ext_font_name  := Null_VString;
    pdf.doc_title      := Null_VString;
    pdf.doc_author     := Null_VString;
    pdf.doc_subject    := Null_VString;
    pdf.doc_keywords   := Null_VString;
    pdf.doc_creator    := Null_VString;
  end Init;

  procedure W (pdf : in out PDF_Out_File; s : VString) is
  begin
    HAT.Put (pdf.pdf_file, s);
    pdf.file_index := pdf.file_index + Length (s);
  end W;

  function NL return Character is begin return Character'Val (10); end NL;

  procedure WL (pdf : in out PDF_Out_File; s : VString) is
  begin
    W (pdf, s & NL);
  end WL;

  procedure No_Nowhere (pdf : in out PDF_Out_File) is
  begin
    if pdf.zone = nowhere then
      New_Page (pdf);
    end if;
  end No_Nowhere;

  --  Delayed output, for internal PDF's "stream" object

  procedure Wd (pdf : in out PDF_Out_File; s : VString) is
  begin
    No_Nowhere (pdf);
    pdf.stream_obj_buf := pdf.stream_obj_buf & s;
  end Wd;

  procedure WLd (pdf : in out PDF_Out_File; s : VString) is
  begin
    Wd (pdf, s & NL);
  end WLd;

  --  External stream index

  function Buffer_index (pdf : PDF_Out_File) return Natural is
  begin
    return pdf.file_index - pdf.start_index;
  end Buffer_index;

  function Img_I (p : Integer) return VString is
  begin
    return HAT.Image (p);
  end Img_I;

  function Img_R (x : Real) return VString is
    im : VString := HAT.Image (x);
  begin
    if Slice (im, 1, 2) = "0." then
      im := Slice (im, 2, Length (im));
    elsif Slice (im, 1, 3) = "-0." then
      im := '-' & Slice (im, 3, Length (im));
    end if;
    if im = ".0" then
      return +"0";
    end if;
    if Slice (im, Length (im) - 1, Length (im)) = ".0" then
      return Slice (im, 1, Length (im) - 2);  --  Return as integer
    end if;
    return im;
  end Img_R;

  --  function "+"(P1, P2 : Point) return Point is
  --  begin
  --    return (P1.x + P2.x, P1.y + P2.y);
  --  end "+";
  --
  --  function "*"(f : Real; P : Point) return Point is
  --  begin
  --    return (f * P.x, f * P.y);
  --  end "*";
  --
  --  function "+"(P : Point; r : Rectangle) return Rectangle is
  --  begin
  --    return (P.x + r.x_min, P.y + r.y_min, r.width, r.height);
  --  end "+";
  --
  --  function "*"(f : Real; r : Rectangle) return Rectangle is
  --  begin
  --    return (r.x_min, r.y_min, f * r.width, f * r.height);
  --  end "*";

  function X_Max (r : Rectangle) return Real is
  begin
    return r.x_min + r.width;
  end X_Max;

  function Y_Max (r : Rectangle) return Real is
  begin
    return r.y_min + r.height;
  end Y_Max;

  type Abs_Rel_Mode is (absolute, relative);

  function Img_P (p : Point) return VString is
  begin
    return Img_R (p.x) & ' ' & Img_R (p.y);
  end Img_P;

  function Img_Rect (box : Rectangle; mode : Abs_Rel_Mode) return VString is
  begin
    case mode is
      when absolute =>
        return Img_R (box.x_min) & ' ' & Img_R (box.y_min) & ' ' &
               Img_R (X_Max (box)) & ' ' & Img_R (Y_Max (box)) & ' ';
      when relative =>
        return Img_R (box.x_min) & ' ' & Img_R (box.y_min) & ' ' &
               Img_R (box.width) & ' ' & Img_R (box.height) & ' ';
    end case;
  end Img_Rect;

  procedure New_fixed_index_object (pdf : in out PDF_Out_File; idx : PDF_Index_Type) is
  begin
    pdf.object_offset (idx) := Buffer_index (pdf);
    WL (pdf, Img_I (Integer (idx)) & " 0 obj");
  end New_fixed_index_object;

  procedure New_object (pdf : in out PDF_Out_File) is
  begin
    pdf.objects := pdf.objects + 1;
    New_fixed_index_object (pdf, pdf.objects);
  end New_object;

  function producer return VString is
  begin
    return +"HAC_PDF_Out, a version of Ada PDF Writer (http://apdf.sf.net/), " &
            "stripped down for the HAC Ada Compiler language subset.";
  end producer;

  procedure Write_PDF_header (pdf : in out PDF_Out_File) is
    sig : String (1 .. 8);
  begin
    pdf.is_created := True;
    pdf.start_index := pdf.file_index;
    case pdf.format is
      when PDF_1_3 =>
        WL (pdf, +"%PDF-1.3");
        sig (1) := Character'Val (16#25#);
        sig (2) := Character'Val (16#C2#);
        sig (3) := Character'Val (16#A5#);
        sig (4) := Character'Val (16#C2#);
        sig (5) := Character'Val (16#B1#);
        sig (6) := Character'Val (16#C3#);
        sig (7) := Character'Val (16#AB#);
        sig (8) := Character'Val (10);
        W (pdf, +sig);
    end case;
    WL (pdf, "%  --  Produced by " & producer);
  end Write_PDF_header;

  procedure New_substream (pdf : in out PDF_Out_File) is
  begin
    pdf.stream_obj_buf := Null_VString;
  end New_substream;

  procedure Finish_substream (pdf : in out PDF_Out_File) is
  begin
    WL (pdf, +"  << /Length" & Integer'Image (Length (pdf.stream_obj_buf)) & " >>");
    --  Length could be alternatively stored in next object,
    --  so we wouldn't need to buffer the stream - see 7.3.10, Example 3.
    --  But we prefer the buffered version, which could be compressed in a future version
    --  of this package.
    WL (pdf, +"stream");
    WL (pdf, pdf.stream_obj_buf);
    WL (pdf, +"endstream");
  end Finish_substream;

  --  Internal - test page for experimenting PDF constructs (and how Adobe Reader reacts to them)
  --
  procedure Test_Page (pdf : in out PDF_Out_File) is
  begin
    WLd (pdf, +"10 10 200 200 re S"); -- rectangle, stroke
    WLd (pdf, +"  BT");            --  Begin Text object (9.4). Text matrix and text line matrix:= I
    WLd (pdf, +"    /Ada_PDF_Std_Font_Helvetica 24 Tf");   --  F1 font, 24 pt size (9.3 Text State Parameters and Operators)
    WLd (pdf, +"    0.5 0 0 rg");  --  red, nonstroking colour (Table 74)
    WLd (pdf, +"    0.25 G");     --  25% gray stroking colour (Table 74)
    WLd (pdf, +"    2 Tr");        --  Tr: Set rendering mode as "Fill, then stroke text" (Table 106)
    WLd (pdf, +"    20 539 Td");
    WLd (pdf, +"    (Hello World !) Tj"); -- Tj: Show a text string (9.4.3 Text-Showing Operators)
    WLd (pdf, +"    16 TL");       --  TL: set text leading (distance between lines, 9.3.5)
    WLd (pdf, +"    T*");          --  T*: Move to the start of the next line (9.4.2)
    WLd (pdf, +"    20 20 200 200 re S"); -- rectangle, stroke (within text region)
    WLd (pdf, +"    /Ada_PDF_Std_Font_Helvetica-Oblique 12 Tf");
    WLd (pdf, +"    0 Tr");        --  Tr: Set rendering mode as default: "Fill text" (Table 106)
    WLd (pdf, +"    0 g");         --  black (default)
    WLd (pdf, +"    (Subtitle here.) Tj T*");
    WLd (pdf, +"  ET");           --  End Text
    WLd (pdf, +"30 30 200 200 re S"); -- rectangle, stroke
    WLd (pdf, +"  BT");
    WLd (pdf, +"    5 5 Td (Second text chunk here.) Tj T*");
    WLd (pdf, +"  ET");
    WLd (pdf, +"40 40 240 240 re S"); -- rectangle, stroke
    WLd (pdf, +"15 15 Td (Text chunk not within BT/ET.) Tj");
  end Test_Page;

  function test_page_mode return Boolean is begin return False; end test_page_mode;

  package Fonts is

    function Standard_Font_Name (f : Standard_Font_Type) return VString;

    --  Font dictionary name within a page
    --
    function Font_Dictionary_Name (font_name : VString) return VString;

    --  Font dictionary name within a page, for standard fonts
    --  Example: /Ada_PDF_Std_Font_Courier-Oblique
    --
    function Standard_Font_Dictionary_Name (f : Standard_Font_Type) return VString;

    --  Output font dictionary (resource for last page)
    --
    procedure Font_Dictionary (pdf : in out PDF_Out_File);

    function Current_Font_Name (pdf : PDF_Out_File) return VString;

    function Current_Font_Dictionary_Name (pdf : PDF_Out_File) return VString;

  end Fonts;

  procedure Insert_PDF_Font_Selection_Code (pdf : in out PDF_Out_File) is
  begin
    Insert_Text_PDF_Code (pdf,
      Fonts.Current_Font_Dictionary_Name (pdf) &
      ' ' & Img_R (pdf.font_size) & " Tf " &  --  Tf: 9.3 Text State Parameters and Operators
      Img_R (pdf.font_size * pdf.line_spacing) & " TL"  --  TL: set text leading (9.3.5)
    );
  end Insert_PDF_Font_Selection_Code;

  procedure Font (pdf : in out PDF_Out_File; f : Standard_Font_Type) is
  begin
    pdf.current_font := f;
    Insert_PDF_Font_Selection_Code (pdf);
  end Font;

  procedure Font_Size (pdf : in out PDF_Out_File; size : Real) is
  begin
    pdf.font_size := size;
    Insert_PDF_Font_Selection_Code (pdf);
  end Font_Size;

  procedure Line_Spacing (pdf : in out PDF_Out_File; factor : Real) is
  begin
    pdf.line_spacing := factor;
    Insert_PDF_Font_Selection_Code (pdf);
  end Line_Spacing;

  procedure Line_Spacing_Pt (pdf : in out PDF_Out_File; pt : Real) is
  begin
    pdf.line_spacing := pt / pdf.font_size;
    --  !! This assumes that the font size is in Point (pt) units.
    Insert_PDF_Font_Selection_Code (pdf);
  end Line_Spacing_Pt;

  procedure Begin_text (pdf : in out PDF_Out_File) is
  begin
    WLd (pdf,  +"  BT");  --  Begin Text object (9.4.1, Table 107)
  end Begin_text;

  procedure End_text (pdf : in out PDF_Out_File) is
  begin
    WLd (pdf,  +"  ET");
  end End_text;

  procedure Flip_to (pdf : in out PDF_Out_File; new_state : Text_or_graphics) is
  begin
    No_Nowhere (pdf);
    --  WLd(pdf,  " % Text_or_graphics before: " & pdf.text_switch'Image);
    if pdf.text_switch /= new_state then
      pdf.text_switch := new_state;
      case new_state is
        when text     => Begin_text (pdf);
        when graphics => End_text (pdf);
      end case;
    end if;
    --  WLd(pdf,  " % Text_or_graphics after: " & pdf.text_switch'Image);
  end Flip_to;

  function pages_idx return Integer is begin return 1; end pages_idx;

  procedure New_Page (pdf : in out PDF_Out_File) is
  begin
    if pdf.zone /= nowhere then
      Finish_Page (pdf);
    end if;
    pdf.last_page := pdf.last_page + 1;
    pdf.current_line := 1;
    pdf.current_col := 1;
    --  PDF_Out.Images.Clear_local_resource_flags (pdf);
    --
    --  Page descriptor object:
    --
    New_object (pdf);
    pdf.page_idx (pdf.last_page) := pdf.objects;
    --  Table 30 (7.7.3.3 Page Objects) for options
    WL (pdf, +"  <</Type /Page");
    WL (pdf, +"    /Parent " & Img_I (pages_idx) & " 0 R");
    --  Contents stream object is object number n+1 (our choice):
    WL (pdf, +"    /Contents " & Img_I (Integer (pdf.objects + 1)) & " 0 R");
    --  Resources: a dictionary containing any resources required by the page.
    --  Resources object is object number n+2 (our choice):
    WL (pdf, +"    /Resources " & Img_I (Integer (pdf.objects + 2)) & " 0 R");
    WL (pdf, +"    /MediaBox [" & Img_Rect (pdf.page_box, absolute) & ']');
    WL (pdf, +"  >>");
    WL (pdf, +"endobj");
    --  Page contents object:
    --
    New_object (pdf);
    New_substream (pdf);
    if test_page_mode then
      Test_Page (pdf);
    else
      pdf.zone := in_page;
      Insert_PDF_Font_Selection_Code (pdf);
      pdf.zone := in_header;
      --  No dispatching in the HAC version -> no custom header!
      Page_Header (pdf);
    end if;
    pdf.zone := in_page;
    Text_XY (pdf, pdf.page_margins.left, Y_Max (pdf.page_box) - pdf.page_margins.top);
  end New_Page;

  procedure Finish_Page (pdf : in out PDF_Out_File) is

    appended_object_idx : PDF_Index_Type;

  begin
    if pdf.zone = nowhere then
      return;  --  We are already "between pages"
    end if;
    if test_page_mode then
      null;  --  Nothing to do anymore with test page
    else
      pdf.zone := in_footer;
      --  No dispatching in the HAC version -> no custom  footer!
      Page_Footer (pdf);
      Flip_to (pdf, graphics);
    end if;
    pdf.zone := nowhere;
    Finish_substream (pdf);
    WL (pdf, +"endobj");  --  end of page contents.
    --  Resources Dictionary (7.8.3) for the page just finished:
    New_object (pdf);
    WL (pdf, +"<<");
    --  Font resources:
    Fonts.Font_Dictionary (pdf);
    appended_object_idx := pdf.objects + 1;  --  Images contents to be appended after this object
    --  Image resources:
    WL (pdf, +"  /XObject <<");
    --  Image_List (pdf);
    WL (pdf, +"  >>");
    WL (pdf, +">>");
    WL (pdf, +"endobj");  --  end of Resources
    --  PDF_Out.Images.Insert_unloaded_local_images (pdf);
  end Finish_Page;

  procedure Put_Real (pdf  : in out PDF_Out_File;
                      num  : in Real
            )
  is
  begin
    Put_Str (pdf, Image (num));
  end Put_Real;

  procedure Put_Int (pdf   : in out PDF_Out_File;
                     num   : in Integer
            )
  is
  begin
    Put_Str (pdf, Image (num));
  end Put_Int;

  procedure Put_Str (pdf : in out PDF_Out_File; str : VString) is
  begin
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      Insert_Text_PDF_Code (pdf, '(' & str & ") Tj");
    end if;
  end Put_Str;

  procedure Put_Line (pdf : in out PDF_Out_File; str : VString) is
  begin
    Put_Str (pdf, str);
    New_Line (pdf);
  end Put_Line;

  procedure New_Line (pdf : in out PDF_Out_File) is
    Spacing : constant := 1;
  begin
    pdf.current_line := pdf.current_line + 1;
    pdf.current_col := 1;
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      for i in 1 .. Spacing loop
        Insert_Text_PDF_Code (pdf, +"T*");
      end loop;
    end if;
  end New_Line;

  procedure Text_XY (pdf : in out PDF_Out_File; x, y : Real) is
  begin
    Flip_to (pdf, text);
    --  The following explicit End_text, Begin_text are just
    --  for resetting the text matrices (hence, position and orientation).
    End_text (pdf);
    Begin_text (pdf);
    Insert_PDF_Code (pdf, Img_R (x) & ' ' & Img_R (y) & " Td");  --  Td: 9.4.2 Text-Positioning Operators
    pdf.current_line := 1;
    pdf.current_col := 1;
  end Text_XY;

  procedure Put_XY (pdf : in out PDF_Out_File; x, y : Real; str : VString) is
  begin
    Text_XY (pdf, x, y);
    Put_Str (pdf, str);
  end Put_XY;

  function Col (pdf : in PDF_Out_File) return Positive is
  begin
    return pdf.current_col;
  end Col;

  function Line (pdf : in PDF_Out_File) return Positive is
  begin
    return pdf.current_line;
  end Line;

  function Page (pdf : in PDF_Out_File) return Natural is
  begin
    return Natural (pdf.last_page);  --  Issue if Integer is 16-bit and last_page > 2**15-1
  end Page;

  procedure Color (pdf : in out PDF_Out_File; c : Color_Type) is
  begin
    Insert_PDF_Code (pdf, Img_R (c.red) & ' ' & Img_R (c.green) & ' ' & Img_R (c.blue) & " rg");
    --  rg = nonstroking colour (Table 74)
  end Color;

  procedure Stroking_Color (pdf : in out PDF_Out_File; c : Color_Type) is
  begin
    Insert_PDF_Code (pdf, Img_R (c.red) & ' ' & Img_R (c.green) & ' ' & Img_R (c.blue) & " RG");
    --  RG = nonstroking colour (Table 74)
  end Stroking_Color;

  procedure Text_Rendering_Mode (pdf : in out PDF_Out_File; r : Rendering_Mode) is
  begin
    Insert_Text_PDF_Code (pdf, Img_I (Integer (Rendering_Mode'Pos (r))) & " Tr");
    --  Tr = Set rendering mode (Table 106)
  end Text_Rendering_Mode;

  function Image_name (i : Positive) return VString is
  begin
    return "/Ada_PDF_Img" & Img_I (i);
  end Image_name;

  --  procedure Image (pdf : in out PDF_Out_File; file_name : String; target : Rectangle) is
  --    image_index : Positive;  --  Index in the list of images
  --  begin
  --    No_Nowhere (pdf);
  --    PDF_Out.Images.Image_ref (pdf, file_name, image_index);
  --    Insert_Graphics_PDF_Code (pdf, "q " &
  --      Img (target.width) & " 0 0 " & Img (target.height) &
  --      ' ' & Img (target.x_min) & ' ' & Img (target.y_min) & " cm " &  --  cm: Table 57
  --      Image_name (image_index) & " Do Q"
  --    );
  --  end Image;
  --
  --  function Get_pixel_dimensions (image_file_name : String) return Rectangle is
  --  begin
  --    return PDF_Out.Images.Get_pixel_dimensions (image_file_name);
  --  end Get_pixel_dimensions;

  -----------------------
  --  Vector graphics  --
  -----------------------

  procedure Line_Width (pdf : in out PDF_Out_File; width : Real) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img_R (width) & " w");
  end Line_Width;

  procedure Single_Line (pdf : in out PDF_Out_File; from, to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf,
      Img_R (from.x) & ' ' & Img_R (from.y) & " m " &
      Img_R (to.x) & ' ' & Img_R (to.y) & " l s"
    );
  end Single_Line;

  --    Table 59 - Path Construction Operators (8.5.2)
  --    Table 60 - Path-Painting Operators (8.5.3.1)

  function inside_path_rule_char (r : Inside_path_rule) return Character is
  begin
    case r is
      when nonzero_winding_number => return ' ';
      when even_odd               => return '*';
    end case;
  end inside_path_rule_char;

  function path_drawing_operator (m : Path_Rendering_Mode) return Character is
  begin
    case m is
      when fill             => return 'F';
      when stroke           => return 'S';
      when fill_then_stroke => return 'B';
    end case;
  end path_drawing_operator;

  procedure Draw (pdf : in out PDF_Out_File; what : Rectangle; rendering : Path_Rendering_Mode) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img_Rect (what, relative) & " re " & path_drawing_operator (rendering));
  end Draw;

  procedure Move (pdf : in out PDF_Out_File; to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img_P (to) & " m");  --  m operator (Table 59)
  end Move;

  procedure Line_To (pdf : in out PDF_Out_File; to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img_P (to) & " l");
  end Line_To;

  procedure Cubic_Bezier (pdf : in out PDF_Out_File; control_1, control_2 : Point; to : Point) is
  begin
    Insert_Graphics_PDF_Code (
      pdf,
      Img_P (control_1) & ' ' &
      Img_P (control_2) & ' ' &
      Img_P (to) & " c"
    );
  end Cubic_Bezier;

  procedure Finish_Path (
    pdf        : in out PDF_Out_File;
    close_path :        Boolean;
    rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
    rule       :        Inside_path_rule
  )
  is
    cmd : VString;
  begin
    cmd := +path_drawing_operator (rendering) & inside_path_rule_char (rule);
    if close_path then
      cmd := To_Lower (cmd);
    end if;
    --  Insert the s, S, f, f*, b, b*, B, B* of Table 60 - Path-Painting Operators (8.5.3.1)
    if cmd = "s*" or cmd = "S*" or cmd = "F " or cmd = "F*" then
      Insert_Graphics_PDF_Code (pdf, +"n");  --  End the path object without filling or stroking it.
    else
      Insert_Graphics_PDF_Code (pdf, cmd);
    end if;
  end Finish_Path;

  -----------------------------
  --  Direct code insertion  --
  -----------------------------

  procedure Insert_PDF_Code (pdf : in out PDF_Out_File; code : VString) is
  begin
    WLd (pdf, "    " & code);  --  Indentation is just cosmetic...
  end Insert_PDF_Code;

  procedure Insert_Text_PDF_Code (pdf : in out PDF_Out_File; code : VString) is
  begin
    Flip_to (pdf, text);
    Insert_PDF_Code (pdf, code);
  end Insert_Text_PDF_Code;

  procedure Insert_Graphics_PDF_Code (pdf : in out PDF_Out_File; code : VString) is
  begin
    Flip_to (pdf, graphics);
    Insert_PDF_Code (pdf, code);
  end Insert_Graphics_PDF_Code;

  --  Table 317 - Entries in the document information dictionary (14.3.3)

  procedure Title (pdf : in out PDF_Out_File; s : VString) is
  begin
    pdf.doc_title := s;
  end Title;

  procedure Author (pdf : in out PDF_Out_File; s : VString) is
  begin
    pdf.doc_author := s;
  end Author;

  procedure Subject (pdf : in out PDF_Out_File; s : VString) is
  begin
    pdf.doc_subject := s;
  end Subject;

  procedure Keywords (pdf : in out PDF_Out_File; s : VString) is
  begin
    pdf.doc_keywords := s;
  end Keywords;

  procedure Creator_Application (pdf : in out PDF_Out_File; s : VString) is
  begin
    pdf.doc_creator := s;
  end Creator_Application;

  procedure Page_Header (pdf : in out PDF_Out_File) is
  begin
    null;  --  Default header is empty.
  end Page_Header;

  procedure Page_Footer (pdf : in out PDF_Out_File) is
  begin
    null;  --  Default footer is empty.
  end Page_Footer;

  procedure Set_Left_Margin (pdf : out PDF_Out_File; pts : Real) is
  begin
    pdf.page_margins.left := pts;
  end Set_Left_Margin;

  function Get_Left_Margin (pdf : PDF_Out_File) return Real is
  begin
    return pdf.page_margins.left;
  end Get_Left_Margin;

  procedure Set_Right_Margin (pdf : out PDF_Out_File; pts : Real) is
  begin
    pdf.page_margins.right := pts;
  end Set_Right_Margin;

  function Get_Right_Margin (pdf : PDF_Out_File) return Real is
  begin
    return pdf.page_margins.right;
  end Get_Right_Margin;

  procedure Set_Top_Margin (pdf : out PDF_Out_File; pts : Real) is
  begin
    pdf.page_margins.top := pts;
  end Set_Top_Margin;

  function Get_Top_Margin (pdf : PDF_Out_File) return Real is
  begin
    return pdf.page_margins.top;
  end Get_Top_Margin;

  procedure Set_Bottom_Margin (pdf : out PDF_Out_File; pts : Real) is
  begin
    pdf.page_margins.bottom := pts;
  end Set_Bottom_Margin;

  function Get_Bottom_Margin (pdf : PDF_Out_File) return Real is
  begin
    return pdf.page_margins.bottom;
  end Get_Bottom_Margin;

  procedure Set_Margins (pdf : out PDF_Out_File; new_margins : Margins_Type) is
  begin
    pdf.page_margins := new_margins;
  end Set_Margins;

  procedure Get_Margins (pdf : PDF_Out_File; result : out Margins_Type) is
  begin
    result := pdf.page_margins;
  end Get_Margins;

  procedure Page_Setup (pdf : in out PDF_Out_File; layout : Rectangle) is
    mb_x_min, mb_y_min, mb_x_max, mb_y_max : Real;
  begin
    pdf.page_box := layout;
    mb_x_min := Min (pdf.maximum_box.x_min, layout.x_min);
    mb_y_min := Min (pdf.maximum_box.y_min, layout.y_min);
    mb_x_max := Max (X_Max (pdf.maximum_box), X_Max (layout));
    mb_y_max := Max (Y_Max (pdf.maximum_box), Y_Max (layout));
    --
    pdf.maximum_box.x_min  := mb_x_min;
    pdf.maximum_box.y_min  := mb_y_min;
    pdf.maximum_box.width  := mb_x_max - mb_x_min;
    pdf.maximum_box.height := mb_y_max - mb_y_min;
  end Page_Setup;

  procedure Layout (pdf : PDF_Out_File; result : out Rectangle) is
  begin
    result := pdf.page_box;
  end Layout;

  procedure Reset (
    pdf        : in out PDF_Out_File;
    PDF_format :        PDF_type
  )
  is
  begin
    Init (pdf);
    --  Check if we are trying to re-use a half-finished object (ouch!):
    if pdf.is_created and not pdf.is_closed then
      null;  --  !!  raise PDF_stream_not_closed;
    end if;
    pdf.format := PDF_format;
    --  Set a default title (replaced when procedure Title is called).
    --  In Adobe Reader, this content can be copied to the clipboard.
    pdf.doc_title := +"Document created with: " & producer;
  end Reset;

  procedure Finish (pdf : in out PDF_Out_File) is

    info_idx, cat_idx : PDF_Index_Type;

    procedure Info is
    begin
      New_object (pdf);
      info_idx := pdf.objects;
      WL (pdf, +"  << /Producer (" & producer & ')');
      WL (pdf, +"     /Title (" & To_String (pdf.doc_title) & ')');
      WL (pdf, +"     /Author (" & To_String (pdf.doc_author) & ')');
      WL (pdf, +"     /Subject (" & To_String (pdf.doc_subject) & ')');
      WL (pdf, +"     /Keywords (" & To_String (pdf.doc_keywords) & ')');
      WL (pdf, +"     /Creator (" & To_String (pdf.doc_creator) & ')');
      WL (pdf, +"  >>");
      WL (pdf, +"endobj");
    end Info;

    procedure Pages_dictionary is
    begin
      New_fixed_index_object (pdf, pages_idx);
      WL (pdf, +"  << /Type /Pages");
      W (pdf,  +"     /Kids [");
      for p in 1 .. pdf.last_page loop
        W (pdf, Img_I (pdf.page_idx (p)) & " 0 R ");
      end loop;
      WL (pdf, +"]");
      if pdf.last_page > 0 then
        WL (pdf, "     /Count " & Img_I (pdf.last_page));
      end if;
      WL (pdf, "     /MediaBox [" & Img_Rect (pdf.maximum_box, absolute) & ']'
      );
      --  7.7.3.3 Page Objects - MediaBox
      --  Boundaries of the physical medium on which the page shall be displayed or printed
      --  7.7.3.4 Inheritance of Page Attributes
      --  Global page size, lower-left to upper-right, measured in points
      --  Bounding box of all pages
      WL (pdf, +"  >>");
      WL (pdf, +"endobj");
    end Pages_dictionary;

    procedure Catalog_dictionary is
    begin
      New_object (pdf);
      cat_idx := pdf.objects;
      WL (pdf, +"  << /Type /Catalog");
      WL (pdf, +"     /Pages " & Img_I (pages_idx) & " 0 R");
      if pdf.last_page > 0 then
        --  Open the document on page 1, fit the
        --  entire page within the window (Table 151):
        WL (pdf, +"     /OpenAction [" & Img_I (pdf.page_idx (1)) & " 0 R /Fit]");
      end if;
      WL (pdf, +"  >>");
      WL (pdf, +"endobj");
    end Catalog_dictionary;

    procedure Trailer is
    begin
      WL (pdf, +"trailer");
      WL (pdf, +"  << /Root " & Img_I (cat_idx) & " 0 R");
      WL (pdf, +"     /Size " & Img_I (pdf.objects + 1));
      WL (pdf, +"     /Info " & Img_I (info_idx) & " 0 R");
      WL (pdf, +"  >>");
    end Trailer;

    xref_offset : Natural;

    procedure XRef is
      s10 : VString;
    begin
      xref_offset := Buffer_index (pdf);
      WL (pdf, +"xref");
      WL (pdf, +"0 " & Img_I (pdf.objects + 1));
      WL (pdf, +"0000000000 65535 f ");
      for i in 1 .. pdf.objects loop
        s10 := Image (pdf.object_offset (i));
        while Length (s10) < 10 loop
          s10 := '0' & s10;
        end loop;
        WL (pdf, s10 & " 00000 n ");  --   <-- the trailing space is needed!
      end loop;
    end XRef;

  begin
    if pdf.last_page = 0 then
      --  No page ? Then make quickly a blank page.
      New_Page (pdf);
    end if;
    Finish_Page (pdf);
    Info;
    Pages_dictionary;
    Catalog_dictionary;
    XRef;
    Trailer;
    WL (pdf, +"startxref"); -- offset of xref
    WL (pdf, Img_I (Integer (xref_offset)));
    WL (pdf, +"%%EOF");
    --  PDF_Out.Images.Clear_image_directory (pdf);
    pdf.is_closed := True;
  end Finish;

  ----------------------
  -- Output to a file --
  ----------------------

  procedure Create (
    pdf        : in out PDF_Out_File;
    file_name  :        VString
  )
  is
  begin
    Reset (pdf, PDF_1_3);
    HAT.Create (pdf.pdf_file, file_name);
    pdf.file_name := file_name;
    Write_PDF_header (pdf);
  end Create;

  procedure Close (pdf : in out PDF_Out_File) is
  begin
    Finish (pdf);
    if pdf.file_name /= "nul" then  --  Test needed for OA 7.2.2 (Close raises Use_Error)
      HAT.Close (pdf.pdf_file);
    end if;
  end Close;

  function Is_Open (pdf : in PDF_Out_File) return Boolean is
  begin
    return HAT.Is_Open (pdf.pdf_file);
  end Is_Open;

  function one_cm   return Real is begin return 28.346456692913385826771653543307; end one_cm; -- = 72.0 / 2.54;
  function cm_2_5   return Real is begin return 70.866141732283464566929133858268; end cm_2_5; -- = one_cm * 2.5;
  function one_inch return Real is begin return 72.0; end one_inch;

  --  !! HAC Bug: presence of subpackage body is not checked !

  package body Fonts is

    --  9.6.2.2  Standard Type 1 Fonts (Standard 14 Fonts)
    function Standard_Font_Name (f : Standard_Font_Type) return VString is
    begin  --  Code generation: see pw_work.xls, Fonts (Std)
      case f is
        when Courier                 => return +"Courier";
        when Courier_Bold            => return +"Courier-Bold";
        when Courier_Bold_Oblique    => return +"Courier-BoldOblique";
        when Courier_Oblique         => return +"Courier-Oblique";
        when Helvetica               => return +"Helvetica";
        when Helvetica_Bold          => return +"Helvetica-Bold";
        when Helvetica_Bold_Oblique  => return +"Helvetica-BoldOblique";
        when Helvetica_Oblique       => return +"Helvetica-Oblique";
        when Symbol                  => return +"Symbol";
        when Times_Bold              => return +"Times-Bold";
        when Times_Bold_Italic       => return +"Times-BoldItalic";
        when Times_Italic            => return +"Times-Italic";
        when Times_Roman             => return +"Times-Roman";
        when Zapf_Dingbats           => return +"ZapfDingbats";
      end case;
    end Standard_Font_Name;

    function Font_Dictionary_Name (font_name : VString) return VString is
    begin
      return "/Ada_PDF_Font_" & font_name;
    end Font_Dictionary_Name;

    function Standard_Font_Dictionary_Name (f : Standard_Font_Type) return VString is
    begin
      return "/Ada_PDF_Std_Font_" & Standard_Font_Name (f);
    end Standard_Font_Dictionary_Name;

    --  7.8.3 Resource Dictionaries (any resources required by a page).
    --  Table 33: Font: A dictionary that maps resource names to font dictionaries.
    --
    procedure Font_Dictionary (pdf : in out PDF_Out_File) is
    begin
      WL (pdf, +"  /Font <<");  --  font dictionary
      for f in Standard_Font_Type loop
        WL (pdf,
          +"    " & Standard_Font_Dictionary_Name (f) &
          " << /Type /Font /Subtype /Type1 /BaseFont /" & Standard_Font_Name (f) &
          --  7.9.2.2 Text String Type: "PDFDocEncoding can encode all of
          --  the ISO Latin 1 character set and is documented in Annex D."
          --  PDFDocEncoding is recognized by the Chrome PDF viewer on Windows but...
          --  *isn't* by Adobe Reader X, on Windows! So we resort to another ISO
          --  Latin 1 superset: WinAnsiEncoding = Windows Code Page 1252 (Table D.1).
          " /Encoding /WinAnsiEncoding " &
          " >>"
        );
      end loop;
      WL (pdf, +"    >>");
    end Font_Dictionary;

    function Current_Font_Name (pdf : PDF_Out_File) return VString is
    begin
      if pdf.current_font in Standard_Font_Type then
        return Standard_Font_Name (pdf.current_font);
      else
        return pdf.ext_font_name;
      end if;
    end Current_Font_Name;

    function Current_Font_Dictionary_Name (pdf : PDF_Out_File) return VString is
    begin
      if pdf.current_font in Standard_Font_Type then
        return Standard_Font_Dictionary_Name (pdf.current_font);
      else
        return Font_Dictionary_Name (pdf.ext_font_name);
      end if;
    end Current_Font_Dictionary_Name;

  end Fonts;

end HAC_PDF_Out;
