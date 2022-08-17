-------------------------------------------------------------------------------------
--
--  HAC_PDF_OUT - A low level package for writing Adobe Acrobat PDF (*) files.
--
--  HAC_PDF_OUT is a stripped-down version of PDF_OUT ( http://apdf.sf.net/ )
--  for successful compilation with HAC (HAC Ada Compiler) v.0.1.
--
--  Elements of PDF_OUT removed in HAC_PDF_OUT are:
--
--    - Object-oriented type extension (tagged type for PDF stream).
--        PDF Files are the only supported PDF streams.
--    - Object.method notation
--    - Default values in records, replaced by explicit initialization.
--    - Inclusion of raster images.
--    - User-defined exceptions (PDF_stream_not_created, ...).
--    - User-defined operators ("+", ...).
--    - Indefinite page table and offset table.
--
-------------------------------------------------------------------------------------

--  Legal licensing note:

--  Copyright (c) 2014 .. 2022 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  (*) All Trademarks mentioned are properties of their respective owners.
-------------------------------------------------------------------------------------
--
--  Follow these steps to create a PDF document stream:
--
--  1. Create
--
--  2. | Put(pdf, data),
--     | New_Line(pdf), ... : other "Text_IO"-like (full list below)
--     | Image(pdf, ...)    : raster images [disabled for the HAC version]
--     | Move/Line/...      : vector graphics
--     | New_Page(pdf)
--
--  3. Close
--
--  4. (PDF_Out_String only) function Contents returns the full .pdf
--
--  Header and footer are set up by overriding the corresponding methods.
--
--  Note: the standard PDF measurement unit is a "point", set as 1/72 inch.
--
--  All technical references are to PDF 1.7 format, ISO 32000-1:2008 standard
--  http://www.adobe.com/devnet/pdf/pdf_reference.html
--
--------------------------------------------------------------------------

with HAT;
with Interfaces;

package HAC_PDF_Out is

  type PDF_type is (
    PDF_1_3 -- PDF 1.3
  );

  subtype Real is HAT.Real;

  type Point is record
    x, y : Real;
  end record;

  --  procedure Add (P1, P2 : Point; result : out Point);
  --
  --  procedure Scale_Point (f : Real; P : Point; result : out Point);

  type Rectangle is record
    x_min, y_min,
    width, height : Real;
  end record;

  --  procedure Translate (P : Point; r : Rectangle; result : out Rectangle);
  --
  --  --  Scaling. r.x_min and r.y_min are preserved.
  --  procedure Scale_Rectangle (f : Real; r : Rectangle; result : out Rectangle);

  function X_Max (r : Rectangle) return Real;
  function Y_Max (r : Rectangle) return Real;

  subtype PDF_Index_Type is Integer;

  subtype Offset_Index_Type is PDF_Index_Type range 1 .. 1000;

  type Offset_table is array (Offset_Index_Type) of Natural;

  subtype Page_Index_Type is PDF_Index_Type range 1 .. 1000;

  type Page_table is array (Page_Index_Type) of PDF_Index_Type; -- object ID's of pages

  type Page_zone is (nowhere, in_page, in_header, in_footer);
  type Text_or_graphics is (text, graphics);

  type Margins_Type is record
    left, right, top, bottom : Real;
  end record;

  type Font_Type is
     ( --  The 14 standard fonts
      Courier,
      Courier_Bold,
      Courier_Bold_Oblique,
      Courier_Oblique,
      Helvetica,
      Helvetica_Bold,
      Helvetica_Bold_Oblique,
      Helvetica_Oblique,
      Symbol,
      Times_Bold,
      Times_Bold_Italic,
      Times_Italic,
      Times_Roman,
      Zapf_Dingbats,
      --  Fonts imported into the PDF document
      External_Font
     );

  subtype Standard_Font_Type is Font_Type range Courier .. Zapf_Dingbats;

  --  !! Should be private (in a future version of HAC)

  type PDF_Out_File is record
    pdf_file       : HAT.File_Type;
    file_index     : Natural;
    file_name      : HAT.VString;
    start_index    : Natural;
    is_created     : Boolean;
    is_closed      : Boolean;
    format         : PDF_type;
    zone           : Page_zone;
    text_switch    : Text_or_graphics;
    last_page      : PDF_Index_Type;
    current_line   : Positive;
    current_col    : Positive;
    page_idx       : Page_table;
    page_box       : Rectangle;
    maximum_box    : Rectangle;
    page_margins   : Margins_Type;
    objects        : PDF_Index_Type;
    object_offset  : Offset_table;
    stream_obj_buf : HAT.VString;
    current_font   : Font_Type;
    font_size      : Real;
    line_spacing   : Real;
    ext_font_name  : HAT.VString;
    doc_title      : HAT.VString;  --  Document information (14.3.3)
    doc_author     : HAT.VString;  --  Document information (14.3.3)
    doc_subject    : HAT.VString;  --  Document information (14.3.3)
    doc_keywords   : HAT.VString;  --  Document information (14.3.3)
    doc_creator    : HAT.VString;  --  Document information (14.3.3) : creator application
  end record;

  procedure Init (pdf : in out PDF_Out_File);

  ----------------------------
  -- (2) Document contents: --
  ----------------------------

  procedure Put_Real (pdf  : in out PDF_Out_File;
                      num  : in Real
            );
  procedure Put_Int (pdf   : in out PDF_Out_File;
                     num   : in Integer
            );
  procedure Put_Str (pdf : in out PDF_Out_File; str : HAT.VString);
  --
  procedure Put_Line (pdf : in out PDF_Out_File; str : HAT.VString);
  --
  procedure New_Line (pdf : in out PDF_Out_File);
  procedure New_Page (pdf : in out PDF_Out_File);
  --  Call to Finish_Page is optional, but can be necessary in some circumstances,
  --  for instance for displaying the footer correctly before changing page
  --  orientation or margins for the following pages.
  procedure Finish_Page (pdf : in out PDF_Out_File);
  --
  procedure Text_XY (pdf : in out PDF_Out_File; x, y : Real);
  procedure Put_XY (pdf : in out PDF_Out_File; x, y : Real; str : HAT.VString);

  function Col (pdf : in PDF_Out_File) return Positive;
  function Line (pdf : in PDF_Out_File) return Positive;
  function Page (pdf : in PDF_Out_File) return Natural;

  --  Select one of the Adobe PDF standard fonts.
  --  The encoding is on 8 bits and follows the "Windows Code Page 1252"
  --  encoding (called WinAnsiEncoding in the PDF standard).
  --  See Annex D, especially "Table D.1 - Latin-text encodings" for details.
  procedure Font (pdf : in out PDF_Out_File; f : Standard_Font_Type);

  --  Set the font size.
  --  In general the size is a scale factor (see Table 105, Tf operator).
  --  For standard fonts the unit seems to be the Point (pt).
  procedure Font_Size (pdf : in out PDF_Out_File; size : Real);

  procedure Line_Spacing (pdf : in out PDF_Out_File; factor : Real);  --  as multiple of font size
  procedure Line_Spacing_Pt (pdf : in out PDF_Out_File; pt : Real);   --  in Point (pt) units

  --------------
  --  Colors  --
  --------------

  --  0.0 = minimum intensity
  --  1.0 = maximum intensity.
  subtype Color_Value is Real;  --  range 0.0 .. 1.0;

  type Color_Type is record
    red, green, blue : Color_Value;
  end record;

  procedure Color (pdf : in out PDF_Out_File; c : Color_Type);
  procedure Stroking_Color (pdf : in out PDF_Out_File; c : Color_Type);

  type Rendering_Mode is (
    fill, stroke, fill_then_stroke, invisible,
    --  Same, but also add text to path for clipping.
    fill_and_add_to_path,
    stroke_and_add_to_path,
    fill_then_stroke_and_add_to_path,
    add_to_path
  );

  procedure Text_Rendering_Mode (pdf : in out PDF_Out_File; r : Rendering_Mode);

  -----------------------
  --  Vector graphics  --
  -----------------------

  procedure Line_Width (pdf : in out PDF_Out_File; width : Real);

  --  Draw a single line segment:
  procedure Single_Line (pdf : in out PDF_Out_File; from, to : Point);

  subtype Path_Rendering_Mode is Rendering_Mode range fill .. fill_then_stroke;

  --  Draw simple figures.
  --  Rectangle:
  procedure Draw (pdf : in out PDF_Out_File; what : Rectangle; rendering : Path_Rendering_Mode);

  --  Paths:

  type Inside_path_rule is (nonzero_winding_number, even_odd);
  --  Rule to determine how to fill areas within a (non-trivial) path.
  --  See 8.5.3.3.2 and 8.5.3.3.3 of PDF specification

  procedure Move    (pdf : in out PDF_Out_File; to : Point);
  procedure Line_To (pdf : in out PDF_Out_File; to : Point);
  procedure Cubic_Bezier (pdf : in out PDF_Out_File; control_1, control_2 : Point; to : Point);
  --  All lines and curves and the eventual filling inside the path
  --  will be drawn when path is completed, with Finish_Path:

  procedure Finish_Path (
    pdf        : in out PDF_Out_File;
    close_path :        Boolean;
    rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
    rule       :        Inside_path_rule
  );

  ------------
  --  Misc  --
  ------------

  --  In the likely case some PDF feature is not yet implemented in
  --  this package, you can insert direct PDF code - at your own risk ;-).
  --
  --  NB: the state the PDF machine is either in text-writing
  --  mode, or graphics mode. To make outputs compliant with the PDF
  --  standard, if you want to insert graphics code, please
  --  use the Insert_Graphics_PDF_Code below. For text-related stuff,
  --  use Insert_Text_PDF_Code.
  --
  procedure Insert_PDF_Code (pdf : in out PDF_Out_File; code : HAT.VString);

  --  This is for direct text PDF code insertion (text-writing mode
  --  will be switched on). In PDF language these are the T... commands.
  --
  procedure Insert_Text_PDF_Code (pdf : in out PDF_Out_File; code : HAT.VString);

  --  This is for direct graphics PDF code insertion (text-writing mode
  --  will be switched off for the graphics output).
  --
  procedure Insert_Graphics_PDF_Code (pdf : in out PDF_Out_File; code : HAT.VString);

  --  Document information
  procedure Title (pdf : in out PDF_Out_File; s : HAT.VString);
  procedure Author (pdf : in out PDF_Out_File; s : HAT.VString);
  procedure Subject (pdf : in out PDF_Out_File; s : HAT.VString);
  procedure Keywords (pdf : in out PDF_Out_File; s : HAT.VString);
  procedure Creator_Application (pdf : in out PDF_Out_File; s : HAT.VString);

  ------------------
  --  Page layout --
  ------------------

  --  You need to override the Header and Footer methods
  --  for setting up your custom header and footer. By default they do nothing.
  procedure Page_Header (pdf : in out PDF_Out_File);
  procedure Page_Footer (pdf : in out PDF_Out_File);

  --  They have to be called before New_Page in order to influence the next page.
  --  For the first page, call them before any output (typically right after Create).
  --
  procedure Set_Left_Margin (pdf : out PDF_Out_File; pts : Real);
  function Get_Left_Margin (pdf : PDF_Out_File) return Real;
  procedure Set_Right_Margin (pdf : out PDF_Out_File; pts : Real);
  function Get_Right_Margin (pdf : PDF_Out_File) return Real;
  procedure Set_Top_Margin (pdf : out PDF_Out_File; pts : Real);
  function Get_Top_Margin (pdf : PDF_Out_File) return Real;
  procedure Set_Bottom_Margin (pdf : out PDF_Out_File; pts : Real);
  function Get_Bottom_Margin (pdf : PDF_Out_File) return Real;

  --  Some distances in Points

  function one_cm   return Real;
  function cm_2_5   return Real;
  function one_inch return Real;

  procedure Set_Margins (pdf : out PDF_Out_File; new_margins : Margins_Type);
  procedure Get_Margins (pdf : PDF_Out_File; result : out Margins_Type);

  procedure Set_A4_portrait (r : out Rectangle);

  procedure Page_Setup (pdf : in out PDF_Out_File; layout : Rectangle);

  procedure Layout (pdf : PDF_Out_File; result : out Rectangle);

  procedure Create (
    pdf        : in out PDF_Out_File;
    file_name  :        HAT.VString
  );

  procedure Close (pdf : in out PDF_Out_File);

  function Is_Open (pdf : in PDF_Out_File) return Boolean;

private

  function Image_name (i : Positive) return HAT.VString;
  procedure New_object (pdf : in out PDF_Out_File);
  procedure WL (pdf : in out PDF_Out_File; s : HAT.VString);

end HAC_PDF_Out;
