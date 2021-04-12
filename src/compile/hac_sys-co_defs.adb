with Ada.Unchecked_Deallocation;

package body HAC_Sys.Co_Defs is

  procedure Set_Source_Stream (
    SD         : in out Current_Unit_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;
    start_line : in     Natural := 0
  )
  is
  begin
    SD.compiler_stream  := Source_Stream_Access (s);
    SD.source_file_name := HAL.To_VString (file_name);
    SD.line_count       := start_line;
  end Set_Source_Stream;

  function Get_Source_Name (SD : Current_Unit_Data) return String is
  begin
    return HAL.VStr_Pkg.To_String (SD.source_file_name);
  end Get_Source_Name;

  overriding procedure Finalize (CD : in out Compiler_Data) is
    procedure Free is
      new Ada.Unchecked_Deallocation (HAC_Sys.PCode.Object_Code_Table, Object_Code_Table_Access);
  begin
    Free (CD.ObjCode);
  end Finalize;

end HAC_Sys.Co_Defs;
