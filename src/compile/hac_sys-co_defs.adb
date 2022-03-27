package body HAC_Sys.Co_Defs is

  function Construct_Root (Typ : Typen) return Exact_Typ is
  begin
    return
      (TYP      => Typ,
       Ref      => 0,
       Is_Range => False);
  end Construct_Root;

  function Undefined return Exact_Typ is
  begin
    return Construct_Root (NOTYP);
  end Undefined;

  overriding function Construct_Root (Typ : Typen) return Exact_Subtyp is
    es : Exact_Subtyp;
  begin
    Exact_Typ (es) := Construct_Root (Typ);
    es.Discrete_First := 0;
    es.Discrete_Last  := 0;
    return es;
  end Construct_Root;

  overriding function Undefined return Exact_Subtyp is
  begin
    return Construct_Root (NOTYP);
  end Undefined;

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

end HAC_Sys.Co_Defs;
