package body HAC_Sys.Co_Defs is

  procedure Construct_Root (Root : out Exact_Typ; Typ : Typen) is
  begin
    Root.TYP      := Typ;
    Root.Ref      := 0;
    Root.Is_Range := False;
  end Construct_Root;

  function Construct_Root (Typ : Typen) return Exact_Typ is
    result : Exact_Typ;
  begin
    Construct_Root (result, Typ);
    return result;
  end Construct_Root;

  function Undefined return Exact_Typ is
  begin
    return Construct_Root (NOTYP);
  end Undefined;

  overriding procedure Construct_Root (Root : out Exact_Subtyp; Typ : Typen) is
  begin
    Construct_Root (Exact_Typ (Root), Typ);  --  Call parent method.
    Root.Discrete_First := 0;
    Root.Discrete_Last  := 0;
  end Construct_Root;

  overriding function Construct_Root (Typ : Typen) return Exact_Subtyp is
    result : Exact_Subtyp;
  begin
    Construct_Root (result, Typ);
    return result;
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
