-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--
--  Co_Defs: Compiler Definitions

with HAC_Sys.Defs,
     HAC_Sys.PCode;

with HAT;

with Ada.Containers.Hashed_Maps,
     Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Streams,
     Ada.Strings.Hash,
     Ada.Strings.Unbounded.Hash,
     Ada.Text_IO;

with System;

package HAC_Sys.Co_Defs is
  --  NB: cannot be a child package of Compiler because of Parser, Scanner, ...

  use HAC_Sys.Defs;

  type Exact_Typ is tagged record  --  NB: was called "Item" in SmallAda.
    TYP  : Typen;
    Ref  : Index;
    --  If TYP is not a standard type, then (TYP, Ref) does identify the base type.
    --  E.g. it can be (Enums, [index of the enumerated type definition]).
    --
    --  Ref is an index into different tables, depending on TYP:
    --    If TYP = Records,      Block_Table;
    --    if TYP = Arrays,       Arrays_Table;
    --    if TYP = Enums,        Idtab (the enumeration type's declaration).
    --
    Is_Range : Boolean;
    --  ^ For X'Range expressions, indicates a pair of values waiting on the stack.
  end record;

  procedure Construct_Root (Root : out Exact_Typ; Typ : Typen);
  pragma Inline (Construct_Root);

  function Construct_Root (Typ : Typen) return Exact_Typ;

  function Undefined return Exact_Typ;

  type Exact_Subtyp is new Exact_Typ with record
    Discrete_First : HAC_Integer;   --  If subtype S is discrete, S'First
    Discrete_Last  : HAC_Integer;   --  If subtype S is discrete, S'Last
  end record;

  overriding procedure Construct_Root (Root : out Exact_Subtyp; Typ : Typen);
  pragma Inline (Construct_Root);

  overriding function Construct_Root (Typ : Typen) return Exact_Subtyp;

  overriding function Undefined return Exact_Subtyp;

  -------------------------------------------------------------------------
  ------------------------------------------------------------ATabEntry----
  -------------------------------------------------------------------------
  --  Array-Table Entry : Array table entry represents an array.  Each entry
  --  contains the following fields (fields marked with a C are used only by
  --  the compiler and ignored by the interpreter):
  --
  type ATabEntry is record
    Index_xTyp   : Exact_Subtyp;  --  C  Type of the index
    Element_Size : Index;         --  Size of an element
    Element_xTyp : Exact_Subtyp;  --  C  Subtype of the elements of the array.
                                  --     If the elements of the array are themselves
                                  --     arrays, Element_xTYP.Ref is an index to an
                                  --     entry in Arrays_Table (it's not a special case).
    Array_Size   : Index;         --  C  Total size of the array
  end record;

  -------------------------------------------------------------------------
  ------------------------------------------------------------BTabEntry----
  -------------------------------------------------------------------------
  --  Block-table Entry : Each entry represents a subprogram or a record type.
  --
  --  A subprogram activation record consists of:
  --
  --         (1) the five word fixed area; (see definition of S in Interpreter)
  --         (2) an area for the actual parameters (whether values or
  --             addresses), and
  --         (3) an area for the local variables of the subprogram
  --
  --  Once again, fields marked with C are used only by the compiler
  --
  type BTabEntry is record
    Id                 : Alfa;         --   Name of the block
    Last_Id_Idx        : Index;        -- C index of the last identifier in this block
    First_Param_Id_Idx : Index;        -- C index of the first parameter in this block
    Last_Param_Id_Idx  : Index;        -- C index of the last parameter in this block
                                       --     (if first > last, it's parameterless)
    PSize              : Index;        --   sum of the lengths of areas (1) & (2) above
    VSize              : Index := 0;   --   sum of PSize and length of area (3)
                                       --   (i.e. size of the activation record for
                                       --    this block if it is a subprogram)
    SrcFrom            : Positive;     --   Source code line count.  Source starts here
    SrcTo              : Positive;     --   and goes until here    (* Manuel *)
  end record;

  fixed_area_size : constant := 5;  --  Size of area (1) described above.

  type Package_Table_Entry is record
    first_public_declaration  : Positive;
    last_public_declaration   : Natural;   -- = 0 if none.
    last_private_declaration  : Natural;   -- = 0 if none.
  end record;

  type Entity_Kind is  --  RM 3.1
  (
      --  Declared number: untyped constant, like
      --  "pi : constant := 3.1415927"; (RM 3.3.2).
      Declared_Number_or_Enum_Item,
      --
      Variable,
      TypeMark,
      --
      Paquetage,
      Paquetage_Body,
      Prozedure,
      Prozedure_Intrinsic,
      Funktion,
      Funktion_Intrinsic,
      --
      aTask,
      aEntry,
      --
      Label,
      Alias   --  Short name of another entity ("use" clause).
  );

  type Declaration_Kind is
    (spec_unresolved, spec_resolved, complete,
     param_in, param_in_out, param_out);

  subtype Split_Declaration_Kind is Declaration_Kind range spec_unresolved .. complete;

  subtype Parameter_Kind is Declaration_Kind range param_in .. param_out;

  ------------------------------
  --  Identifier Table Entry  --
  ------------------------------
  type IdTabEntry is record
    name             : Alfa;                 --  identifier name in ALL CAPS
    name_with_case   : Alfa;                 --  identifier name with original casing
    link             : Index;
    entity           : Entity_Kind;
    read_only        : Boolean;              --  If Entity = Variable and read_only = True,
                                             --    it's a typed constant.
    decl_kind        : Declaration_Kind;     --  Declaration kind: forward or complete.
    --                                             Matters for a type, a constant, a subprogram;
    --                                             Values param_in .. param_out are
    --                                             for subprogram parameters.
    xtyp             : Exact_Subtyp;         --  Subtype identification
    block_or_pkg_ref : Index;                --  Reference in the block or package tables.
    normal           : Boolean;              --  value param?
    lev              : Nesting_level;
    adr_or_sz        : Integer;
  end record;

  --  Entity                        Meaning of Adr_or_Sz
  --  -------------------------------------------------------------------------------
  --  Declared_Number_or_Enum_Item  Value (number), position (enumerated type)
  --  Variable                      Relative position in the stack.
  --  TypeMark                      Size (in PCode stack items) of an object
  --                                    of the declared type.
  --  Prozedure                     Index into the Object Code table.
  --  Prozedure_Intrinsic           Standard Procedure code (SP_Code).
  --  Funktion                      Index into the Object Code table.
  --  Funktion_Intrinsic            Standard Function code (SF_Code).
  --  aTask                         ?
  --  aEntry                        ?
  --  Label                         ?
  --  Alias                         Index into the Identifier table of the aliased entity.

  subtype Source_Line_String is String (1 .. 1000);  --  Must be at least 200 (RM 2.2 (15))

  -----------------------
  --  Compiler tables  --
  -----------------------

  type    Arrays_Table_Type            is array (1 .. AMax)         of ATabEntry;
  type    Blocks_Table_Type            is array (0 .. BMax)         of BTabEntry;
  type    Display_Type                 is array (Nesting_level)     of Integer;
  type    Entries_Table_Type           is array (0 .. EntryMax)     of Index;
  type    Identifier_Table_Type        is array (0 .. Id_Table_Max) of IdTabEntry;
  type    Packages_Table_Type          is array (0 .. PMax)         of Package_Table_Entry;
  subtype Strings_Constants_Table_Type is String (1 .. SMax);
  type    Tasks_Definitions_Table_Type is array (0 .. TaskMax)      of Index;
  --      ^ Task #0 is main task.

  --  Display: keeps track of addressing by nesting level. See Ben-Ari Appendix A.

  No_Id : constant := 0;

  type Source_Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

  package Id_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => Alfa,
     Element_Type    => Positive,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => HAT."=");

  type Current_Unit_Data is record
    --  Current source code information and scanner data
    compiler_stream  : Source_Stream_Access;
    source_file_name : HAT.VString;         --  Indicative, for error messages
    --  Parsing
    line_count       : Natural;             --  Source line counter, used for listing
    input_line       : Source_Line_String;
    c                : Character;           --  Character read from source program
    CC               : Integer;             --  Character counter (=column in current line)
    LL               : Natural;             --  Length of current line
    --  Level 0 definitions visible to currently compiled unit:
    level_0_def      : Id_Maps.Map;
  end record;

  --  Set current source stream (file, editor data, zipped file,...)
  procedure Set_Source_Stream (
    CUD        : in out Current_Unit_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;       --  Can be a virtual name (editor title, zip entry)
    start_line : in     Natural := 0  --  We could have a shebang or other Ada sources before
  );

  function Get_Source_Name (SD : Current_Unit_Data) return String;

  type Compilation_Feedback is access procedure (message : String);

  type Compilation_Trace_Parameters is record
    pipe         : Defs.Smart_Error_Pipe := null;  --  Default: messages to Current_Error.
    progress     : Compilation_Feedback  := null;  --  Default: messages to Current_Output.
    detail_level : Natural               := 0;
  end record;

  default_trace : constant Compilation_Trace_Parameters := (others => <>);

  package Exported_Procedure_Mapping is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => String,
     Element_Type    => System.Address,
                        --  Actually: HAC_Sys.Interfacing.Exported_Procedure, but we
                        --  end up in a circular unit dependency mess.
     Hash            => Ada.Strings.Hash,
     Equivalent_Keys => "=",
     "="             => System."=");

  ---------------------
  --  Compiler_Data  --
  ---------------------

  type Compiler_Data is record
    CUD : Current_Unit_Data;
    --  Scanning & Parsing
    Sy, prev_sy      : KeyWSymbol;         --  Last KeyWSymbol read by InSymbol
    syStart, syEnd   : Integer;            --  Start and end on line for the symbol in Sy
    prev_sy_start,
    prev_sy_end,
    prev_sy_line     : Integer;
    Id               : Alfa;               --  Identifier from InSymbol
    Id_with_case     : Alfa;               --  Same as Id, but with casing.
    INum             : HAC_Integer;        --  Integer from InSymbol
    RNum             : HAC_Float;          --  FLOAT Number from InSymbol
    SLeng            : Integer;            --  String Length
    pkg_prefix       : HAT.VString;        --  Prefix of package being currently parsed.
    --  Compiler tables. Floats and Strings are used by interpreter at run-time.
    Arrays_Table            : Arrays_Table_Type;  --  NB: only static-sized arrays so far.
    Blocks_Table            : Blocks_Table_Type;
    Display                 : Display_Type;
    Entries_Table           : Entries_Table_Type;
    Float_Constants_Table   : Float_Constants_Table_Type;
    IdTab                   : Identifier_Table_Type;
    Packages_Table          : Packages_Table_Type;
    Strings_Constants_Table : Strings_Constants_Table_Type;
    Tasks_Definitions_Table : Tasks_Definitions_Table_Type;
    --  Indices to compiler tables
    Arrays_Count            : Natural;
    Blocks_Count            : Natural;
    Entries_Count           : Natural;
    Float_Constants_Count   : Natural;
    Id_Count                : Natural;
    Main_Proc_Id_Index      : Natural;
    Packages_Count          : Natural;
    String_Id_Index         : Natural;
    Strings_Table_Top       : Natural;
    Tasks_Definitions_Count : Natural;
    --  Object code
    ObjCode                 : PCode.Object_Code_Table (0 .. CDMax);
    LC                      : Integer;  --  Location counter in the Object_Code_Table
    CMax                    : Integer;  --  Top of available ObjCode table;
                                        --  CMax + 1 .. CDMax: variable initialization code
    folded_instructions      : Natural;
    specialized_instructions : Natural;
    --  Information about source code
    Full_Block_Id             : HAT.VString;         --  Full block's Id (P1.P2.F3.P4)
    Main_Program_ID           : Alfa := Empty_Alfa;  --  Main program name
    Main_Program_ID_with_case : Alfa := Empty_Alfa;
    Exported_Procedures       : Exported_Procedure_Mapping.Map;
    --
    listing_requested   : Boolean := False;
    comp_dump_requested : Boolean := False;
    listing   : Ada.Text_IO.File_Type;
    comp_dump : Ada.Text_IO.File_Type;
    --
    error_count, minor_error_count : Natural;
    errs         : Error_set;
    total_lines  : Natural;
    trace        : Compilation_Trace_Parameters;
    --  On `WITH X`, we start the recursive compilation of X,
    --  if X is not yet compiled or built-in. We monitor the
    --  recursion level for the fun of it.
    recursion    : Natural := 0;
  end record;

  --  Image function for compilation errors or out-of-range exception messages.
  --
  function Discrete_Image
    (CD : Compiler_Data; value : HAC_Integer; Typ : Typen; Ref : Index) return String;

  function Discrete_Range_Image
    (CD : Compiler_Data; value_1, value_2 : HAC_Integer; Typ : Typen; Ref : Index) return String;

  --  Size of a variable or subprogram parameter
  --
  function Size_of (CD : Compiler_Data; Id_Index : Natural) return Positive;

  Universe : constant HAT.VString := HAT.To_VString ("[-- The Universe --]");

  type CASE_Label_Value is record
    value_1, value_2 : HAC_Integer;  --  value of a choice in a CASE statement
    LC               : Index;        --  instruction address
    Is_others        : Boolean;
  end record;

  type Constant_Rec is record
    TP : Exact_Subtyp;
    I  : HAC_Integer;  --  Includes Character and enumeration types (including Boolean)
    R  : HAC_Float;
  end record;

end HAC_Sys.Co_Defs;
