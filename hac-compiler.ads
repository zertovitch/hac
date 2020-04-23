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

with HAC.Data, HAC.PCode;

with Ada.Text_IO;

package HAC.Compiler is

  use HAC.Data;

  type Exact_Type is record  --  NB: was called "Item" in SmallAda.
    TYP : Types;
    Ref : Index;
    --  If TYP is not a standard type, then (TYP, Ref) does identify the
    --  type. E.g. it can be (Enums, [index of the enumerated type definition]).
  end record;

  -------------------------------------------------------------------------
  ------------------------------------------------------------ATabEntry----
  -------------------------------------------------------------------------
  --  Array-Table Entry : Array table entry represents an array.  Each entry
  --  contains the following fields (fields marked with a C are used only by
  --  the compiler and ignored by the interpreter):
  --
  type ATabEntry is record
    Index_TYP    : Exact_Type;  --  C  Type of the index
    Element_TYP  : Types;       --  C  Type of the elements of the array
    ELREF        : Index;       --  C  Pointer to an entry in Arrays_Table if the
                                --     elements of the array are themselves arrays
    Size         : Index;       --  C  Total size of the array
    Low, High    : Index;       --  Limits on the array index: array (Low .. High) of Element_TYP
    ELSize       : Index;       --  Size of an element
  end record;

  -------------------------------------------------------------------------
  ------------------------------------------------------------BTabEntry----
  -------------------------------------------------------------------------
  --  Block-table Entry : Each entry represents a subprogram
  --  An activation record consists of:
  --
  --         (1) the five word fixed area; (see definition of S in Interpreter)
  --         (2) an area for the actual parameters (whether values or
  --             addresses), and
  --         (3) an area for the local variables of the subprogram
  --
  --  Once again, fields marked with C are used only by the compiler
  --
  type BTabEntry is record
    Id   : Alfa;            --   Name of the block
    Last : Index;           -- C pointer to the last identifier in this subprogram
    LastPar : Index;        -- C pointer to the last parameter in this subprogram
    PSize : Index;          --  sum of the lengths of areas (1) & (2) above
    VSize : Index := 0;     --  sum of PSize and length of area (3)
                            --  (i.e. size of the activation record for
                            --  this subprogram)
    SrcFrom : Positive;     --  Source code line count.  Source starts here
    SrcTo   : Positive;     --  and goes until here    (* Manuel *)
  end record;

  type aObject is
  (
      --  Declared number: untyped constant, like
      --  "pi : constant := 3.1415927"; (RM 3.3.2).
      Declared_Number_or_Enum_Item,
      --
      Variable,
      TypeMark,
      Prozedure,
      Funktion,
      aTask,
      aEntry,
      Label
  );

  subtype Nesting_level is Integer range 0 .. LMax;

  ------------------------------------------------------------------------
  ------------------------------------------------------------TabEntry----
  ------------------------------------------------------------------------
  --  Identifier Table Entry
  type IdTabEntry is record
    Name           : Alfa;          --  identifier name in ALL CAPS
    Name_with_case : Alfa;          --  identifier name with original casing
    Link           : Index;
    Obj            : aObject;       --  One of:
                                    --    Declared_Number, Variable, TypeMark,
                                    --    Prozedure, Funktion, aTask, aEntry
    Read_only      : Boolean;       --  If Obj = Variable and Read_only = True,
                                    --    it's a typed constant.
    TYP            : Types;         --  One of: NoTyp, Ints, Floats, Bools,
                                    --    xChars, Arrays, Records, Enums, Strings
    Ref            : Index;         --  Index into the Block table
    Normal         : Boolean;       --  value param?
    LEV            : Nesting_level;
    Adr            : Integer;       --  index into the Code table for the
                                    --  procedure code (if Name is a
                                    --  procedure or function)
  end record;

  ------------------------------------------------------------------------
  ------------------------------------------------------------FilDescr----
  ------------------------------------------------------------------------

  subtype File_count is Integer range 0 .. FMax;
  subtype File_index is Integer range 1 .. FMax;
  type FilDescr_Texts is array (File_index) of Ada.Text_IO.File_Type;
  type FilDescr_Lname is array (File_index) of Natural;
  type FilDescr_Names is array (File_index) of String (1 .. Alng);

  type FilDescr is record
    Curr  : Integer;
    Kount : File_count:= 0;
    Fil   : FilDescr_Texts;
    Nam   : FilDescr_Names;
    LNam  : FilDescr_Lname;
  end record;

  subtype Source_Line_String is String (1 .. 1000);

  -----------------------
  --  Compiler tables  --
  -----------------------

  subtype Fixed_Size_Object_Code_Table is HAC.PCode.Object_Code_Table (0 .. CDMax);

  type Arrays_Table_Type            is array (1 .. AMax)         of ATabEntry;
  type Blocks_Table_Type            is array (0 .. BMax)         of BTabEntry;
  type Display_Type                 is array (Nesting_level)     of Integer;
  type Entries_Table_Type           is array (0 .. EntryMax)     of Index;
  type Float_Constants_Table_Type   is array (1 .. C2Max)        of HAC_Float;
  type Identifier_Table_Type        is array (0 .. Id_Table_Max) of IdTabEntry;
  type Strings_Table_Type           is array (0 .. SMax)         of Character;
  type Tasks_Definitions_Table_Type is array (0 .. TaskMax)      of Index;

  --  Display: keeps track of addressing by nesting level. See Ben-Ari Appendix A.

  ---------------------
  --  Compiler_Data  --
  ---------------------

  --  !! TBD: make Compiler_Data private.

  type Compiler_Data is record
    --  Source code information and scanner data
    Line_Count              : Natural;            --  Source line counter, used for listing
    InpLine                 : Source_Line_String;
    CH                      : Character;          --  Previous Character read from source program
    CC                      : Integer;            --  Character counter (=column in current line)
    LL                      : Natural;            --  Length of current line
    Sy                      : KeyWSymbol;         --  Last KeyWSymbol read by InSymbol
    syStart, syEnd          : Integer;            --  Start and end on line for the symbol in Sy
    Id                      : Alfa;               --  Identifier from InSymbol
    Id_with_case            : Alfa;               --  Same as Id, but with casing.
    INum                    : Integer;            --  Integer from InSymbol
    RNum                    : HAC_Float;          --  FLOAT Number from InSymbol
    SLeng                   : Integer;            --  String Length
    --  Compiler tables
    Arrays_Table            : Arrays_Table_Type;  --  NB: only static-sized arrays so far.
    Blocks_Table            : Blocks_Table_Type;
    Display                 : Display_Type;
    Entries_Table           : Entries_Table_Type;
    File_IO_Table           : FilDescr;
    Float_Constants_Table   : Float_Constants_Table_Type;
    IdTab                   : Identifier_Table_Type;
    Strings_Table           : Strings_Table_Type;
    Tasks_Definitions_Table : Tasks_Definitions_Table_Type;
    --  Indices to compiler tables
    Arrays_Count            : Natural;
    Blocks_Count            : Natural;
    Entries_Count           : Natural;
    Float_Constants_Count   : Natural;
    RNum_Index              : Natural;
    Id_Count                : Natural;
    Strings_Table_Top       : Natural;
    Tasks_Definitions_Count : Natural;
    --  Object code
    ObjCode                 : Fixed_Size_Object_Code_Table;
    LC                      : Integer;  --  Location counter in the Object_Code_Table
    CMax                    : Integer;  --  Top of available ObjCode table;
                                        --  CMax + 1 .. CDMax: variable initialization code
    --  Information about source code
    Block_Id_with_casing      : HAC.Data.Alfa;                --  Copy of current block's Id
    Main_Program_ID           : HAC.Data.Alfa := Empty_Alfa;  --  Main program name
    Main_Program_ID_with_case : HAC.Data.Alfa := Empty_Alfa;
    --
    listing_requested   : Boolean;
    comp_dump_requested : Boolean;
    listing   : Ada.Text_IO.File_Type;
    comp_dump : Ada.Text_IO.File_Type;
    --
    Err_Count : Natural;
    Errs      : Error_set;
  end record;

  --  Main compilation procedure.
  --
  procedure Compile (
    CD                 : in out Compiler_Data;
    asm_dump_file_name :        String  := "";  --  Assembler oputput of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  );

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode);

  procedure Emit1 (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode;
    B    :        Integer);

  procedure Emit2 (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode;
    a, B :        Integer);

  procedure Emit_Comparison_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        HAC.Data.Comparison_Operator;
    Base_Type :        HAC.Data.Types
  );

  procedure Emit_Unary_Minus (
    CD        : in out HAC.Compiler.Compiler_Data;
    Base_Type :        HAC.Data.Numeric_Typ
  );

  procedure Emit_Arithmetic_Binary_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        HAC.Data.Arithmetic_Binary_Operator;
    Base_Type :        HAC.Data.Numeric_Typ
  );

end HAC.Compiler;
