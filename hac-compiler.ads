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

with Ada.Streams;
with Ada.Text_IO;

package HAC.Compiler is

  use HAC.Data, HAC.PCode;

  type Exact_Typ is record  --  NB: was called "Item" in SmallAda.
    TYP  : Typen;
    Ref  : Index;
    --  If TYP is not a standard type, then (TYP, Ref) does identify the type.
    --  E.g. it can be (Enums, [index of the enumerated type definition]).
    --  If TYP = Records, Ref is an index into the Block_Table;
    --  if TYP = Arrays,  Ref is an index into the Arrays_Table , or
    --  if TYP = Enums,   Ref is an index into the Id table (the type's name).
  end record;

  -------------------------------------------------------------------------
  ------------------------------------------------------------ATabEntry----
  -------------------------------------------------------------------------
  --  Array-Table Entry : Array table entry represents an array.  Each entry
  --  contains the following fields (fields marked with a C are used only by
  --  the compiler and ignored by the interpreter):
  --
  type ATabEntry is record
    Index_xTyp   : Exact_Typ;  --  C  Type of the index
    Element_Size : Index;      --  Size of an element
    Element_xTyp : Exact_Typ;  --  C  Type of the elements of the array.
                               --        Element_xTYP.Ref is an index to an entry
                               --        in Arrays_Table if the elements of the array
                               --        are themselves arrays
    Array_Size   : Index;      --  C  Total size of the array
    Low, High    : Index;      --  Limits on the array index: array (Low .. High) of Element_TYP
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
    Id                : Alfa;         --   Name of the block
    Last_Id_Idx       : Index;        -- C pointer to the last identifier in this block
    Last_Param_Id_Idx : Index;        -- C pointer to the last parameter in this block
    PSize             : Index;        --   sum of the lengths of areas (1) & (2) above
    VSize             : Index := 0;   --   sum of PSize and length of area (3)
                                      --   (i.e. size of the activation record for
                                      --    this block if it is a subprogram)
    SrcFrom           : Positive;     --   Source code line count.  Source starts here
    SrcTo             : Positive;     --   and goes until here    (* Manuel *)
  end record;

  type aObject is
  (
      --  Declared number: untyped constant, like
      --  "pi : constant := 3.1415927"; (RM 3.3.2).
      Declared_Number_or_Enum_Item,
      --
      Variable,
      TypeMark,
      --
      Prozedure,
      Funktion,
      --
      aTask,
      aEntry,
      --
      Label
  );

  subtype Nesting_level is Integer range 0 .. Nesting_Level_Max;

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
    xTyp           : Exact_Typ;     --  Type identification
    Block_Ref      : Index;         --  Was: Ref (was used also for what is now xTyp.Ref,
                                    --       which caused a mixup for functions return types!)
    Normal         : Boolean;       --  value param?
    LEV            : Nesting_level;
    Adr_or_Sz      : Integer;
  end record;

  --  Obj                           Meaning of Adr_or_Sz
  --  -------------------------------------------------------------------------------
  --  Declared_Number_or_Enum_Item  Value (number), position (enumerated type)
  --  Variable                      Relative position in the stack.
  --  TypeMark                      Size (in PCode stack items) of an object
  --                                    of the declared type.
  --  Prozedure                     Index into the Object Code table,
  --                                    or Level 0 Standard Procedure code
  --  Funktion                      Index into the Object Code table,
  --                                    or Level 0 Standard Function code (SF_Code)
  --  aTask                         ?
  --  aEntry                        ?
  --  Label                         ?

  ------------------------------------------------------------------------
  ------------------------------------------------------------FilDescr----
  ------------------------------------------------------------------------

  subtype File_count is Integer range 0 .. FMax;
  subtype File_index is Integer range 1 .. FMax;
  type FilDescr_Texts is array (File_index) of Ada.Text_IO.File_Type;
  type FilDescr_Lname is array (File_index) of Natural;
  type FilDescr_Names is array (File_index) of Alfa;

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

  type    Arrays_Table_Type            is array (1 .. AMax)                  of ATabEntry;
  type    Blocks_Table_Type            is array (0 .. BMax)                  of BTabEntry;
  type    Display_Type                 is array (Nesting_level)              of Integer;
  type    Entries_Table_Type           is array (0 .. EntryMax)              of Index;
  type    Float_Constants_Table_Type   is array (1 .. Float_Const_Table_Max) of HAC_Float;
  type    Identifier_Table_Type        is array (0 .. Id_Table_Max)          of IdTabEntry;
  subtype Strings_Constants_Table_Type is String (1 .. SMax);
  type    Tasks_Definitions_Table_Type is array (0 .. TaskMax)               of Index;

  --  Display: keeps track of addressing by nesting level. See Ben-Ari Appendix A.

  type Source_Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

  ---------------------
  --  Compiler_Data  --
  ---------------------

  --  !! TBD: make Compiler_Data private.

  type Compiler_Data is record
    --  Source code information and scanner data
    compiler_stream  : Source_Stream_Access;
    source_file_name : VString;  --  Indicative (error messages)
    --
    Line_Count       : Natural;            --  Source line counter, used for listing
    InpLine          : Source_Line_String;
    CH               : Character;          --  Previous Character read from source program
    CC               : Integer;            --  Character counter (=column in current line)
    LL               : Natural;            --  Length of current line
    Sy               : KeyWSymbol;         --  Last KeyWSymbol read by InSymbol
    syStart, syEnd   : Integer;            --  Start and end on line for the symbol in Sy
    Id               : Alfa;               --  Identifier from InSymbol
    Id_with_case     : Alfa;               --  Same as Id, but with casing.
    INum             : HAC_Integer;        --  Integer from InSymbol
    RNum             : HAC_Float;          --  FLOAT Number from InSymbol
    SLeng            : Integer;            --  String Length
    --  Compiler tables
    Arrays_Table            : Arrays_Table_Type;  --  NB: only static-sized arrays so far.
    Blocks_Table            : Blocks_Table_Type;
    Display                 : Display_Type;
    Entries_Table           : Entries_Table_Type;
    File_IO_Table           : FilDescr;
    Float_Constants_Table   : Float_Constants_Table_Type;    --  Used by interpreter at run-time
    IdTab                   : Identifier_Table_Type;
    Strings_Constants_Table : Strings_Constants_Table_Type;  --  Used by interpreter at run-time
    Tasks_Definitions_Table : Tasks_Definitions_Table_Type;
    --  Indices to compiler tables
    Arrays_Count            : Natural;
    Blocks_Count            : Natural;
    Entries_Count           : Natural;
    Float_Constants_Count   : Natural;
    Id_Count                : Natural;
    Strings_Table_Top       : Natural;
    Tasks_Definitions_Count : Natural;
    --  Object code
    ObjCode                 : Fixed_Size_Object_Code_Table;
    LC                      : Integer;  --  Location counter in the Object_Code_Table
    CMax                    : Integer;  --  Top of available ObjCode table;
                                        --  CMax + 1 .. CDMax: variable initialization code
    --  Information about source code
    Block_Id_with_casing      : Alfa;                --  Copy of current block's Id
    Main_Program_ID           : Alfa := Empty_Alfa;  --  Main program name
    Main_Program_ID_with_case : Alfa := Empty_Alfa;
    --
    listing_requested   : Boolean;
    comp_dump_requested : Boolean;
    listing   : Ada.Text_IO.File_Type;
    comp_dump : Ada.Text_IO.File_Type;
    --
    Err_Count  : Natural;
    Errs       : Error_set;
    error_pipe : Smart_error_pipe := null;
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

  --  Set current source stream (file, editor data, zipped file,...)
  procedure Set_Source_Stream (
    CD        : in out Compiler_Data;
    s         : access Ada.Streams.Root_Stream_Type'Class;
    file_name :        String  --  Can be a virtual name (editor title, zip entry)
  );

  function Get_Current_Source_Name (CD: Compiler_Data) return String;

  procedure Set_Error_Pipe (
    CD   : in out Compiler_Data;
    pipe :        Smart_error_pipe
  );

  function Unit_Compilation_Successful (CD: Compiler_Data) return Boolean;

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        Opcode
  );

  procedure Emit1 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    B    :        Operand_2_Type
  );

  procedure Emit2 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  );

  procedure Emit_Std_Funct (
    CD   : in out Compiler_Data;
    Code :        SF_Code
  );

  procedure Emit_Comparison_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        Comparison_Operator;
    Base_Typ  :        Typen
  );

  procedure Emit_Unary_Minus (
    CD        : in out HAC.Compiler.Compiler_Data;
    Base_Typ  :        Numeric_Typ
  );

  procedure Emit_Arithmetic_Binary_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        Arithmetic_Binary_Operator;
    Base_Typ  :        Numeric_Typ
  );

end HAC.Compiler;
