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

with HAL;

with Ada.Finalization,
     Ada.Streams,
     Ada.Text_IO;

package HAC_Sys.Co_Defs is
  --  NB: cannot be a child package of Compiler because of Parser, Scanner, ...

  use HAC_Sys.Defs;

  type Exact_Typ is record  --  NB: was called "Item" in SmallAda.
    TYP  : Typen;
    Ref  : Index;
    --  If TYP is not a standard type, then (TYP, Ref) does identify the type.
    --  E.g. it can be (Enums, [index of the enumerated type definition]).
    --  If TYP = Records, Ref is an index into the Block_Table;
    --  if TYP = Arrays,  Ref is an index into the Arrays_Table , or
    --  if TYP = Enums,   Ref is an index into the Id table (the type's name).
  end record;

  Type_Undefined : constant Exact_Typ := (TYP => NOTYP, Ref => 0);

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

  ------------------------------------------------------------------------
  ------------------------------------------------------------TabEntry----
  ------------------------------------------------------------------------
  --  Identifier Table Entry
  type IdTabEntry is record
    Name           : Alfa;          --  identifier name in ALL CAPS
    Name_with_case : Alfa;          --  identifier name with original casing
    Link           : Index;
    Entity         : Entity_Kind;
    Read_only      : Boolean;       --  If Entity = Variable and Read_only = True,
                                    --    it's a typed constant.
    xTyp           : Exact_Typ;     --  Type identification
    Block_Ref      : Index;         --  Was: Ref (that was used also for what is now xTyp.Ref,
                                    --       which caused a mixup for functions' return types!)
    Normal         : Boolean;       --  value param?
    LEV            : Nesting_level;
    Adr_or_Sz      : Integer;
    Discrete_First : HAC_Integer;   --  If Entity = TypeMark, T'First
    Discrete_Last  : HAC_Integer;   --  If Entity = TypeMark, T'Last
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

  subtype Source_Line_String is String (1 .. 1000);

  -----------------------
  --  Compiler tables  --
  -----------------------

  type Object_Code_Table_Access is access HAC_Sys.PCode.Object_Code_Table;

  type    Arrays_Table_Type            is array (1 .. AMax)         of ATabEntry;
  type    Blocks_Table_Type            is array (0 .. BMax)         of BTabEntry;
  type    Display_Type                 is array (Nesting_level)     of Integer;
  type    Entries_Table_Type           is array (0 .. EntryMax)     of Index;
  type    Identifier_Table_Type        is array (0 .. Id_Table_Max) of IdTabEntry;
  subtype Strings_Constants_Table_Type is String (1 .. SMax);
  type    Tasks_Definitions_Table_Type is array (0 .. TaskMax)      of Index;
  --      ^ Task #0 is main task.

  --  Display: keeps track of addressing by nesting level. See Ben-Ari Appendix A.

  No_Id : constant := 0;

  type Source_Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

  type Current_Unit_Data is record
    --  Current source code information and scanner data
    compiler_stream  : Source_Stream_Access;
    source_file_name : HAL.VString;         --  Indicative, for error messages
    --  Parsing
    line_count       : Natural;             --  Source line counter, used for listing
    input_line       : Source_Line_String;
    c                : Character;           --  Character read from source program
    CC               : Integer;             --  Character counter (=column in current line)
    LL               : Natural;             --  Length of current line
  end record;

  ---------------------
  --  Compiler_Data  --
  ---------------------

  type Compiler_Data is new Ada.Finalization.Limited_Controlled with record
    CUD : Current_Unit_Data;
    --  Parsing
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
    Main_Proc_Id_Index      : Natural;
    String_Id_Index         : Natural;
    Strings_Table_Top       : Natural;
    Tasks_Definitions_Count : Natural;
    --  Object code
    ObjCode                 : Object_Code_Table_Access := new HAC_Sys.PCode.Object_Code_Table (0 .. CDMax);
    LC                      : Integer;  --  Location counter in the Object_Code_Table
    CMax                    : Integer;  --  Top of available ObjCode table;
                                        --  CMax + 1 .. CDMax: variable initialization code
    --  Information about source code
    Full_Block_Id             : HAL.VString;         --  Full block's Id (P1.P2.F3.P4)
    Main_Program_ID           : Alfa := Empty_Alfa;  --  Main program name
    Main_Program_ID_with_case : Alfa := Empty_Alfa;
    --
    listing_requested   : Boolean := False;
    comp_dump_requested : Boolean := False;
    listing   : Ada.Text_IO.File_Type;
    comp_dump : Ada.Text_IO.File_Type;
    --
    Err_Count  : Natural;
    Errs       : Error_set;
    error_pipe : Smart_error_pipe := null;
  end record;

  overriding procedure Finalize (CD : in out Compiler_Data);

  Universe : constant HAL.VString := HAL.To_VString ("[-- The Universe --]");

  type CASE_Label_Value is record
    Val       : HAC_Integer;  --  value of a choice in a CASE statement
    LC        : Index;        --  instruction address
    Is_others : Boolean;
  end record;

  type Constant_Rec is record
    TP : Exact_Typ;
    I  : HAC_Integer;  --  Includes Character and enumeration types (including Boolean)
    R  : HAC_Float;
  end record;

end HAC_Sys.Co_Defs;
