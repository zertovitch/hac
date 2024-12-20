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
     HAC_Sys.Files.Default,
     HAC_Sys.PCode,
     HAC_Sys.Targets;

with HAT;

with Ada.Containers.Hashed_Maps,
     Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Finalization,
     Ada.Strings.Hash,
     Ada.Strings.Unbounded.Hash,
     Ada.Text_IO;

package HAC_Sys.Co_Defs is
  --  NB: cannot be a child package of Compiler because of Parser, Scanner, ...

  use Defs;

  ------------------
  --  Exact type  --
  ------------------

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

  procedure Construct_Root (Root : out Exact_Typ; Typ : Typen)
  with Inline;

  ---------------------
  --  Exact subtype  --
  ---------------------

  type Exact_Subtyp is new Exact_Typ with record
    Discrete_First : HAC_Integer;   --  If subtype S is discrete, S'First
    Discrete_Last  : HAC_Integer;   --  If subtype S is discrete, S'Last
  end record;

  overriding procedure Construct_Root (Root : out Exact_Subtyp; Typ : Typen)
  with Inline;

  function Construct_Root (Typ : Typen) return Exact_Subtyp
  with Inline;

  undefined_subtyp : Exact_Subtyp;  --  This global variable is initialized.

  -------------------------------------------------------------------------
  ------------------------------------------------------------ATabEntry----
  -------------------------------------------------------------------------
  --  An Array Table Entry represents an array.  Each entry contains
  --  the following fields (fields marked with a C are used only by
  --  the compiler and ignored by the interpreter):
  --
  type Array_Table_Entry is record
    Index_xTyp   : Exact_Subtyp;  --  C  Subtype of the index (with bounds).
    Element_Size : Index;         --     Size of an element.
    Element_xTyp : Exact_Subtyp;  --  C  Subtype of the elements of the array.
                                  --     If the elements of the array are themselves
                                  --     arrays, Element_xTYP.Ref is an index to an
                                  --     entry in Arrays_Table (it's not a special case).
    Array_Size   : Index;         --  C  Total size of the array.
    dimensions   : Positive;      --  C  Total dimensions of the array.
  end record;

  -------------------------------------------------------------------------
  ------------------------------------------------------------BTabEntry----
  -------------------------------------------------------------------------
  --  Block Table Entry : Each entry represents a subprogram or a record type.
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
  type Block_Table_Entry is record
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
    (declared_number_or_enum_item,  --  Declared number: untyped constant, like
                                    --  "pi : constant := 3.1415927" (RM 3.3.2)
     --
     variable_object,  --  RM 3.3.1(5)
     constant_object,  --  RM 3.3.1(6)
     --
     type_mark,
     --
     paquetage,
     paquetage_body,
     --
     prozedure,
     prozedure_intrinsic,
     funktion,
     funktion_intrinsic,
     --
     tache,
     entree,
     --
     loop_identifier,
     alias);   --  Short name of another entity ("use" clause).

  subtype Object_Kind is Entity_Kind range variable_object .. constant_object;  --  RM 3.3

  type Declaration_Kind is
    (spec_unresolved, spec_resolved, complete,
     param_in, param_in_out, param_out);

  subtype Split_Declaration_Kind is Declaration_Kind range spec_unresolved .. complete;

  subtype Parameter_Kind is Declaration_Kind range param_in .. param_out;

  type Initialized_Kind is (none, explicit, implicit);

  type No_Maybe_Yes is (no, maybe, yes);

  --  Unless we are in the main execution path (not within "if", "loop", etc.)
  --  we can only note that a variable or field is *maybe* read or written.
  --
  procedure Elevate_to_Maybe (item : in out No_Maybe_Yes)
  with Inline;

  --  Elevate certainty level to "maybe" or "yes", depending on the context.
  --
  procedure Elevate_to_Maybe_or_Yes (item : in out No_Maybe_Yes; context : Flow_Context)
  with Inline;

  ------------------------------
  --  Identifier Table Entry  --
  ------------------------------
  type Identifier_Table_Entry is record
    name             : Alfa;                   --  Identifier name in ALL CAPS
    name_with_case   : Alfa;                   --  Identifier name with original casing
    link             : Index;                  --  Previous declaration on same nesting level, or No_Id
    entity           : Entity_Kind;
    decl_kind        : Declaration_Kind;       --  Declaration kind: forward or complete.
    --                                             Matters for a type, a constant, a subprogram;
    --                                             Values param_in .. param_out are
    --                                             for subprogram parameters.
    xtyp                  : Exact_Subtyp;      --  Subtype identification
    block_or_pkg_ref      : Index;             --  Reference in the block or package tables.
    normal                : Boolean;           --  value param?
    lev                   : Nesting_Level;
    adr_or_sz             : HAC_Integer;       --  Address, Size; index of aliased entity (USE) !! rather use block_or_pkg_ref ?!
    is_referenced         : Boolean;           --  For any item: is it referenced at all?
    is_read               : No_Maybe_Yes;      --  For variable or constant: is it read?
    is_written_after_init : No_Maybe_Yes;      --  Is variable written via ":=" or "out" mode?
    is_initialized        : Initialized_Kind;  --  For variable or constant: is it initialized?
    location              : Symbol_Location;
  end record;

  --  Entity                        Meaning of Adr_or_Sz
  --  -------------------------------------------------------------------------------
  --  declared_number_or_enum_item  Value (number), position (enumerated type)
  --  variable_object               Relative position in the stack.
  --  constant_object               Relative position in the stack.
  --  type_mark                     Size (in PCode stack items) of an object
  --                                    of the declared type.
  --  prozedure                     Index into the Object Code table.
  --  prozedure_intrinsic           Standard Procedure code (SP_Code).
  --  funktion                      Index into the Object Code table.
  --  funktion_intrinsic            Standard Function code (SF_Code).
  --  tache                         ?
  --  entree                        ?
  --  loop_identifier               ?
  --  alias                         Index into the Identifier table of the aliased entity.

  type Loop_Info is record
    loop_Id     : Natural;  --  No_Id : no identifier
    is_FOR_loop : Boolean;  --  Emit k_FOR_Release_Stack for each exited FOR loop
    start_line  : Natural;
  end record;

  subtype Source_Buffer_String is String (1 .. 65_536);

  subtype Source_Line_String is String (1 .. 1000);  --  Must be at least 200 (RM 2.2 (15))

  -----------------------
  --  Compiler tables  --
  -----------------------

  type    Arrays_Table_Type            is array (1 .. AMax)             of Array_Table_Entry;
  type    Blocks_Table_Type            is array (0 .. BMax)             of Block_Table_Entry;
  type    Display_Type                 is array (Nesting_Level)         of Integer;
  type    Entries_Table_Type           is array (0 .. entry_table_max)  of Index;
  type    Identifier_Table_Type        is array (0 .. Id_Table_Max)     of Identifier_Table_Entry;
  type    Nested_Loop_Table_Type       is array (1 .. loop_nesting_max) of Loop_Info;
  type    Packages_Table_Type          is array (0 .. package_table_max)             of Package_Table_Entry;
  type    Tasks_Definitions_Table_Type is array (0 .. TaskMax)          of Index;
  --      ^ Task #0 is main task.

  type    Use_HAT_Stack_Type           is array (0 .. nesting_and_descending_max) of Boolean;

  --  Display: keeps track of addressing by nesting level. See Ben-Ari Appendix A.

  No_Id       : constant :=  0;
  No_Id_Cache : constant := -1;

  subtype Source_Stream_Access is Files.Root_Stream_Class_Access;

  package Id_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => Alfa,
     Element_Type    => Positive,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => HAT."=");

  type Current_Unit_Data is record
    --  Current source code information and scanner data
    compiler_stream   : Source_Stream_Access;
    source_file_name  : HAT.VString;           --  Indicative, for error messages
    buffer            : Source_Buffer_String;
    buffer_length     : Natural;               --  = 0 only on init or when exhausted.
    buffer_position   : Positive;              --  Points to the next character to be read.
    --  Parsing
    location          : Symbol_Location;
    input_line        : Source_Line_String;
    c, prev_c         : Character;           --  Character read from source program
    CC                : Integer;             --  Character counter (=column in current line)
    LL                : Natural;             --  Length of current line
    --  Level 0 definitions visible to currently compiled unit:
    level_0_def       : Id_Maps.Map;
    Use_HAT_Stack     : Use_HAT_Stack_Type;
    use_hat_stack_top : Natural;
  end record;

  --  Set current source stream (file, editor data, zipped file,...)
  procedure Set_Source_Stream
    (CUD        : in out Current_Unit_Data;
     s          : in     Source_Stream_Access;
     file_name  : in     String;         --  Can be a virtual name (editor title, zip entry)
     start_line : in     Natural := 0);  --  We could have a shebang or other Ada sources before

  function Get_Source_Name (CUD : Current_Unit_Data) return String;

  function Source_Buffer_has_Data (CUD : Current_Unit_Data) return Boolean;

  type Compilation_Feedback is access procedure (message : String);

  type Compilation_Trace_Parameters is record
    pipe         : Defs.Smart_Error_Pipe := null;  --  Default: messages to Current_Error.
    progress     : Compilation_Feedback  := null;  --  Default: messages to Current_Output.
    detail_level : Natural               := 0;
  end record;

  default_trace : constant Compilation_Trace_Parameters := (others => <>);

  procedure Silent_Diagnostics (kit : Diagnostic_Kit) is null;
  procedure Silent_Feedback (message : String) is null;

  silent_trace : constant Compilation_Trace_Parameters :=
    (Silent_Diagnostics'Access, Silent_Feedback'Access, 0);

  type Dummy_Procedure_Access is access procedure;

  package Exported_Procedure_Mapping is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => String,
     Element_Type    => Dummy_Procedure_Access,
                        --  Actually: HAC_Sys.Interfacing.Exported_Procedure, but we
                        --  end up in a circular unit dependency mess.
     Hash            => Ada.Strings.Hash,
     Equivalent_Keys => "=");

  --  Global object used as a default for library file management:
  default_file_catalogue : aliased Files.Default.File_Catalogue;

  ---------------------
  --  Compiler_Data  --
  ---------------------

  type Compiler_Data is new Ada.Finalization.Limited_Controlled with record
    CUD : Current_Unit_Data;
    --  Scanning & Parsing
    Sy, prev_sy      : Symbol;         --  sy: last Symbol read by In_Symbol
    prev_sy_loc      : Symbol_Location;
    Id               : Alfa;               --  Identifier from In_Symbol
    Id_with_case     : Alfa;               --  Same as Id, but with casing.
    Id_location      : Integer;            --  Cache for Locate_CD_Id
    INum             : HAC_Integer;        --  Integer from In_Symbol
    RNum             : HAC_Float;          --  Float number from In_Symbol
    SLeng            : Integer;            --  String Length
    pkg_prefix       : HAT.VString;        --  Prefix of package being currently parsed.
    --  Compiler tables. Floats and Strings are used by interpreter at run-time.
    Arrays_Table            : Arrays_Table_Type;  --  NB: only static-sized arrays so far.
    Blocks_Table            : Blocks_Table_Type;
    Display                 : Display_Type;
    Entries_Table           : Entries_Table_Type;
    Float_Constants_Table   : Float_Constants_Table_Type;
    id_table                : Identifier_Table_Type;
    Nested_Loop_Table       : Nested_Loop_Table_Type;
    Packages_Table          : Packages_Table_Type;
    Strings_Constants_Table : Strings_Constants_Table_Type;
    Tasks_Definitions_Table : Tasks_Definitions_Table_Type;
    --  Indices to compiler tables
    Arrays_Count            : Natural;
    Blocks_Count            : Natural;
    Entries_Count           : Natural;
    Float_Constants_Count   : Natural;
    Id_Count                : Natural;
    loop_nesting_level      : Natural;
    main_proc_id_index      : Natural := No_Id;  --  No_Id <=> the main unit is not executable.
    Packages_Count          : Natural;
    String_Id_Index         : Natural;
    Strings_Table_Top       : Natural;
    Tasks_Definitions_Count : Natural;
    --  Object code
    --  Mostly for HAC VM / p-code -> will be moved to HAC_Sys.Targets.HAC_Virtual_Machine)
    target                  : Targets.Abstract_Machine_Reference := null;
    ObjCode                 : PCode.Object_Code_Table (0 .. CDMax);
    LC                      : Integer;  --  Location counter in the Object_Code_Table
    CMax                    : Integer;  --  Top of available ObjCode table;
                                        --  CMax + 1 .. CDMax: variable initialization code
    folded_instructions      : Natural;
    specialized_instructions : Natural;
    --  Information about source code
    Full_Block_Id             : HAT.VString;         --  Full block's Id (P1.P2.F3.P4)
    main_unit_ident           : Alfa := Empty_Alfa;
    main_unit_ident_with_case : Alfa := Empty_Alfa;
    Exported_Procedures       : Exported_Procedure_Mapping.Map;
    --
    listing_requested   : Boolean := False;
    comp_dump_requested : Boolean := False;
    listing   : Ada.Text_IO.File_Type;
    comp_dump : Ada.Text_IO.File_Type;
    --
    error_count, minor_error_count : Natural;
    remarks      : Remark_Set := default_remarks;
    diags        : Diagnostic_Set;
    total_lines  : Natural;
    cat          : Files.Abstract_File_Catalogue_Reference :=
                     default_file_catalogue'Access;
    trace        : Compilation_Trace_Parameters;
    --  On `WITH X`, we start the recursive compilation of X,
    --  if X is not yet compiled or built-in. We monitor the
    --  recursion level for the fun of it.
    recursion    : Natural := 0;
  end record;

  overriding procedure Initialize (CD : in out Compiler_Data);
  overriding procedure Finalize (CD : in out Compiler_Data);

  function Is_Executable (CD : Compiler_Data) return Boolean;
  function Is_HAC_VM (CD : Compiler_Data) return Boolean;

  procedure Set_Target
    (CD : in out Compiler_Data; new_target : Targets.Abstract_Machine_Reference);

  --  Method used internally by HAC.
  --  Prefer using Build_Data's Set_File_Catalogue method.
  --
  procedure Set_File_Catalogue
    (CD  : in out Compiler_Data;
     cat : in     Files.Abstract_File_Catalogue_Reference);

  type Compiler_Data_Access is access all Co_Defs.Compiler_Data;

  --  Image function for compilation errors or out-of-range exception messages.
  --
  function Discrete_Image
    (CD : Compiler_Data; value : HAC_Integer; Typ : Typen; Ref : Index) return String;

  function Discrete_Range_Image
    (CD : Compiler_Data; value_1, value_2 : HAC_Integer; Typ : Typen; Ref : Index) return String;

  --  Size of a variable or subprogram parameter
  --
  function Size_of (CD : Compiler_Data; Id_Index : Natural) return Positive;

  procedure Increment_Nesting_or_Descending_Level (CD : in out Compiler_Data);
  procedure Decrement_Nesting_or_Descending_Level (CD : in out Compiler_Data);

  Universe : constant HAT.VString := HAT.To_VString ("[-- The Universe --]");

  type Constant_Rec is record
    TP : Exact_Subtyp;
    I  : HAC_Integer;  --  Includes Character and enumeration types (including Boolean)
    R  : HAC_Float;
  end record;

end HAC_Sys.Co_Defs;
