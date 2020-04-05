-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  This packages contains constants and global data (ouch!) for the compiler
--  and the p-code interpreter.

with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with HAC.UErrors;

package HAC.Data is

  pragma Elaborate_Body;

  -----------------------------------------------------------------------------
  -- SmallAda global constants, Types, and data objects.
  -----------------------------------------------------------------------------

  -------------------------------------------------------------------------
  -- Global constants [ 8-( ]
  -------------------------------------------------------------------------

  StMax   : constant := 2000;          --  Maximum Stack Size
  STKINCR : constant := 200;           --  Stack Increment allocated per Task

  c128 : constant Character := Character'Val (128);

  Header : constant String := "HAC - Hacker's Ada Compiler";
  --  Was: "Small-Ada  Macintosh Ver 1.1  Nov 1989  George Washington University"

  MaxINT     : constant Integer := Integer'Last - 1;

  Alng       : constant := 40;            --  NO. OF SIGNIFICANT CHARS IN IDENTIFIERS
  AMax       : constant := 30;            --  Size OF ARRAY-TABLE
  BMax       : constant := 25;            --  Size OF Block-TABLE
  CallSTDP   : constant := 0;             --  Call type for standard procedure
                                          --Call
  CallSTDE   : constant := 1;             --  Call type for standard Entry Call
  CallTMDE   : constant := 2;             --  Call type for timed Entry Call
  CallCNDE   : constant := 3;             --  Call type for conditional Entry
                                          --Call
  C2Max      : constant := 20;            --  Size OF FLOAT Constant TABLE
  CSMax      : constant := 30;            --  MAX NO. OF CASES
  CDMax      : constant := 2500;          --  Size OF ObjCode
  ERMax      : constant := 90;            --  MAX Error NO.
  EMax       : constant := 77;            --  MAX EXPONENT of FLOAT NUMBERS
  EMin       : constant := -78;          --  MIN EXPONENT
  EntryMax   : constant := 30;            --  Maximum Number of Entry
                                          --Statements
  FMax       : constant := 20;            --  Maximum Number of files for I/O
  KMax       : constant := 7;             --  Max No. of significant digits
  LLNG       : constant := 83;            --  input line Length
  LMax       : constant := 7;             --  maximum Level
  NMax       : constant Integer := MaxINT;
  OrdMinChar : constant := 0;             --  Ord of First Char
  OrdMaxChar : constant := 255;           --  Ord of last Char
  --  OMax: highest Order ObjCode - see constants in HAC.PCode.
  OMax       : constant := 79;
  PriMax     : constant := 100;           --  Maximum Task priority
  SMax       : constant := 10_000;        --  Size of String-table
  TaskMax    : constant := 12;            --  Max # of concurrent tasks

  Wind_Size : constant := TaskMax + 2;    --  SnapShot window size
  TMax      : constant := 200;            --  Size of identifier table
  XMax      : constant Integer := MaxINT;

  -- =======================================================================
  --  Global Types
  -- =======================================================================

  -----------------------------------------------------------------------
  ---------------------------------------------------------KeyWSymbol----
  -----------------------------------------------------------------------
  type KeyWSymbol is (   --  All keywords or symbols used by the compiler
   IntCon,
   FloatCon,
   CharCon,
   StrCon,
   Plus,
   MinUS,
   xTimes,
   xx_Power,
   Divide,
   EQL,
   NEQ,
   GTR,
   GEQ,
   LSS,
   LEQ,
   LParent,
   RParent,
   LBrack,
   RBrack,
   Comma,
   Semicolon,
   Period,
   Range_Double_Dot_Symbol,  --  ".." compound delimiter (RM 2.2)
   Colon,
   Alt,
   Finger,
   Becomes,
   IDent,
   String_Symbol, -- !! hack ! used for constraining the String unconstrained array
   USy,                -- (Apparently) unused symbol
   Dummy_Symbol,       -- Symbol that is never parsed.
   Ampersand_Symbol,
   --                  Ada keywords
   ABORT_Symbol,
   ABSTRACT_Symbol,
   ACCEPT_Symbol,
   ACCESS_Symbol,
   ALIASED_Symbol,
   ALL_Symbol,
   AND_Symbol,
   ARRAY_Symbol,
   AT_Symbol,
   BEGIN_Symbol,
   BODY_Symbol,
   CASE_Symbol,
   CONSTANT_Symbol,
   DECLARE_Symbol,
   DELAY_Symbol,
   DELTA_Symbol,
   DIGITS_Symbol,
   DO_Symbol,
   ELSE_Symbol,
   ELSIF_Symbol,
   END_Symbol,
   ENTRY_Symbol,
   EXCEPTION_Symbol,
   EXIT_Symbol,
   FOR_Symbol,
   FUNCTION_Symbol,
   GENERIC_Symbol,
   GOTO_Symbol,
   IF_Symbol,
   IN_Symbol,
   INTERFACE_Symbol,
   IS_Symbol,
   LIMITED_Symbol,
   LOOP_Symbol,
   MOD_Symbol,
   NEW_Symbol,
   NOT_Symbol,
   NULL_Symbol,
   OF_Symbol,
   OR_Symbol,
   OTHERS_Symbol,
   OUT_Symbol,
   OVERRIDING_Symbol,
   PACKAGE_Symbol,
   PRAGMA_Symbol,
   PRIVATE_Symbol,
   PROCEDURE_Symbol,
   PROTECTED_Symbol,
   RAISE_Symbol,
   RANGE_Keyword_Symbol,  --  "range" reserved word (RM 2.9)
   RECORD_Symbol,
   REM_Symbol,
   RENAMES_Symbol,
   REQUEUE_Symbol,
   RETURN_Symbol,
   REVERSE_Symbol,
   SELECT_Symbol,
   SEPARATE_Symbol,
   SOME_Symbol,
   SUBTYPE_Symbol,
   SYNCHRONIZED_Symbol,
   TAGGED_Symbol,
   TASK_Symbol,
   TERMINATE_Symbol,
   THEN_Symbol,
   TYPE_Symbol,
   UNTIL_Symbol,
   USE_Symbol,
   WHEN_Symbol,
   WHILE_Symbol,
   WITH_Symbol,
   XOR_Symbol);

  subtype Opcode is Integer range 0 .. OMax;

  subtype Operand1 is Integer range -LMax .. +LMax;       -- -LMax..+LMax;  --
                                                          --operand
  subtype Operand2 is Integer range -NMax .. +NMax;       -- -NMax..+NMax;  --
                                                          --operand

  type Set is array (Integer range <>) of Boolean;
  function "+" (a, b : Set) return Set;

  ---------------------
  -- Sets of symbols --
  ---------------------

  type Symset is array (KeyWSymbol) of Boolean;
  function "+" (a, b : Symset) return Symset;
  function "+" (a : Symset; b : KeyWSymbol) return Symset;
  function "-" (a, b : Symset) return Symset;
  function "-" (a : Symset; b : KeyWSymbol) return Symset;
  Empty_Symset : constant Symset := (others => False);

  -----------------
  -- Identifiers --
  -----------------

  --  Alfa is a space-padded string
  subtype Alfa is String (1 .. Alng);
  Empty_Alfa : Alfa := (others => ' ');
  function Alfa_to_String(a: Alfa) return String;

  type aObject is (
   Konstant,  --  Numerical constant
   Variable,
   TypeMark,
   Prozedure,
   Funktion,
   aTask,
   aEntry,
   Label);

  --  The Order of these is significant
  type Types is (
   NOTYP,
   Ints,
   Floats,
   Bools,
   xChars,
   Arrays,
   Records,
   Enums,
   Strings);

  type Typset is array (Types) of Boolean;

  type CHTP is (Letter, LowCase, Number, Special, Illegal);
  type Set_of_CHTP is array (CHTP) of Boolean;
  special_or_illegal : Set_of_CHTP :=
   (Letter   |
    LowCase  |
    Number   => False,
    Special  |
    Illegal  => True);

  subtype Index is Integer range -XMax .. +XMax;

  type Item is record
    TYP : Types;
    Ref : Index;
  end record;

  subtype Nesting_level is Integer range 0 .. LMax;

  subtype Longint is Integer;

  -- ----------------------------------------------------------------------
  -- ----------------------------------------------------------TabEntry----
  -- ----------------------------------------------------------------------
  --  Identifier-Table Entry
  type TabEntry is record
    Name : Alfa;          --  identifier name
    Link : Index;
    Obj  : aObject;       --  One of Konstant, Variable, TypeMark,
                          --   Prozedure, Funktion, Task, Entry
    TYP  : Types;         --  One of NoTyp, Ints, Floats, Bools,
                          --   xChars, Arrays, Records, Enums, Strings
    Ref  : Index;         --  Index into the Block table
    Normal: Boolean;       --  value param?
    LEV  : Nesting_level;
    Adr  : Integer;       --  index into the Code table for the
                          --  procedure code (if Name is a
                          --  procedure or function)
  end record;

  -- ----------------------------------------------------------------------
  -- ---------------------------------------------------------ATabEntry----
  -- ----------------------------------------------------------------------
  --  Array-Table Entry : Array table entry represents an array.  Each entry
  --  contains the following fields (fields marked with a C are used only by
  --  the compiler and ignored by the interpreter):
  type ATabEntry is record
    Index_TYP    : Types;         --  C Type of the index
    Element_TYP  : Types;         --  C Type of the elements of the array
    ELREF        : Index;         --  C Pointer to an entry in ArraysTab if the
                                  --    elements of the array are themselves arrays
    Size         : Index;         --  C Total size of the array
    Low, High    : Index;         --  Limits on the array index: array (Low..High) of Element_TYP
    ELSize       : Index;         --  Size of an element
  end record;

  -- ----------------------------------------------------------------------
  -- ---------------------------------------------------------BTabEntry----
  -- ----------------------------------------------------------------------
  --  Block-table Entry : Each entry represents a procedure
  --  An activation record consists of:
  --
  --         (1) the five word fixed area; (see definition of S in Interpreter)
  --         (2) an area for the actual parameters (whether values or
  --             addresses), and
  --         (3) an area for the local variables of the procedure
  --
  --  Once again, fields marked with C are used only by the compiler
  type BTabEntry is record
    Id   : Alfa;      --  name of the block
    Last : Index;     -- C pointer to the last identifier in
                      --   this procedure
    LastPar: Index;     -- C pointer to the last parameter in
                        --   this procedure
    PSize: Index;     --  sum of the lengths of areas (1) & (2) above
    VSize: Index := 0;     --  sum of PSize and length of area (3)
                      --  (i.e. size of the activation record for
                      --  this procedure)
    SrcFrom: Longint;   --  Source code line count.  Source starts here
    SrcTo: Longint;   --  and goes until here    (* Manuel *)
  end record;

  -- ----------------------------------------------------------------------
  -- -------------------------------------------------------------Order----
  -- ----------------------------------------------------------------------
  --  PCode instruction record (stores a compiled PCode instruction)
  type Order is record
    F : Opcode;      --  Opcode (or instruction field)
    X : Operand1;     --  Operand 1 is used to point to the static level
    Y : Operand2;     --  Operand 2 is used to pass operands to the
                      --  instructions
  end record;

  ------------------------------------------------------------------------
  ------------------------------------------------------------FilDescr----
  ------------------------------------------------------------------------

  subtype File_count is Integer range 0 .. FMax;
  subtype File_index is Integer range 1 .. FMax;
  type FilDescr_Texts is array (File_index) of File_Type;
  type FilDescr_Lname is array (File_index) of Natural;
  type FilDescr_Names is array (File_index) of String (1 .. Alng);

  No_File_Index: constant:= -1;

  type FilDescr is record
    Curr  : Integer;
    Kount : File_count:= 0;
    Fil   : FilDescr_Texts;
    Nam   : FilDescr_Names;
    LNam  : FilDescr_Lname;
  end record;

  subtype InternalTime is Float;
  --  internal timing values are stored in milliseconds since
  --  the start of the current year.
  --  Manuel: Replaced TIME for InternalTime to avoid problems
  --  with already defined Mac type.

  -- =======================================================================
  --  Global Variables
  -- =======================================================================

  Err_Count : Natural;      --  (Marcelo) copied from PC source
  Scheduler : Integer;      --  (Marcelo) scheduler routine to use, choose
                            --  from several

  TSlice : Integer;         --  Default Task time-slice in milliseconds
                            --  Feldman: 60ths of a sec on Mac

  Listing_Was_Requested : Boolean;
  Debug                 : Boolean;          --  Run-time program info on/off flag
  Map                   : Boolean;            --  Compile-time output of Global
                                            --VAR Map
  RunningTime         : Boolean;    --  Display running timer

  Listing, Sym_dump : File_Type;            --  File pointers

  CH : Character;  --  previous Character Read from Source program

  Standard_Typ : constant Typset :=
    (NOTYP | Ints | Floats | Bools | xChars => True, others => False);

  Standard_or_Enum_Typ : constant Typset :=
    Standard_Typ or Typset'(Enums => True, others => False);

  ProgramID : Alfa := (others => ' '); --  Main program name

  m, N : Integer;
  CMax : Integer := CDMax; -- := added 7-Dec-2009
  --  top of available ObjCode table;
  --  CMax+1..CDMax: variable initialization code (elaboration)

  --  Compiler tables

  --* Manuel  Renamed most compiler tables to use more meaningful names.
  --*
  --*  Old Name    New Name        Use
  --*  ----------- -----------     -------------------------------
  --*  Tab         IdTab           Table of all identifiers
  --*  ATab        ArraysTab       Array table
  --*  BTAb        BlockTab        Block table
  --*  Fat         FileIOTab       File I/O Table
  --*  FcTab       FloatPtTab      Floating Point Constant Table
  --*  STab        StringTab       String Table
  --*  TaskTab     TaskDefTab      Task Definition Table
  --*  Key         AdaKeyW         Array of Ada keywords in Order
  --*  Ksy         AdaKeyWSy       Corresponding keyword symbols
  --*  Code        ObjCode         Object Code table

  subtype AdaKeyW_String is String (1 .. 12);

  type AdaKeyW_Pair is record
    st: AdaKeyW_String;
    sy: KeyWSymbol;
  end record;

  type AdaKeyW_List is array(Positive range <>) of AdaKeyW_Pair;

  AdaKeyW    : constant AdaKeyW_List;

  --  HAC's default floating-point type is double-precision
  --  and is called "Real" in HAC's HAC_Pack package.
  --
  type HAC_Float is digits 15;
  HAC_Float_Name   : constant String := "REAL";
  HAC_Integer_Name : constant String := "INTEGER";

  ArraysTab  : array (1 .. AMax) of ATabEntry;  --  Array table
  BlockTab   : array (0 .. BMax) of BTabEntry;  --  Block-table [7-Dec-2009:
                                                --was 1..]
  ObjCode    : array (0 .. CDMax) of Order;     --  Object Code table
  EntryTab   : array (0 .. EntryMax) of Index;  --  Entry Table
  FileIOTab  : FilDescr;                        --  File I/O table
  FloatPtTab : array (1 .. C2Max) of HAC_Float; --  Float Constant table
  StringTab  : array (0 .. SMax) of Character;  --  String table
  IdTab      : array (0 .. TMax) of TabEntry;   --  Identifier table
  TaskDefTab : array (0 .. TaskMax) of Index;   --  Task Table

  --  Display - keeps track of addressing by nesting level.
  --  See Ben-Ari Appendix A.
  Display : array (0 .. LMax) of Integer;

  -- --- Indices To Compiler tables  ---
  -- Manuel : These could use some renaming tool

  A      : Integer;       --  Index To ArraysTab
  B      : Integer;       --  Index To BlockTab
  C1     : Integer;       --  Index To FloatPtTab
  C2     : Natural;       --  Index To FloatPtTab
  Sx     : Integer;       --  Index To StringTab
  T      : Integer;       --  Index To IdTab
  TCount : Integer;       --  Index To TskDefTab
  ECount : Integer;       --  Index To EntryTAB

  type SSTBzz is array (Character'(' ') .. ']') of KeyWSymbol;
  Special_Symbols : constant SSTBzz :=
   SSTBzz'
   ('+'    => Plus,
    '-'    => MinUS,
    '*'    => xTimes,
    '/'    => Divide,
    '('    => LParent,
    ')'    => RParent,
    '['    => LBrack,
    ']'    => RBrack,
    '='    => EQL,
    '"'    => NEQ,    -- ?!
    ','    => Comma,
    ';'    => Semicolon,
    '&'    => Ampersand_Symbol,
    others => NULL_Symbol);

  -- --- Error Control Variables ---

  type Error_set is array (HAC.UErrors.Error_code) of Boolean;
  Errs       : Error_set;       --  compilation Errors
  error_free : constant Error_set := (others => False);

  ErrPos   : Integer;
  SkipFlag : Boolean;                   --  used by procedure EndSkip
  EofInput : Boolean;                   --  signals end of input (this is set
                                        --to false and
                                        --  never used again!)

  -- --- InSymbol (Scanner) Variables ---

  Sy             : KeyWSymbol;                --  last KeyWSymbol Read by
                                              --InSymbol
  syStart, syEnd : Integer;                   --  Start and end on line for
                                              --the symbol in Sy
  syLine         : Integer;                   --  Source line of Sy
  Id             : Alfa;                      --  identifier from InSymbol
  INum           : Integer;                   --  Integer from InSymbol
  RNum           : HAC_Float;                 --  FLOAT Number from InSymbol
  SLeng          : Integer;                   --  String Length
  CharacterTypes : array (Character) of CHTP;   --  character types
  InpLine        : String (1 .. 255);             --  input line. Manuel:
                                                  --Renamed To InpLine
  CC             : Integer;                   --  character counter (=column in current line)
  LC             : Integer;                   --  location counter
  LL             : Integer;                   --  Length of current line
  Line_Count     : Integer;                   --  Source line counter, used
                                              --for listing
  Tx             : Integer;                   --  scratch Variable
  TCH            : Character;                      --  scratch Variable
  TStr           : String (1 .. 14);             --  scratch Variable

  --  Debugging Flags - these flags are used to print out information
  --  about the code status.  If a flag is true, then the section of
  --  code associated with it will print out extra information allowing
  --  for easier debugging.

  qDebug : Boolean := True;

  procedure cICompiler;

  procedure cFeedback;

  --  Error messages can be routed to a specialized pipe instead of
  --  text-based Standard_Error.

  --  !! This would be ideally private. Please use c_Set_Stream below.
  current_error_pipe: HAC.UErrors.Smart_error_pipe := null;
  current_compiler_file_name : Unbounded_String;

  type Stream_Access is access all Root_Stream_Type'Class;

--  Set current source stream (file, editor data, zipped file,...)
  procedure c_Set_Stream (
    s         : Stream_Access;
    file_name : String         --  Can be virtual (editor, zip entry)
  );

  --  Get the next line from source
  procedure cGetNextLine (InpLine : out String; Last : out Natural);

  --  No more input left
  function cEndOfSource return Boolean;

  procedure cFoundError
    (errCode: HAC.UErrors.Error_code;
     srcNumber, charStart, charEnd, objNumber : Integer;
     hint: String);

private

  AdaKeyW : constant AdaKeyW_List:=
      ( ("ABORT       ", ABORT_Symbol),
        ("ABSTRACT    ", ABSTRACT_Symbol),     -- [added in] Ada 95
        ("ABS         ",  USy),                -- !! SmallAda has a built-in function (wrong)
        ("ACCEPT      ", ACCEPT_Symbol),
        ("ACCESS      ", ACCESS_Symbol),
        ("ALIASED     ", ALIASED_Symbol),      -- Ada 95
        ("ALL         ", ALL_Symbol),          -- Ada 95
        ("AND         ", AND_Symbol),
        ("ARRAY       ", ARRAY_Symbol),
        ("AT          ", AT_Symbol),
        ("BEGIN       ", BEGIN_Symbol),
        ("BODY        ", BODY_Symbol),
        ("CASE        ", CASE_Symbol),
        ("CONSTANT    ", CONSTANT_Symbol),
        ("DECLARE     ", DECLARE_Symbol),
        ("DELAY       ", DELAY_Symbol),
        ("DELTA       ", DELTA_Symbol),
        ("DIGITS      ", DIGITS_Symbol),
        ("DO          ", DO_Symbol),
        ("ELSE        ", ELSE_Symbol),
        ("ELSIF       ", ELSIF_Symbol),
        ("END         ", END_Symbol),
        ("ENTRY       ", ENTRY_Symbol),
        ("EXCEPTION   ", EXCEPTION_Symbol),
        ("EXIT        ", EXIT_Symbol),
        ("FOR         ", FOR_Symbol),
        ("FUNCTION    ", FUNCTION_Symbol),
        ("GENERIC     ", GENERIC_Symbol),
        ("GOTO        ", GOTO_Symbol),
        ("IF          ", IF_Symbol),
        ("IN          ", IN_Symbol),
        ("INTERFACE   ", INTERFACE_Symbol),    -- Ada 2005
        ("IS          ", IS_Symbol),
        ("LIMITED     ", LIMITED_Symbol),
        ("LOOP        ", LOOP_Symbol),
        ("MOD         ", MOD_Symbol),
        ("NEW         ", NEW_Symbol),
        ("NOT         ", NOT_Symbol),
        ("NULL        ", NULL_Symbol),
        ("OF          ", OF_Symbol),
        ("OR          ", OR_Symbol),
        ("OTHERS      ", OTHERS_Symbol),
        ("OUT         ", OUT_Symbol),
        ("OVERRIDING  ", OVERRIDING_Symbol),   -- Ada 2005
        ("PACKAGE     ", PACKAGE_Symbol),
        ("PRAGMA      ", PRAGMA_Symbol),
        ("PRIVATE     ", PRIVATE_Symbol),
        ("PROCEDURE   ", PROCEDURE_Symbol),
        ("PROTECTED   ", PROTECTED_Symbol),    -- Ada 95
        ("RAISE       ", RAISE_Symbol),
        ("RANGE       ", RANGE_Keyword_Symbol),
        ("RECORD      ", RECORD_Symbol),
        ("REM         ", REM_Symbol),
        ("RENAMES     ", RENAMES_Symbol),
        ("REQUEUE     ", REQUEUE_Symbol),      -- Ada 95
        ("RETURN      ", RETURN_Symbol),
        ("REVERSE     ", REVERSE_Symbol),
        ("SELECT      ", SELECT_Symbol),
        ("SEPARATE    ", SEPARATE_Symbol),
        ("SOME        ", SOME_Symbol),         -- Ada 2012
        ("SUBTYPE     ", SUBTYPE_Symbol),
        ("SYNCHRONIZED", SYNCHRONIZED_Symbol), -- Ada 2005
        ("TAGGED      ", TAGGED_Symbol),       -- Ada 95
        ("TASK        ", TASK_Symbol),
        ("TERMINATE   ", TERMINATE_Symbol),
        ("THEN        ", THEN_Symbol),
        ("TYPE        ", TYPE_Symbol),
        ("UNTIL       ", UNTIL_Symbol),        -- Ada 95
        ("USE         ", USE_Symbol),
        ("WHEN        ", WHEN_Symbol),
        ("WHILE       ", WHILE_Symbol),
        ("WITH        ", WITH_Symbol),
        ("XOR         ", XOR_Symbol)
       );

end HAC.Data;
