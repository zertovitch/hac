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

with Ada.Streams, Ada.Text_IO;

package HAC.Data is

  pragma Elaborate_Body;

  -----------------------------------------------------------------------------
  -- SmallAda global constants, types, and (ouch!) data objects. (<- removing in progress)
  -----------------------------------------------------------------------------

  -------------------------------------------------------------------------
  -- Global constants
  -------------------------------------------------------------------------

  StMax   : constant := 2000;          --  Maximum Stack Size
  STKINCR : constant := 200;           --  Stack Increment allocated per Task

  Header : constant String := "HAC - Hacker's Ada Compiler";
  --  Was: "Small-Ada  Macintosh Ver 1.1  Nov 1989  George Washington University"

  MaxINT     : constant Integer := Integer'Last - 1;

  Alng       : constant := 40;            --  NO. OF SIGNIFICANT CHARS IN IDENTIFIERS
  AMax       : constant := 30;            --  Size OF ARRAY-TABLE
  BMax       : constant := 25;            --  Size OF Block-TABLE
  CallSTDP   : constant := 0;             --  Call type for standard procedure call
  CallSTDE   : constant := 1;             --  Call type for standard Entry Call
  CallTMDE   : constant := 2;             --  Call type for timed Entry Call
  CallCNDE   : constant := 3;             --  Call type for conditional Entry call
  C2Max      : constant := 20;            --  Size OF FLOAT Constant TABLE
  Cases_Max  : constant := 30;            --  Max number of cases in a CASE statement
  CDMax      : constant := 2500;          --  Size OF ObjCode
  ERMax      : constant := 90;            --  MAX Error NO.
  EMax       : constant :=  77;           --  MAX EXPONENT of FLOAT NUMBERS
  EMin       : constant := -78;           --  MIN EXPONENT
  EntryMax   : constant := 30;            --  Maximum Number of Entry Statements
  FMax       : constant := 20;            --  Maximum Number of files for I/O
  KMax       : constant := 7;             --  Max No. of significant digits
  LMax       : constant := 7;             --  maximum Level

  OrdMinChar : constant := 0;             --  Ord of First Char
  OrdMaxChar : constant := 255;           --  Ord of last Char

  PriMax     : constant := 100;           --  Maximum Task priority
  SMax       : constant := 10_000;        --  Size of String table
  TaskMax    : constant := 12;            --  Max # of concurrent tasks

  Wind_Size    : constant := TaskMax + 2;    --  SnapShot window size
  Id_Table_Max : constant := 200;            --  Size of identifier table
  XMax         : constant Integer := MaxINT;

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
   --
   Plus,     --  +
   Minus,    --  -
   Times,    --  *
   Divide,   --  /
   Power,    --  **
   --
   EQL,      --  =
   NEQ,      --  /=
   GTR,      --  >
   GEQ,      --  >=
   LSS,      --  <
   LEQ,      --  <=
   --
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

  subtype Comparison_Operator is KeyWSymbol range EQL .. LEQ;
  subtype Arithmetic_Binary_Operator is KeyWSymbol range Plus .. Power;

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
  function To_String (a: Alfa) return String;
  function To_Alfa (s: String) return Alfa;

  --  The order of these is significant
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

  type Typ_Set is array (Types) of Boolean;

  subtype Standard_Typ is Types range NOTYP .. xChars;

  Standard_or_Enum_Typ : constant Typ_Set :=
    (Standard_Typ | Enums => True, others => False);

  Discrete_Typ : constant Typ_Set :=  --  RM 3.2 (12)
    (Ints | Bools | xChars | Enums => True, others  => False);

  subtype Numeric_Typ is Types range Ints .. Floats;  --  RM 3.2 (1)

  subtype Index is Integer range -XMax .. +XMax;

  -- =======================================================================
  --  Global Variables
  -- =======================================================================

  Err_Count : Natural;      --  (Marcelo) copied from PC source

  Listing_Was_Requested : Boolean;
  Debug                 : Boolean;            --  Run-time program info on/off flag
  Listing, Sym_dump : Ada.Text_IO.File_Type;  --  File pointers

  Main_Program_ID           : Alfa := Empty_Alfa;  --  Main program name
  Main_Program_ID_with_case : Alfa := Empty_Alfa;

  CMax : Integer := CDMax; -- := added 7-Dec-2009
  --  top of available ObjCode table;
  --  CMax+1..CDMax: variable initialization code (elaboration)

  --  HAC's default floating-point type is double-precision
  --  and is called "Real" in HAC's HAC_Pack package.
  --
  type HAC_Float is digits 15;
  HAC_Float_Name   : constant String := "REAL";
  HAC_Integer_Name : constant String := "INTEGER";

  --  Debugging Flags - these flags are used to print out information
  --  about the code status.  If a flag is true, then the section of
  --  code associated with it will print out extra information allowing
  --  for easier debugging.

  qDebug : Boolean := True;

  type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

  --  Set current source stream (file, editor data, zipped file,...)
  procedure c_Set_Stream (
    s         : Stream_Access;
    file_name : String         --  Can be virtual (editor, zip entry)
  );

  function Get_Current_Source_Name return String;

  --  Get the next line from source
  procedure c_Get_Next_Line (InpLine : out String; Last : out Natural);

end HAC.Data;
