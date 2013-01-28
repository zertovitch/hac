-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset

-- Legal licensing note:

--  Copyright (c) 2013 Gautier de Montmollin
--
--  History and authors list of works HAC was originally derived from
--  can be found in hac.txt.

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2013 on the site
-- http://www.opensource.org/licenses/mit-license.php

-------------------------------------------------------------------------------------
--

with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

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
  -- was "Small-Ada  Macintosh Ver 1.1  Nov 1989  George Washington University"

  MaxINT     : constant Integer := Integer'Last;
  Alng       : constant := 10;            --  NO. OF SIGNIFICANT CHARS IN
                                          --IDENTIFIERS
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
  NKW        : constant := 64;            --  No. of AdaKeyW words
  NMax       : constant Integer := MaxINT;
  OrdMinChar : constant := 0;             --  Ord of First Char
  OrdMaxChar : constant := 255;           --  Ord of last Char
  OMax       : constant := 74;            --  highest Order ObjCode
  PriMax     : constant := 100;           --  Maximum Task priority
  SMax       : constant := 1000;          --  Size of String-table
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
  type KeyWSymbol is (  --  All KeyWSymbols used by the Compiler
   IntCon,
   FloatCon,
   CharCon,
   StrCon,
   Plus,
   MinUS,
   xTimes,
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
   Colon,
   Alt,
   Finger,
   Becomes,
   IDent,
   StringSy,
   AcceptSy,
   USy,
   And_Symbol,
   ArraySy,
   ATSy,
   BeginSy,
   BodySy,
   CaseSy,
   ConstSy,
   DeclareSy,
   DelaySy,
   doSy,
   ElseSy,
   ElsIfSy,
   EndSy,
   EntrySy,
   ExitSy,
   ForSy,
   FuncSy,
   IfSy,
   InSy,
   IsSy,
   LoopSy,
   ModSy,
   Not_Symbol,
   NullSy,
   OFSy,
   Or_Symbol,
   OthersSy,
   OutSy,
   ProcSy,
   RangeSy,
   RecordSy,
   ReturnSy,
   ReverseSy,
   SelectSy,
   SubTypeSy,
   TaskSy,
   TerminateSy,
   ThenSy,
   TypeSy,
   UseSy,
   WhenSy,
   WhileSy,
   WithSy);

  subtype Opcode is Integer range -OMax .. +OMax;         -- -OMax..+OMax;  --
                                                          --opcode
  subtype Operand1 is Integer range -LMax .. +LMax;       -- -LMax..+LMax;  --
                                                          --operand
  subtype Operand2 is Integer range -NMax .. +NMax;       -- -NMax..+NMax;  --
                                                          --operand

  type Set is array (Integer range <>) of Boolean;
  function "+" (a, b : Set) return Set;

  type Symset is array (KeyWSymbol) of Boolean;
  function "+" (a, b : Symset) return Symset;
  function "+" (a : Symset; b : KeyWSymbol) return Symset;
  function "-" (a, b : Symset) return Symset;
  function "-" (a : Symset; b : KeyWSymbol) return Symset;
  Empty_Symset : constant Symset := (others => False);

  subtype Index is Integer range -XMax .. +XMax;

  subtype Alfa is String (1 .. Alng);

  Empty_Alfa : Alfa := (others => ' ');

  type aObject is (
   Konstant,
   Variable,
   TypeMark,
   Prozedure,
   Funktion,
   aTask,
   aEntry);

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

  type Item is record
    TYP : Types;
    Ref : Index;
  end record;

  subtype Neting_level is Integer range 0 .. LMax;

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
    LEV  : Neting_level;
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
    InXTYP : Types;         --  C Type of the index
    ELTYP  : Types;         --  C Type of the elements of the array
    ELREF  : Index;         --  C Pointer to an entry in ArraysTab if the
                            --    elements of the array are themselves arrays
    Size   : Index;         --  C Total size of the array
    Low    : Index;         --  Limits on the array index
    High   : Index;         --  Array [Low..High] OF ElTyp
    ELSize : Index;         --  Size of an element
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
    VSize: Index;     --  sum of PSize and length of area (3)
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

  type FilDescr is record
    Curr  : Integer;
    Kount : File_count;       --  short
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

  Err_Count : Integer;      --  (Marcelo) copied from PC source
  Scheduler : Integer;      --  (Marcelo) scheduler routine to use, choose
                            --from several

  TSlice : Integer;         --  Default Task time-slice in milliseconds
                            --  Feldman: 60ths of a sec on Mac

  ListingWasRequested : Boolean;   --  -l was specified in command line
  Debug               : Boolean;          --  Run-time program info on/off flag
  Map                 : Boolean;            --  Compile-time output of Global
                                            --VAR Map
  SNAP                : Boolean;            --  SNAP-shot flag To Display
                                            --sched. status
  RunningTime         : Boolean;    --  Display running timer

  Listing : File_Type;            --  File pointers

  CH : Character;  --  previous Character Read from Source program

  StanTyps : constant Typset :=
   Typset'(NOTYP | Ints | Floats | Bools | xChars => True, others => False);

  ProgramID : Alfa := (others => ' '); --  Main program name

  m, N : Integer;
  CMax : Integer := CDMax; -- := added 7-Dec-2009
  --  top of available ObjCode table;
  --  CMax+1..CDMax: var initialization code

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

  AdaKeyW    : array (1 .. NKW) of String (1 .. 10);  --  Array of Ada
                                                      --keywords in Order
  AdaKeyWSy  : array (1 .. NKW) of KeyWSymbol;  --  Corresponding keyword
                                                --symbols
  ArraysTab  : array (1 .. AMax) of ATabEntry;  --  Array table
  BlockTab   : array (0 .. BMax) of BTabEntry;  --  Block-table [7-Dec-2009:
                                                --was 1..]
  ObjCode    : array (0 .. CDMax) of Order;     --  Object Code table
  EntryTab   : array (0 .. EntryMax) of Index;  --  Entry Table
  FileIOTab  : FilDescr;                      --  File I/O table
  FloatPtTab : array (1 .. C2Max) of Float;     --  Float Constant table
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
  C2     : Integer;       --  Index To FloatPtTab
  Sx     : Integer;       --  Index To StringTab
  T      : Integer;       --  Index To IdTab
  TCount : Integer;       --  Index To TskDefTab
  ECount : Integer;       --  Index To EntryTAB

  type SSTBzz is array (Character'(' ') .. ']') of KeyWSymbol;
  SpecialSymbols : constant SSTBzz :=
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
    '&'    => And_Symbol, -- Mmmh !!
    others => NullSy);

  -- --- Error Control Variables ---

  subtype Error_set is Set (0 .. ERMax);
  Errs       : Error_set;       --  compilation Errors
  error_free : Error_set := (others => False);

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
  RNum           : Float;                     --  FLOAT Number from InSymbol
  SLeng          : Integer;                   --  String Length
  CharacterTypes : array (Character) of CHTP;   --  character types
  InpLine        : String (1 .. 255);             --  input line. Manuel:
                                                  --Renamed To InpLine
  CC             : Integer;                   --  character counter
  LC             : Integer;                   --  location counter
  LL             : Integer;                   --  Length of current line
  LineCount      : Integer;                   --  Source line counter, used
                                              --for listing
  Tx             : Integer;                   --  scratch Variable
  TCH            : Character;                      --  scratch Variable
  TStr           : String (1 .. 14);             --  scratch Variable

  -- --- Compiler symbol sets ---

  --  Constant definition begin symbol(S)

  ConstBegSys : constant Symset :=
   Symset'
   (Plus      |
    MinUS     |
    IntCon    |
    FloatCon  |
    CharCon   |
    IDent     => True,
    others => False);

  --  Type definition begin symbol(S)

  TypeBegSys : constant Symset :=
   Symset'
   (IDent     |
    ArraySy   |
    RecordSy  |
    RangeSy   |
    LParent   => True,
    others => False);

  --  Start of Block begin symbol(S)

  BlockBegSyS : constant Symset :=
   (ProcSy     |
    FuncSy     |
    TaskSy     |
    EntrySy    |
    BeginSy    |
    DeclareSy  => True,
    others => False);
  --  Start of Factor begin symbol

  Factor_Begin_Symbol : constant Symset :=
   (IntCon     |
    FloatCon   |
    CharCon    |
    IDent      |
    LParent    |
    Not_Symbol => True,
    others => False);

  --  Start of Statement begin symbol

  StatBegSys : constant Symset :=
   Symset'
   (IDent     |
    BeginSy   |
    IfSy      |
    WhileSy   |
    LoopSy    |
    ForSy     |
    CaseSy    |
    ExitSy    |
    NullSy    |
    ReturnSy  |
    SelectSy  |
    AcceptSy  |
    DelaySy   => True,

    others => False);

  --  Debugging Flags - these flags are used to print out information
  --  about the code status.  If a flag is true, then the section of
  --  code associated with it will print out extra information allowing
  --  for easier debugging.  Using conditional compilation, they are
  --  declared as Booleans or False constants depending on the qDebug
  --  compiler switch.

  qDebug : constant Boolean := True;

  procedure cICompiler;

  procedure cFeedback;

  -- Set current source stream (file, editor data, zipped file,...)
  type Stream_Access is access all Root_Stream_Type'Class;
  procedure c_Set_Stream (s : Stream_Access);

  --  Get the next line from source
  procedure cGetNextLine (InpLine : out String; Last : out Natural);

  --  No more input left
  function cEndOfSource return Boolean;

  procedure cFoundError
   (errCode, srcNumber, charStart, charEnd, objNumber : Integer);

end HAC.Data;
