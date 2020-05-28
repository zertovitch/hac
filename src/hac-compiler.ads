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

with HAC.Co_Defs, HAC.Defs, HAC.PCode;

with Ada.Streams, Ada.Text_IO;

package HAC.Compiler is

  use HAC.Co_Defs, HAC.Defs, HAC.PCode;

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
