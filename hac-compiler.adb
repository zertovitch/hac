with HAC.UErrors;                       use HAC.UErrors;
with HAC.Parser;                        use HAC.Parser;
with HAC.Parser.Helpers;
with HAC.PCode;                         use HAC.PCode;
with HAC.Scanner;                       use HAC.Scanner;

with Ada.Integer_Text_IO, Ada.Characters.Handling;

package body HAC.Compiler is

  use Ada.Strings.Unbounded;

  procedure Set_Source_Stream (
    CD        : in out Compiler_Data;
    s         : access Ada.Streams.Root_Stream_Type'Class;
    file_name :        String  --  Can be a virtual name (editor title, zip entry)
  )
  is
  begin
    CD.compiler_stream := Source_Stream_Access (s);
    CD.source_file_name := To_Unbounded_String (file_name);
  end Set_Source_Stream;

  function Get_Current_Source_Name (CD: Compiler_Data) return String is
  begin
    return To_String (CD.source_file_name);
  end Get_Current_Source_Name;

  procedure Set_Error_Pipe (
    CD   : in out Compiler_Data;
    pipe :        Smart_error_pipe
  )
  is
  begin
    CD.error_pipe := pipe;
  end Set_Error_Pipe;

  procedure Init (CD : out Compiler_Data) is
  begin
    --  Array and block tables are clearly 1-based
    CD.Arrays_Count := 0;
    CD.Blocks_Count := 0;
    CD.Float_Constants_Count := 0;
    --  Identifiers
    CD.Id_Count := 0;
    --  Strings
    CD.Strings_Table_Top := 0;
    --  Tasks, Entries
    CD.Tasks_Definitions_Count := 0;
    CD.Entries_Count := 0;
    --  Location Counter (in output code)
    CD.LC   := 0;
    CD.CMax := CDMax;
    --  Current block name for debugging of HAC programs.
    CD.Block_Id_with_casing := To_Alfa ("[-- Nowhere --]");
    --
    CD.Main_Program_ID           := Empty_Alfa;
    CD.Main_Program_ID_with_case := Empty_Alfa;
    --
    --  Scanner data
    --
    CD.CH := ' ';
    CD.CC := 0;
    CD.LL := 0;
    CD.syStart := 1;
    CD.syEnd   := 1;
    CD.Line_Count := 0;
    --
    CD.Err_Count := 0;
    CD.Errs      := error_free;
  end Init;

  --  Print_Tables is for debugging purposes.
  --
  procedure Print_Tables (CD : in Compiler_Data) is
    use Ada.Text_IO, Ada.Integer_Text_IO;
  begin
    New_Line (CD.comp_dump);
    Put (CD.comp_dump, " Identifiers          Link  Obj  TYP  Ref  NRM  LEV  Adr");
    New_Line (CD.comp_dump);
    for I in CD.Blocks_Table (1).Last .. CD.Id_Count loop
      declare
        r : IdTabEntry renames CD.IdTab (I);
      begin
        Put (CD.comp_dump, I, 4);
        Put (CD.comp_dump, ' ' & To_String (r.Name));
        Put (CD.comp_dump, r.Link, 10);
        Put (CD.comp_dump, aObject'Pos (r.Obj), 5);
        Put (CD.comp_dump, Types'Pos (r.TYP), 5);
        Put (CD.comp_dump, r.Ref, 5);
        Put (CD.comp_dump, Boolean'Pos (r.Normal), 5);
        Put (CD.comp_dump, r.LEV, 5);
        Put (CD.comp_dump, r.Adr, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, " Tasks       Block#");
    for I in 0 .. CD.Tasks_Definitions_Count loop
      Put (CD.comp_dump, I, 4);
      Put (CD.comp_dump, ' ');
      Put (CD.comp_dump, To_String (CD.IdTab (CD.Tasks_Definitions_Table (I)).Name) & "  ");
      Put (CD.comp_dump, CD.IdTab (CD.Tasks_Definitions_Table (I)).Ref);
      New_Line (CD.comp_dump);
    end loop;

    New_Line (CD.comp_dump);

    if CD.Entries_Count > 0 then
      Put (CD.comp_dump, " Entries ");
      New_Line (CD.comp_dump);
      for I in 1 .. CD.Entries_Count loop
        Put (CD.comp_dump, I, 4);
        Put (CD.comp_dump,
             ' ' & To_String (CD.IdTab (CD.Entries_Table (I)).Name) & " in Task " &
             To_String (CD.IdTab (
               CD.Tasks_Definitions_Table (CD.IdTab (CD.Entries_Table (I)).Adr)
             ).Name)
        );
        New_Line (CD.comp_dump);
      end loop;
      New_Line (CD.comp_dump);
    end if;

    Put_Line (CD.comp_dump, " Blocks               last LPar PSze Vsze");
    --  There is a hidden block #0, "the Universe", with Standard
    for I in 1 .. CD.Blocks_Count loop
      declare
        r : BTabEntry renames CD.Blocks_Table (I);
      begin
        Put (CD.comp_dump, I, 4);
        Put (CD.comp_dump, ' ' & To_String (r.Id));
        Put (CD.comp_dump, r.Last, 10);
        Put (CD.comp_dump, r.LastPar, 5);
        Put (CD.comp_dump, r.PSize, 5);
        Put (CD.comp_dump, r.VSize, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);

    if CD.Arrays_Count > 0 then
      Put_Line (CD.comp_dump, " Arrays    Xtyp Etyp Eref  Low High ELSZ Size");
      for I in 1 .. CD.Arrays_Count loop
        declare
          r : ATabEntry renames CD.Arrays_Table (I);
        begin
          Put (CD.comp_dump, I, 4);
          Put (CD.comp_dump, Types'Image (r.Index_TYP.TYP) & "   " &
                             Types'Image (r.Element_TYP.TYP));
          Put (CD.comp_dump, r.Element_TYP.Ref, 5);
          Put (CD.comp_dump, r.Low, 5);
          Put (CD.comp_dump, r.High, 5);
          Put (CD.comp_dump, r.ELSize, 5);
          Put (CD.comp_dump, r.Size, 5);
          New_Line (CD.comp_dump);
        end;
      end loop;
    end if;
  end Print_Tables;

  ---------------------------------------------------------------------------

  function Compiler_Data_to_Debug_Info (CD: Compiler_Data) return Debug_Info is
  begin
    return (Line  => CD.Line_Count,
            Block => CD.Block_Id_with_casing);
  end Compiler_Data_to_Debug_Info;

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode)
  is
  begin
    PCode.Emit (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT);
  end Emit;

  procedure Emit1 (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode;
    B    :        Integer)
  is
  begin
    PCode.Emit1 (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT, B);
  end Emit1;

  procedure Emit2 (
    CD   : in out Compiler_Data;
    FCT  :        HAC.PCode.Opcode;
    a, B :        Integer)
  is
  begin
    PCode.Emit2 (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT, a, B);
  end Emit2;

  procedure Emit_Comparison_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        HAC.Data.Comparison_Operator;
    Base_Type :        HAC.Data.Types
  )
  is
  begin
    if Base_Type = Floats then
      case Operator is
        when EQL => Emit (CD, k_EQL_Float);
        when NEQ => Emit (CD, k_NEQ_Float);
        when LSS => Emit (CD, k_LSS_Float);
        when LEQ => Emit (CD, k_LEQ_Float);
        when GTR => Emit (CD, k_GTR_Float);
        when GEQ => Emit (CD, k_GEQ_Float);
      end case;
    elsif Discrete_Typ (Base_Type) then
      case Operator is
        when EQL => Emit (CD, k_EQL_Integer);
        when NEQ => Emit (CD, k_NEQ_Integer);
        when LSS => Emit (CD, k_LSS_Integer);
        when LEQ => Emit (CD, k_LEQ_Integer);
        when GTR => Emit (CD, k_GTR_Integer);
        when GEQ => Emit (CD, k_GEQ_Integer);
      end case;
    else
      raise Internal_error with "Comparison instructions only for atomic types";
    end if;
  end Emit_Comparison_Instruction;

  procedure Emit_Unary_Minus (
    CD        : in out HAC.Compiler.Compiler_Data;
    Base_Type :        HAC.Data.Numeric_Typ
  )
  is
  begin
    case Base_Type is
      when Floats => Emit (CD, k_Unary_MINUS_Float);
      when Ints   => Emit (CD, k_Unary_MINUS_Integer);
    end case;
  end Emit_Unary_Minus;

  procedure Emit_Arithmetic_Binary_Instruction (
    CD        : in out HAC.Compiler.Compiler_Data;
    Operator  :        HAC.Data.Arithmetic_Binary_Operator;
    Base_Type :        HAC.Data.Numeric_Typ
  )
  is
  begin
    case Base_Type is
      when Floats =>
        case Operator is
          when Plus    => Emit (CD, k_ADD_Float);
          when Minus   => Emit (CD, k_SUBTRACT_Float);
          when Times   => Emit (CD, k_MULT_Float);
          when Divide  => Emit (CD, k_DIV_Float);
          when Power   => Emit (CD, k_Power_Float);
        end case;
      when Ints   =>
        case Operator is
          when Plus    => Emit (CD, k_ADD_Integer);
          when Minus   => Emit (CD, k_SUBTRACT_Integer);
          when Times   => Emit (CD, k_MULT_Integer);
          when Divide  => Emit (CD, k_DIV_Integer);
          when Power   => Emit (CD, k_Power_Integer);
        end case;
    end case;
  end Emit_Arithmetic_Binary_Instruction;

  procedure Compile (
    CD                 : in out Compiler_Data;
    asm_dump_file_name :        String  := "";  --  Assembler oputput of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  )
  is

    procedure Enter_Standard_Functions_and_Main is

      procedure Enter
       (X0 : String;
        X1 : aObject;
        X2 : Types;
        X3 : Integer)
      is
        X0A  : constant Alfa := To_Alfa (X0);
        X0AU : constant Alfa := To_Alfa (Ada.Characters.Handling.To_Upper (X0));
      begin
        CD.Id_Count            := CD.Id_Count + 1;  --  Enter standard identifier
        CD.IdTab (CD.Id_Count) :=
         (
          Name           => X0AU,
          Name_with_case => X0A,
          Link           => CD.Id_Count - 1,
          Obj            => X1,
          Read_only      => True,
          TYP            => X2,
          Ref            => 0,
          Normal         => True,
          LEV            => 0,
          Adr            => X3);
      end Enter;

    begin
      Enter ("",               Variable,        NOTYP, 0);
      --
      Enter ("False",          Declared_Number_or_Enum_Item, Bools, 0);
      Enter ("True",           Declared_Number_or_Enum_Item, Bools, 1);
      --
      Enter (HAC_Float_Name,   TypeMark, Floats, 1);
      Enter ("Character",      TypeMark, xChars, 1);
      Enter ("Boolean",        TypeMark, Bools, 1);
      Enter (HAC_Integer_Name, TypeMark, Ints, 1);
      Enter ("String",         TypeMark, Strings, 1);  --{ Hathorn }
      Enter ("SEMAPHORE",      TypeMark, Ints, 1);     --{ Hathorn }
      Enter ("TEXT",           TypeMark, Ints, 1);     --{ Schoening }
      --
      --  Standard functions
      --
      Enter ("abs",            Funktion, Floats, SF_Abs);     --  abs is an Ada keyword...
      Enter ("CHR",            Funktion, xChars, SF_T_Val);   --  S'Val : RM 3.5.5 (5)
      Enter ("ORD",            Funktion, Ints,   SF_T_Pos);   --  S'Pos : RM 3.5.5 (2)
      Enter ("SUCC",           Funktion, xChars, SF_T_Succ);  --  S'Succ : RM 3.5 (22)
      Enter ("PRED",           Funktion, xChars, SF_T_Pred);  --  S'Pred : RM 3.5 (25)
      Enter ("ROUND",          Funktion, Ints,   SF_Round_Float_to_Int);
      Enter ("TRUNC",          Funktion, Ints,   SF_Trunc_Float_to_Int);
      Enter ("Sin",            Funktion, Floats, SF_Sin);
      Enter ("Cos",            Funktion, Floats, SF_Cos);
      Enter ("Exp",            Funktion, Floats, SF_Exp);
      Enter ("Log",            Funktion, Floats, SF_Log);
      Enter ("Sqrt",           Funktion, Floats, SF_Sqrt);
      Enter ("Arctan",         Funktion, Floats, SF_Arctan);
      Enter ("EOF",            Funktion, Bools,  SF_EOF);
      Enter ("EOLN",           Funktion, Bools,  SF_EOLN);
      Enter ("RAND",           Funktion, Ints,   SF_Random_Int);    --{ Schoening }
      Enter ("RND",            Funktion, Floats, SF_Random_Float);
      Enter ("CLOCK",          Funktion, Floats, SF_Clock);         --{ Cramer }
      --{ Niladic functions such as CLOCK will have   }
      --{ IdTab[].Adr >= 100 To differentiate them from }
      --{ functions with args.  See Parser.StandFct.  }
      Enter ("Get       ",     Prozedure, NOTYP, 1);
      Enter ("Get_Line  ",     Prozedure, NOTYP, 2);
      Enter ("Put       ",     Prozedure, NOTYP, 3);
      Enter ("Put_Line  ",     Prozedure, NOTYP, 4);
      Enter ("New_Line  ",     Prozedure, NOTYP, 4); --{ Hathorn }
      Enter ("WAIT      ",     Prozedure, NOTYP, 5);
      Enter ("SIGNAL    ",     Prozedure, NOTYP, 6);
      Enter ("RESET     ",     Prozedure, NOTYP, 7); --{ Schoening }
      Enter ("REWRITE   ",     Prozedure, NOTYP, 8); --{ Schoening }
      Enter ("CLOSE     ",     Prozedure, NOTYP, 9); --{ Schoening }
      Enter ("CURSORAT  ",     Prozedure, NOTYP, 10); --{ Cramer }
      Enter ("QUANTUM   ",     Prozedure, NOTYP, 11); --{ Cramer }
      Enter ("PRIORITY  ",     Prozedure, NOTYP, 12); --{ Cramer }
      Enter ("INHERITP  ",     Prozedure, NOTYP, 13); --{ Cramer }
      Enter (To_String (CD.Main_Program_ID),  Prozedure, NOTYP, 0);
    end Enter_Standard_Functions_and_Main;

    use Ada.Text_IO, Ada.Integer_Text_IO, HAC.Parser.Helpers;

    asm_dump : File_Type;
    map_file : File_Type;

    procedure InSymbol is begin InSymbol (CD); end;

  begin  --  Compile
    Init (CD);

    CD.listing_requested := listing_file_name /= "";
    if CD.listing_requested then
      Create (CD.listing, Name => listing_file_name);
      Put_Line (CD.listing, Header);
    end if;

    CD.comp_dump_requested := cmp_dump_file_name /= "";
    if CD.comp_dump_requested then
      Create (CD.comp_dump, Name => cmp_dump_file_name);
      Put_Line (CD.comp_dump, "Compiler: check for program heading");
    end if;

    InSymbol;
    if CD.Sy /= WITH_Symbol then   -- WITH SMALL_SP;
      Error (CD, err_WITH_Small_Sp, "", stop_on_error => True);
    else
      InSymbol;
      if CD.Sy /= IDent or CD.Id(1..10) /= "HAC_PACK  " then
        Error (CD, err_WITH_Small_Sp, "", stop_on_error => True);
      else
        InSymbol;
        if CD.Sy /= Semicolon then
          Error (CD, err_semicolon_missing, "");
        else
          InSymbol;
        end if;
      end if;
    end if;

    if CD.Sy /= USE_Symbol then
      Error (CD, err_use_Small_Sp, ""); -- USE SMALL_SP;
    else
      InSymbol;
      if CD.Sy /= IDent or CD.Id(1..10) /= "HAC_PACK  " then
        Error (CD, err_use_Small_Sp, "");
      else
        InSymbol;
        if CD.Sy /= Semicolon then
          Error (CD, err_semicolon_missing, "");
        else
          InSymbol;
        end if;
      end if;
    end if;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, "Compiler: check for main procedure");
    end if;

    if CD.Sy /= PROCEDURE_Symbol then
      Error (CD, err_missing_a_procedure_declaration, ""); -- PROCEDURE Name IS
    else
      InSymbol;
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
      else
        CD.Main_Program_ID           := CD.Id;
        CD.Main_Program_ID_with_case := CD.Id_with_case;
        InSymbol;
      end if;
    end if;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, "Compiler: main procedure is " & To_String (CD.Main_Program_ID));
    end if;

    Enter_Standard_Functions_and_Main;  --  Enter Standard function id's and ProgramID

    CD.Blocks_Table (0) :=  --  Block Table Entry for Standard [was Main, 1]
     (Id      => To_Alfa ("Std Defns"),
      Last    => CD.Id_Count,
      LastPar => 1,
      PSize   => 0,
      VSize   => 0,
      SrcFrom => CD.Line_Count,
      SrcTo   => CD.Line_Count);
    CD.Display (0) := 0;  --  Added 7-Dec-2009

    CD.Tasks_Definitions_Table (0) := CD.Id_Count;  --  { Task Table Entry }

    --  Start Compiling
    Block (CD, Block_Begin_Symbol + Statement_Begin_Symbol,
           False, False, 1, CD.Id_Count,
           CD.IdTab (CD.Id_Count).Name, CD.Main_Program_ID_with_case);
    --  Main procedure is parsed.
    Emit (CD, k_Halt_Interpreter);

    if CD.Sy /= Semicolon then
      if CD.comp_dump_requested then
        Put_Line (CD.comp_dump, "Compile terminated BEFORE FILE END");
      end if;
      if CD.listing_requested then
        Put_Line (CD.listing, "Compile terminated BEFORE FILE END");
      end if;
    end if;

    if CD.Blocks_Table (1).VSize > StMax - (STKINCR * CD.Tasks_Definitions_Count) then
      Error (CD, err_stack_size, "");
    end if;
    CD.Blocks_Table (1).SrcTo := CD.Line_Count;  --(* Manuel : terminate source *)

    if CD.listing_requested then
      Close (CD.listing);
    end if;

    if CD.comp_dump_requested then
      Print_Tables (CD);
      Close (CD.comp_dump);
    end if;

    if CD.Errs /= error_free then
      Compilation_Errors_Summary (CD);
    elsif var_map_file_name /= "" then
      Create (map_file, Out_File, var_map_file_name);
      Put_Line (map_file, "  -* Symbol Table *-");
      New_Line (map_file);
      Put_Line (map_file, "  LOC  Name       scope");
      Put_Line (map_file, "------------------------");
      New_Line (map_file);
      for Tx in CD.Blocks_Table (1).Last + 1 .. CD.Id_Count loop
        if CD.IdTab (Tx).Obj = Variable then
          if CD.IdTab (Tx).TYP /= NOTYP then
            Put (map_file, CD.IdTab (Tx).Adr, 4);
            Put (map_file, To_String (CD.IdTab (Tx).Name) & "   ");
          end if;
          if CD.IdTab (Tx).LEV = 1 then
            Put (map_file, " Global(");
          else
            Put (map_file, " Local (");
          end if;
          Put (map_file, CD.IdTab (Tx).LEV, 1);
          Put (map_file, ')');
          New_Line (map_file);
        end if;
      end loop;
      New_Line (map_file);
      Close (map_file);
    end if;

    if asm_dump_file_name /= "" then
      Create (asm_dump, Out_File, asm_dump_file_name);
      Dump (CD.ObjCode (CD.ObjCode'First .. CD.LC - 1), asm_dump);
      Close (asm_dump);
    end if;

  exception
    when End_Error =>
      Error (CD, err_unexpected_end_of_text);
    when Compilation_abandoned =>
      null;  --  Just too many errors...
  end Compile;

  function Unit_Compilation_Successful (CD: Compiler_Data) return Boolean is
  begin
    return CD.Err_Count = 0;
  end Unit_Compilation_Successful;

end HAC.Compiler;
