with HAC.Data;                          use HAC.Data;
with HAC.UErrors;                       use HAC.UErrors;
with HAC.Parser;                        use HAC.Parser;
with HAC.Parser.Helpers;
with HAC.PCode;                         use HAC.PCode;
with HAC.Scanner;                       use HAC.Scanner;

with Ada.Integer_Text_IO, Ada.Characters.Handling;

package body HAC.Compiler is

  procedure Init_Tables (CD : out Compiler_Data) is
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
    CD.LC := 0;
    --  Current block name for debugging of HAC programs.
    CD.Block_Id_with_casing := To_Alfa ("[-- Nowhere --]");
    --
    --  Scanner data
    --
    CD.CH := ' ';
    CD.CC := 0;
    CD.LL := 0;
    CD.syStart := 1;
    CD.syEnd   := 1;
  end Init_Tables;

  --  Print_Tables is for debugging purposes.
  --
  procedure Print_Tables (CD : in Compiler_Data) is
    use Ada.Text_IO, Ada.Integer_Text_IO;
  begin
    New_Line;
    Put (" Identifiers          Link  Obj  TYP  Ref  NRM  LEV  Adr");
    New_Line;
    for I in CD.Blocks_Table (1).Last .. CD.Id_Count loop
      declare
        r : IdTabEntry renames CD.IdTab (I);
      begin
        Put (I, 4);
        Put (' ');
        Put (r.Name);
        Put (r.Link, 10);
        Put (aObject'Pos (r.Obj), 5);
        Put (Types'Pos (r.TYP), 5);
        Put (r.Ref, 5);
        Put (Boolean'Pos (r.Normal), 5);
        Put (r.LEV, 5);
        Put (r.Adr, 5);
        New_Line;
      end;
    end loop;

    New_Line;
    Put_Line (" Tasks       Block#");
    for I in 0 .. CD.Tasks_Definitions_Count loop
      Put (I, 4);
      Put (' ');
      Put (CD.IdTab (CD.Tasks_Definitions_Table (I)).Name);
      Put ("  ");
      Put (CD.IdTab (CD.Tasks_Definitions_Table (I)).Ref);
      New_Line;
    end loop;

    New_Line;

    if CD.Entries_Count > 0 then
      Put (" Entries ");
      New_Line;
      for I in 1 .. CD.Entries_Count loop
        Put (I, 4);
        Put (' ');
        Put (CD.IdTab (CD.Entries_Table (I)).Name);
        Put ("in Task ");
        Put (CD.IdTab (CD.Tasks_Definitions_Table (CD.IdTab (CD.Entries_Table (I)).Adr)).Name);
        New_Line;
      end loop;
      New_Line;
    end if;

    Put_Line (" Blocks               last LPar PSze Vsze");
    --  There is a hidden block #0, "the Universe", with Standard
    for I in 1 .. CD.Blocks_Count loop
      declare
        r : BTabEntry renames CD.Blocks_Table (I);
      begin
        Put (I, 4);
        Put (' ');
        Put (r.Id);
        Put (r.Last, 10);
        Put (r.LastPar, 5);
        Put (r.PSize, 5);
        Put (r.VSize, 5);
        New_Line;
      end;
    end loop;

    New_Line;

    if CD.Arrays_Count > 0 then
      Put_Line (" Arrays    Xtyp Etyp Eref  Low High ELSZ Size");
      for I in 1 .. CD.Arrays_Count loop
        declare
          r : ATabEntry renames CD.Arrays_Table (I);
        begin
          Put (I, 4);
          Put (Types'Pos (r.Index_TYP), 10);
          Put (Types'Pos (r.Element_TYP), 5);
          Put (r.ELREF, 5);
          Put (r.Low, 5);
          Put (r.High, 5);
          Put (r.ELSize, 5);
          Put (r.Size, 5);
          New_Line;
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
    asm_dump_file_name :        String := "";
    map                :        Boolean := False  -- !! as file name
  )
  is

    procedure Enter_Standard_Functions_and_Main is

      procedure Enter
       (X0 : String;
        X1 : aObject;
        X2 : Types;
        X3 : Integer)
      is
        X0A : Alfa := Empty_Alfa;
      begin
        X0A (1 .. X0'Length)   := X0;
        CD.Id_Count            := CD.Id_Count + 1;  --  Enter standard identifier
        CD.IdTab (CD.Id_Count) :=
         (
          Name           => Ada.Characters.Handling.To_Upper (X0A),
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
      Enter ("RANDOM",         Funktion, Ints,   SF_Random); --{ Schoening }
      Enter ("CLOCK",          Funktion, Floats, SF_Clock);  --{ Cramer }
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
      Enter (Main_Program_ID,  Prozedure, NOTYP, 0);
    end Enter_Standard_Functions_and_Main;

    use Ada.Text_IO, Ada.Integer_Text_IO, HAC.Parser.Helpers;

    asm_dump : File_Type;

    procedure InSymbol is begin InSymbol (CD); end;

  begin  --  Compile

    -- !! As options
    Listing_Was_Requested:= False;
    Debug:= False;

    Errs:= error_free;
    -- (MRC) Total error count, from PC version
    Err_Count := 0;

    Init_Tables (CD);

    if Listing_Was_Requested then
      Create (Listing, Name => "compiler.lst");
      Put_Line (Listing, Header);
    end if;

    if qDebug then
      Create (Sym_dump, Name => "symbols.lst");
      Put_Line ("Compiler: check for program heading");
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

    if qDebug then
      Put_Line ("Compiler: check for main procedure");
    end if;

    if CD.Sy /= PROCEDURE_Symbol then
      Error (CD, err_missing_a_procedure_declaration, ""); -- PROCEDURE Name IS
    else
      InSymbol;
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
      else
        Main_Program_ID           := CD.Id;
        Main_Program_ID_with_case := CD.Id_with_case;
        InSymbol;
      end if;
    end if;

    if qDebug then
      Put_Line ("Compiler: main procedure is " & Main_Program_ID);
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
           CD.IdTab (CD.Id_Count).Name, Main_Program_ID_with_case);
    --  Main procedure is parsed.
    Emit (CD, k_Halt_Interpreter);

    if CD.Sy /= Semicolon then
      if qDebug then
        Put_Line ("Compile terminated BEFORE FILE END");
      end if;
      if Listing_Was_Requested then
        Put_Line (Listing, "Compile terminated BEFORE FILE END");
      end if;
    end if;

    if CD.Blocks_Table (1).VSize > StMax - (STKINCR * CD.Tasks_Definitions_Count) then
      Error (CD, err_stack_size, "");
    end if;
    CD.Blocks_Table (1).SrcTo := CD.Line_Count;  --(* Manuel : terminate source *)

    if Listing_Was_Requested then
      Close (Listing);
    end if;
    if qDebug then
      Close (Sym_dump);
    end if;

    if qDebug and Debug then
      Print_Tables (CD);
    end if;
    if Errs /= error_free then
      ErrorMsg;
    --{Close(ErrFile);}
    --{halt;}
    elsif map then
      if qDebug then
        New_Line;
        Put_Line ("  -* Symbol Table *-");
        New_Line;
        Put_Line ("  LOC  Name       scope");
        Put_Line ("------------------------");
        New_Line;
        for Tx in CD.Blocks_Table (1).Last + 1 .. CD.Id_Count loop
          if CD.IdTab (Tx).Obj = Variable then
            if CD.IdTab (Tx).TYP /= NOTYP then
              Put (CD.IdTab (Tx).Adr, 4);
              Put (CD.IdTab (Tx).Name, Alng + 3);
            end if;
            if CD.IdTab (Tx).LEV = 1 then
              Put (" Global(");
            else
              Put (" Local (");
            end if;
            Put (CD.IdTab (Tx).LEV, 1);
            Put (')');
            New_Line;
          end if;
        end loop;
        New_Line;
      end if;
    end if;
    --{Close(ErrFile);}

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

end HAC.Compiler;
