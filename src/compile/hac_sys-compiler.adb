with HAC_Sys.Compiler.PCode_Emit;
with HAC_Sys.UErrors;                       use HAC_Sys.UErrors;
with HAC_Sys.Parser;                        use HAC_Sys.Parser;
with HAC_Sys.Parser.Helpers;
with HAC_Sys.PCode;
with HAC_Sys.Scanner;                       use HAC_Sys.Scanner;

with Ada.Integer_Text_IO, Ada.Characters.Handling, Ada.Strings.Fixed, Ada.Text_IO;

package body HAC_Sys.Compiler is

  use VStrings_Pkg;

  procedure Set_Source_Stream (
    CD         : in out Compiler_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;       --  Can be a virtual name (editor title, zip entry)
    start_line : in     Natural := 0  --  We could have a shebang or other Ada sources before
  )
  is
  begin
    CD.compiler_stream  := Source_Stream_Access (s);
    CD.source_file_name := To_VString (file_name);
    CD.Line_Count       := start_line;
  end Set_Source_Stream;

  function Get_Current_Source_Name (CD : Compiler_Data) return String is
  begin
    return Defs.To_String (CD.source_file_name);
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
    CD.Strings_Table_Top := CD.Strings_Constants_Table'First;
    --  Tasks, Entries
    CD.Tasks_Definitions_Count := 0;
    CD.Entries_Count := 0;
    --  Location Counter (in output code)
    CD.LC   := 0;
    CD.CMax := CDMax;
    --  Current block name for debugging of HAC programs.
    CD.Full_Block_Id := Universe;
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
    --
    CD.Err_Count := 0;
    CD.Errs      := error_free;
  end Init;

  --  Print_Tables is for debugging purposes.
  --
  procedure Print_Tables (CD : in Compiler_Data) is
    use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Fixed;
    --
    procedure Show_Padded (n : String; t : Positive) is
    begin
      Put (CD.comp_dump, "  " & n & Integer'Max (0, t - n'Length) * ' ');
    end Show_Padded;
  begin
    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump,
       " Identifiers" & (Alng - 6) * ' ' & "Link  Object                        " &
       "TYP              Ref  Norm Lvl  Adr"
    );
    Put_Line (CD.comp_dump,
       (Alng + aObject'Width + Typen'Width + Boolean'Width + 29) * '-'
    );
    --  We list all definitions, starting
    --  from Main (last Id of the "zero block" / standard).
    --
    for I in CD.Blocks_Table (0).Last_Id_Idx .. CD.Id_Count loop
      declare
        r : IdTabEntry renames CD.IdTab (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (To_String (r.Name_with_case), Alng);
        Put (CD.comp_dump, r.Link, 4);
        Show_Padded (aObject'Image (r.Obj), aObject'Width);
        Show_Padded (Typen'Image (r.xTyp.TYP), Typen'Width);
        Put (CD.comp_dump, r.xTyp.Ref, 5);
        Show_Padded (Boolean'Image (r.Normal), Boolean'Width);
        Put (CD.comp_dump, Integer (r.LEV), 3);
        Put (CD.comp_dump, r.Adr_or_Sz, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, " Tasks       Block#");
    for I in 0 .. CD.Tasks_Definitions_Count loop
      Put (CD.comp_dump, I, 4);
      Put (CD.comp_dump, ' ');
      Put (CD.comp_dump, To_String (CD.IdTab (CD.Tasks_Definitions_Table (I)).Name) & "  ");
      Put (CD.comp_dump, CD.IdTab (CD.Tasks_Definitions_Table (I)).Block_Ref);
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
               CD.Tasks_Definitions_Table (CD.IdTab (CD.Entries_Table (I)).Adr_or_Sz)
             ).Name)
        );
        New_Line (CD.comp_dump);
      end loop;
      New_Line (CD.comp_dump);
    end if;

    Put_Line (CD.comp_dump, " Blocks" & Alng * ' ' & "  Last_ID LPar PSze Vsze");
    --  There is a hidden block #0, "the Universe", with Standard
    for I in 0 .. CD.Blocks_Count loop
      declare
        r : BTabEntry renames CD.Blocks_Table (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (To_String (r.Id), Alng);
        Put (CD.comp_dump, r.Last_Id_Idx, 10);
        Put (CD.comp_dump, r.Last_Param_Id_Idx, 5);
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
          Put (CD.comp_dump, Typen'Image (r.Index_xTyp.TYP) & "   " &
                             Typen'Image (r.Element_xTyp.TYP));
          Put (CD.comp_dump, r.Element_xTyp.Ref, 5);
          Put (CD.comp_dump, r.Low, 5);
          Put (CD.comp_dump, r.High, 5);
          Put (CD.comp_dump, r.Element_Size, 5);
          Put (CD.comp_dump, r.Array_Size, 5);
          New_Line (CD.comp_dump);
        end;
      end loop;
    end if;
  end Print_Tables;

  ---------------------------------------------------------------------------

  procedure Compile (
    CD                 : in out Compiler_Data;
    asm_dump_file_name :        String  := "";  --  Assembler oputput of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  )
  is
    use PCode;

    procedure Enter_Standard_Functions_and_Main is

      procedure Enter_Std
       (X0    : String;
        X1    : aObject;
        X2    : Typen;
        Size  : Integer;
        First : HAC_Integer := 0;
        Last  : HAC_Integer := 0)
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
          xTyp           => (TYP => X2, Ref => 0),
          Block_Ref      => 0,
          Normal         => True,
          LEV            => 0,
          Adr_or_Sz      => Size,
          Discrete_First => First,
          Discrete_Last  => Last);
      end Enter_Std;

      procedure Enter_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
      begin
        Enter_Std (Name, TypeMark, T, 1, First, Last);
      end Enter_Typ;

      procedure Enter_Std_Funct (Name : String; T : Typen; Code : SF_Code) is
      begin
        Enter_Std (Name, Funktion, T, SF_Code'Pos (Code));
      end Enter_Std_Funct;

      procedure Enter_Std_Proc (Name : String; Code : SP_Code) is
      begin
        Enter_Std (Name, Prozedure, NOTYP, SP_Code'Pos (Code));
      end Enter_Std_Proc;

    begin
      Enter_Std ("",               Variable,        NOTYP, 0);
      --
      Enter_Std ("False",          Declared_Number_or_Enum_Item, Bools, 0);
      Enter_Std ("True",           Declared_Number_or_Enum_Item, Bools, 1);
      --
      Enter_Typ (HAC_Float_Name,   Floats, 0, 0);
      Enter_Typ ("Character",      Chars, 0, 255);
      Enter_Typ ("Boolean",        Bools, 0, 1);
      Enter_Typ (HAC_Integer_Name, Ints, HAC_Integer'First, HAC_Integer'Last);
      --
      --  The "String" type identifier is treated separately in the Type_Definition parser
      --  and returns a constrained array of Character.
      --  Here we just reserve the "String" identifier at level 0, with a bogus base type,
      --  String_Literals, which is actually used only for string literals like "abcd".
      Enter_Typ ("String",         String_Literals, 0, 0);
      CD.String_Id_Index := CD.Id_Count;
      --
      Enter_Typ ("SEMAPHORE",      Ints, 0, 0);
      Enter_Typ ("VString",        VStrings, 0, 0);    --  2020.05.02
      Enter_Typ ("File_Type",      Text_Files, 0, 0);  --  2020.05.17
      Enter_Typ ("Natural",        Ints, 0, HAC_Integer'Last);
      Enter_Typ ("Positive",       Ints, 1, HAC_Integer'Last);
      Enter_Typ ("Time",           Times, 0, 0);
      Enter_Typ ("Duration",       Durations, 0, 0);
      --
      --  Standard functions
      --
      Enter_Std_Funct ("Chr",                 Chars,  SF_T_Val);    --  S'Val : RM 3.5.5 (5)
      Enter_Std_Funct ("Ord",                 Ints,   SF_T_Pos);    --  S'Pos : RM 3.5.5 (2)
      Enter_Std_Funct ("Succ",                Chars,  SF_T_Succ);   --  S'Succ : RM 3.5 (22)
      Enter_Std_Funct ("Pred",                Chars,  SF_T_Pred);   --  S'Pred : RM 3.5 (25)
      Enter_Std_Funct ("Round",               Ints,   SF_Round_Float_to_Int);
      Enter_Std_Funct ("Trunc",               Ints,   SF_Trunc_Float_to_Int);
      Enter_Std_Funct ("Sin",                 Floats, SF_Sin);
      Enter_Std_Funct ("Cos",                 Floats, SF_Cos);
      Enter_Std_Funct ("Exp",                 Floats, SF_Exp);
      Enter_Std_Funct ("Log",                 Floats, SF_Log);
      Enter_Std_Funct ("Sqrt",                Floats, SF_Sqrt);
      Enter_Std_Funct ("Arctan",              Floats, SF_Arctan);
      Enter_Std_Funct ("End_Of_File",         Bools,  SF_EOF);
      Enter_Std_Funct ("End_Of_Line",         Bools,  SF_EOLN);
      Enter_Std_Funct ("Rand",                Ints,   SF_Random_Int);
      Enter_Std_Funct ("Rnd",                 Floats, SF_Random_Float);
      Enter_Std_Funct ("Clock",               Times,  SF_Clock);
      --
      Enter_Std_Funct ("Element",             Chars,    SF_Element);
      Enter_Std_Funct ("Index",               Ints,     SF_Index);
      Enter_Std_Funct ("Index_Backward",      Ints,     SF_Index_Backward);
      Enter_Std_Funct ("Length",              Ints,     SF_Length);
      Enter_Std_Funct ("Slice",               VStrings, SF_Slice);
      Enter_Std_Funct ("To_Lower",            Chars,    SF_To_Lower_Char);
      Enter_Std_Funct ("To_Upper",            Chars,    SF_To_Upper_Char);
      Enter_Std_Funct ("To_VString",          VStrings, SF_Literal_to_VString);
      --
      Enter_Std_Funct ("Trim_Left",           VStrings, SF_Trim_Left);
      Enter_Std_Funct ("Trim_Right",          VStrings, SF_Trim_Right);
      Enter_Std_Funct ("Trim_Both",           VStrings, SF_Trim_Both);
      --
      Enter_Std_Funct ("Head",                VStrings, SF_Head);
      Enter_Std_Funct ("Tail",                VStrings, SF_Tail);
      Enter_Std_Funct ("Starts_With",         Bools,    SF_Starts_With);
      Enter_Std_Funct ("Ends_With",           Bools,    SF_Ends_With);
      --
      --  Ada.Calendar-like functions
      --
      Enter_Std_Funct ("Year",                Ints,      SF_Year);
      Enter_Std_Funct ("Month",               Ints,      SF_Month);
      Enter_Std_Funct ("Day",                 Ints,      SF_Day);
      Enter_Std_Funct ("Seconds",             Durations, SF_Seconds);
      --
      Enter_Std_Funct ("Image",               VStrings, SF_Image_Ints);
      Enter_Std_Funct ("Image_Attribute",     VStrings, SF_Image_Attribute_Floats);
      Enter_Std_Funct ("Integer_Value",       Ints,     SF_Integer_Value);
      Enter_Std_Funct ("Float_Value",         Floats,   SF_Float_Value);
      --
      Enter_Std_Funct ("Argument_Count",      Ints,     SF_Argument_Count);
      Enter_Std_Funct ("Argument",            VStrings, SF_Argument);
      Enter_Std_Funct ("Command_Name",        VStrings, SF_Command_Name);
      Enter_Std_Funct ("Get_Env",             VStrings, SF_Get_Env);
      Enter_Std_Funct ("Directory_Separator", Chars,    SF_Directory_Separator);
      --
      --  Ada.Directories-like functions
      --
      Enter_Std_Funct ("Current_Directory",   VStrings, SF_Current_Directory);
      Enter_Std_Funct ("Directory_Exists",    Bools,    SF_Directory_Exists);
      Enter_Std_Funct ("Exists",              Bools,    SF_Exists);
      Enter_Std_Funct ("File_Exists",         Bools,    SF_File_Exists);
      --
      Enter_Std_Funct ("Get_Needs_Skip_Line", Bools, SF_Get_Needs_Skip_Line);
      --
      Enter_Std_Proc ("Get",            SP_Get);
      Enter_Std_Proc ("Get_Immediate",  SP_Get_Immediate);
      Enter_Std_Proc ("Get_Line",       SP_Get_Line);
      Enter_Std_Proc ("Skip_Line",      SP_Skip_Line);
      Enter_Std_Proc ("Put",            SP_Put);
      Enter_Std_Proc ("Put_Line",       SP_Put_Line);
      Enter_Std_Proc ("New_Line",       SP_New_Line);
      Enter_Std_Proc ("Wait",           SP_Wait);
      Enter_Std_Proc ("Signal",         SP_Signal);
      --
      --  Ada.Text_IO-like procedures
      --
      Enter_Std_Proc ("Create",         SP_Create);
      Enter_Std_Proc ("Open",           SP_Open);
      Enter_Std_Proc ("Append",         SP_Append);
      Enter_Std_Proc ("Close",          SP_Close);
      --
      Enter_Std_Proc ("Quantum",        SP_Quantum);
      Enter_Std_Proc ("Priority",       SP_Priority);
      Enter_Std_Proc ("InheritP",       SP_InheritP);
      --
      --  Ada.Environment_Variables-like procedures
      --
      Enter_Std_Proc ("Set_Env",        SP_Set_Env);
      --
      --  Ada.Directories-like procedures
      --
      Enter_Std_Proc ("Copy_File ",     SP_Copy_File);
      Enter_Std_Proc ("Delete_File ",   SP_Delete_File);
      Enter_Std_Proc ("Rename ",        SP_Rename);
      Enter_Std_Proc ("Set_Directory ", SP_Set_Directory);
      --
      Enter_Std_Proc ("Shell_Execute",   SP_Shell_Execute_with_Result);
      Enter_Std_Proc ("Set_Exit_Status", SP_Set_Exit_Status);
      --
      --  Enter Main.
      --
      Enter_Std (To_String (CD.Main_Program_ID),  Prozedure, NOTYP, 0);
      CD.Main_Proc_Id_Index := CD.Id_Count;
    end Enter_Standard_Functions_and_Main;

    use Ada.Text_IO, HAC_Sys.Parser.Helpers;

    asm_dump : File_Type;
    map_file : File_Type;

    procedure InSymbol is begin InSymbol (CD); end InSymbol;

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
    if CD.Sy /= WITH_Symbol then  --  WITH HAC_PACK;
      Error (CD, err_WITH_Small_Sp, "", stop => True);
    else
      InSymbol;
      if CD.Sy /= IDent or not Equal (CD.Id, "HAC_PACK") then
        Error (CD, err_WITH_Small_Sp, "", stop => True);
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
      Error (CD, err_use_Small_Sp, "");  --  USE HAC_PACK;
    else
      InSymbol;
      if CD.Sy /= IDent or not Equal (CD.Id, "HAC_PACK") then
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
     (Id                => To_Alfa ("-- Standard Definitions (The Universe)"),
      Last_Id_Idx       => CD.Id_Count,
      Last_Param_Id_Idx => 1,
      PSize             => 0,
      VSize             => 0,
      SrcFrom           => CD.Line_Count,
      SrcTo             => CD.Line_Count);
    CD.Display (0) := 0;  --  Added 7-Dec-2009

    CD.Tasks_Definitions_Table (0) := CD.Id_Count;  --  { Task Table Entry }

    --  Start Compiling
    Block (CD, Block_Begin_Symbol + Statement_Begin_Symbol,
           False, False, 1, CD.Id_Count,
           CD.IdTab (CD.Id_Count).Name, CD.Main_Program_ID_with_case);
    --  Main procedure is parsed.
    PCode_Emit.Emit (CD, k_Halt_Interpreter);

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
      for Tx in CD.Blocks_Table (0).Last_Id_Idx + 1 .. CD.Id_Count loop
        if CD.IdTab (Tx).Obj = Variable then
          if CD.IdTab (Tx).xTyp.TYP /= NOTYP then
            Ada.Integer_Text_IO.Put (map_file, CD.IdTab (Tx).Adr_or_Sz, 4);
            Put (map_file, To_String (CD.IdTab (Tx).Name) & "   ");
          end if;
          if CD.IdTab (Tx).LEV = 1 then
            Put (map_file, " Global(");
          else
            Put (map_file, " Local (");
          end if;
          Put (map_file, Nesting_level'Image (CD.IdTab (Tx).LEV));
          Put (map_file, ')');
          New_Line (map_file);
        end if;
      end loop;
      New_Line (map_file);
      Close (map_file);
    end if;

    if asm_dump_file_name /= "" then
      Create (asm_dump, Out_File, asm_dump_file_name);
      Dump (
        CD.ObjCode (CD.ObjCode'First .. CD.LC - 1),  --  Dump only compiled part.
        CD.Strings_Constants_Table,
        CD.Float_Constants_Table,
        asm_dump
      );
      Close (asm_dump);
    end if;

  exception
    when End_Error =>
      Error (CD, err_unexpected_end_of_text);
    when Compilation_abandoned =>
      null;  --  Just too many errors...
  end Compile;

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean is
  begin
    return CD.Err_Count = 0;
  end Unit_Compilation_Successful;

  function Unit_Object_Code_Size (CD : Compiler_Data) return Natural is
  begin
    return CD.LC;
  end Unit_Object_Code_Size;

  function Maximum_Object_Code_Size return Natural is
  begin
    return Defs.CDMax;
  end Maximum_Object_Code_Size;

end HAC_Sys.Compiler;
