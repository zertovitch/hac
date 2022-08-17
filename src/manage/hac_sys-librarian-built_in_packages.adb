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

with HAC_Sys.Defs,
     HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Packages,
     HAC_Sys.PCode;

with HAT;
with Interfaces;

package body HAC_Sys.Librarian.Built_In_Packages is

  procedure Enter_and_Register_Built_In_Package (
    CD   : in out Co_Defs.Compiler_Data;
    LD   : in out Library_Data;
    name : in     String
  )
  is
    use Co_Defs, Defs;
    unit : Library_Unit :=
        (full_name     => HAT.To_VString (name),
         kind          => Package_Declaration,
         status        => Done,
         id_index      => No_Id,
         id_body_index => No_Id,
         spec_context  => Co_Defs.Id_Maps.Empty_Map);
  begin
    Enter_Library_Level_Def (CD, name, Paquetage, NOTYP, 0);
    Parser.Packages.Feed_Packages_Table (CD);
    --  Feed library:
    unit.id_index := CD.Id_Count;
    Register_Unit (LD, unit);
  end Enter_and_Register_Built_In_Package;

  procedure Define_and_Register_Standard (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  )
  is
    use Co_Defs, Defs;
    procedure Enter_Std_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Library_Level_Def (CD, "Standard." & Name, TypeMark, T, 1, First, Last);
    end Enter_Std_Typ;
  begin
    Enter_Library_Level_Def (CD, "", Variable, NOTYP, 0);  --  Unreachable Id with invalid Link.
    --
    Enter_and_Register_Built_In_Package (CD, LD, "Standard");
    --
    Enter_Library_Level_Def (CD, "Standard.False", Declared_Number_or_Enum_Item, Bools, 0);
    Enter_Library_Level_Def (CD, "Standard.True",  Declared_Number_or_Enum_Item, Bools, 1);
    --
    Enter_Std_Typ ("Character",      Chars, 0, 255);
    Enter_Std_Typ ("Boolean",        Bools, 0, 1);
    Enter_Std_Typ (HAC_Integer_Name, Ints, HAC_Integer'First, HAC_Integer'Last);
    --
    --  The "String" type identifier is treated separately in the Type_Definition parser
    --  and returns a constrained array of Character.
    --  Here we just reserve the "String" identifier at library level, with a bogus base,
    --  type String_Literals, which is actually used only for string literals like "abcd".
    Enter_Std_Typ ("String",         String_Literals, 0, 0);
    CD.String_Id_Index := CD.Id_Count;
    --
    Enter_Std_Typ ("Natural",  Ints, 0, HAC_Integer'Last);
    Enter_Std_Typ ("Positive", Ints, 1, HAC_Integer'Last);
    Enter_Std_Typ ("Duration", Durations, 0, 0);
    --
    CD.Packages_Table (CD.Packages_Count).last_public_declaration := CD.Id_Count;
  end Define_and_Register_Standard;

  procedure Define_and_Register_Interfaces (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  )
  is
    use Co_Defs, Defs, Interfaces;
    procedure Enter_Interfaces_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Library_Level_Def (CD, "Interfaces." & Name, TypeMark, T, 1, First, Last);
    end Enter_Interfaces_Typ;
  begin
    Enter_and_Register_Built_In_Package (CD, LD, "Interfaces");
    --
    Enter_Interfaces_Typ ("Integer_64", Ints, -2**63, 2**63 - 1);
    Enter_Interfaces_Typ ("Integer_32", Ints, -2**31, 2**31 - 1);
    Enter_Interfaces_Typ ("Integer_16", Ints, -2**15, 2**15 - 1);
    Enter_Interfaces_Typ ("Integer_8",  Ints, -2**7,  2**7  - 1);
    --
    CD.Packages_Table (CD.Packages_Count).last_public_declaration := CD.Id_Count;
  end Define_and_Register_Interfaces;

  procedure Define_and_Register_HAT (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  )
  is
    use Co_Defs, Defs, PCode;

    procedure Enter_HAT_Const (Name : String; Value : HAC_Float) is
      Float_Index : Integer;
    begin
      Compiler.PCode_Emit.Enter_or_find_Float (CD, Value, Float_Index);
      Enter_Library_Level_Def
        (CD, HAT_Name & '.' & Name,
         Declared_Number_or_Enum_Item, Floats, Float_Index);
    end Enter_HAT_Const;

    procedure Enter_HAT_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Library_Level_Def (CD, HAT_Name & '.' & Name, TypeMark, T, 1, First, Last);
    end Enter_HAT_Typ;

    procedure Enter_HAT_Funct (Name : String; T : Typen; Code : PCode.SF_Code) is
    begin
      Enter_Library_Level_Def (CD, HAT_Name & '.' & Name, Funktion_Intrinsic, T, PCode.SF_Code'Pos (Code));
    end Enter_HAT_Funct;

    procedure Enter_HAT_Proc (Name : String; Code : PCode.SP_Code) is
    begin
      Enter_Library_Level_Def (CD, HAT_Name & '.' & Name, Prozedure_Intrinsic, NOTYP, PCode.SP_Code'Pos (Code));
    end Enter_HAT_Proc;

  begin
    Enter_and_Register_Built_In_Package (CD, LD, HAT_Name);
    --
    Enter_HAT_Typ ("File_Type",    Text_Files, 0, 0);  --  2020.05.17
    Enter_HAT_Typ (HAC_Float_Name, Floats, 0, 0);      --  Moved from Std 2021.12.26
    Enter_HAT_Typ ("Semaphore",    Ints, 0, 0);
    Enter_HAT_Typ ("Time",         Times, 0, 0);
    Enter_HAT_Typ ("VString",      VStrings, 0, 0);    --  2020.05.02
    --
    --  Standard functions
    --
    Enter_HAT_Funct ("Chr",                 Chars,  SF_T_Val);    --  S'Val : RM 3.5.5 (5)
    Enter_HAT_Funct ("Ord",                 Ints,   SF_T_Pos);    --  S'Pos : RM 3.5.5 (2)
    Enter_HAT_Funct ("Succ",                Chars,  SF_T_Succ);   --  S'Succ : RM 3.5 (22)
    Enter_HAT_Funct ("Pred",                Chars,  SF_T_Pred);   --  S'Pred : RM 3.5 (25)
    --
    Enter_HAT_Funct ("Round",               Ints,   SF_Round_Float_to_Int);
    Enter_HAT_Funct ("Trunc",               Ints,   SF_Trunc_Float_to_Int);
    --
    Enter_HAT_Funct ("Min",                 Ints,   SF_Min_Int);  --  Overloaded for float
    Enter_HAT_Funct ("Max",                 Ints,   SF_Max_Int);  --  Overloaded for float
    --
    Enter_HAT_Const ("Pi",                  HAT.Pi);
    Enter_HAT_Funct ("Sin",                 Floats, SF_Sin);
    Enter_HAT_Funct ("Cos",                 Floats, SF_Cos);
    Enter_HAT_Funct ("Exp",                 Floats, SF_Exp);
    Enter_HAT_Funct ("Log",                 Floats, SF_Log);
    Enter_HAT_Funct ("Sqrt",                Floats, SF_Sqrt);
    Enter_HAT_Funct ("Arctan",              Floats, SF_Arctan);
    --
    Enter_HAT_Funct ("Rand",                Ints,   SF_Random_Int);
    Enter_HAT_Funct ("Rnd",                 Floats, SF_Random_Float);
    Enter_HAT_Funct ("Clock",               Times,  SF_Clock);
    --
    Enter_HAT_Funct ("Null_VString",        VStrings,            SF_Null_VString);
    Enter_HAT_Funct ("Element",             Chars,               SF_Element);
    Enter_HAT_Funct ("Index",               Ints,                SF_Index);
    Enter_HAT_Funct ("Index_Backward",      Ints,                SF_Index_Backward);
    Enter_HAT_Funct ("Length",              Ints,                SF_Length);
    Enter_HAT_Funct ("Slice",               VStrings,            SF_Slice);
    Enter_HAT_Funct ("To_Lower",            Chars,               SF_To_Lower_Char);
    Enter_HAT_Funct ("To_Upper",            Chars,               SF_To_Upper_Char);
    Enter_HAT_Funct ("To_VString",          VStrings,            SF_Literal_to_VString);
    Enter_HAT_Funct ("To_String",           Strings_as_VStrings, SF_VString_to_String);
    --
    Enter_HAT_Funct ("Trim_Left",           VStrings, SF_Trim_Left);
    Enter_HAT_Funct ("Trim_Right",          VStrings, SF_Trim_Right);
    Enter_HAT_Funct ("Trim_Both",           VStrings, SF_Trim_Both);
    --
    Enter_HAT_Funct ("Head",                VStrings, SF_Head);
    Enter_HAT_Funct ("Head_Before_Match",   VStrings, SF_Head_Before_Match);
    Enter_HAT_Funct ("Tail",                VStrings, SF_Tail);
    Enter_HAT_Funct ("Tail_After_Match",    VStrings, SF_Tail_After_Match);
    Enter_HAT_Funct ("Starts_With",         Bools,    SF_Starts_With);
    Enter_HAT_Funct ("Ends_With",           Bools,    SF_Ends_With);
    --
    --  Ada.Calendar-like functions
    --
    Enter_HAT_Funct ("Year",                Ints,      SF_Year);
    Enter_HAT_Funct ("Month",               Ints,      SF_Month);
    Enter_HAT_Funct ("Day",                 Ints,      SF_Day);
    Enter_HAT_Funct ("Seconds",             Durations, SF_Seconds);
    --
    --  Attribute-like functions
    --
    Enter_HAT_Funct ("Image",               VStrings, SF_Image_Ints);
    Enter_HAT_Funct ("Integer_Value",       Ints,     SF_Integer_Value);
    Enter_HAT_Funct ("Float_Value",         Floats,   SF_Float_Value);
    --
    --  Ada.Command_Line & Ada.Environment_Variables - like functions
    --
    Enter_HAT_Funct ("Argument_Count",      Ints,     SF_Argument_Count);
    Enter_HAT_Funct ("Argument",            VStrings, SF_Argument);
    Enter_HAT_Funct ("Command_Name",        VStrings, SF_Command_Name);
    Enter_HAT_Funct ("Get_Env",             VStrings, SF_Get_Env);
    Enter_HAT_Funct ("Get_VM_Variable",     VStrings, SF_Get_VM_Variable);
    --
    --  Ada.Directories-like functions
    --
    Enter_HAT_Funct ("Current_Directory",   VStrings, SF_Current_Directory);
    Enter_HAT_Funct ("Directory_Exists",    Bools,    SF_Directory_Exists);
    Enter_HAT_Funct ("Exists",              Bools,    SF_Exists);
    Enter_HAT_Funct ("File_Exists",         Bools,    SF_File_Exists);
    --  This one is *not* in Ada.Directories:
    Enter_HAT_Funct ("Directory_Separator", Chars,    SF_Directory_Separator);
    --
    Enter_HAT_Funct ("Get_Needs_Skip_Line", Bools, SF_Get_Needs_Skip_Line);
    --
    --  Ada.Text_IO-like subprograms
    --
    Enter_HAT_Proc ("Create",         SP_Create);
    Enter_HAT_Proc ("Open",           SP_Open);
    Enter_HAT_Proc ("Append",         SP_Append);
    Enter_HAT_Proc ("Close",          SP_Close);
    Enter_HAT_Proc ("Get",            SP_Get);
    Enter_HAT_Proc ("Get_Immediate",  SP_Get_Immediate);
    Enter_HAT_Proc ("Get_Line",       SP_Get_Line);
    Enter_HAT_Proc ("Skip_Line",      SP_Skip_Line);
    Enter_HAT_Proc ("Put",            SP_Put);
    Enter_HAT_Proc ("Put_Line",       SP_Put_Line);
    Enter_HAT_Proc ("New_Line",       SP_New_Line);
    Enter_HAT_Funct ("End_Of_File",   Bools, SF_EOF);
    Enter_HAT_Funct ("End_Of_Line",   Bools, SF_EOLN);
    Enter_HAT_Funct ("Is_Open",       Bools, SF_Is_Open);
    --
    --  Ada.Environment_Variables-like procedures
    --
    Enter_HAT_Proc ("Set_Env",         SP_Set_Env);
    Enter_HAT_Proc ("Set_VM_Variable", SP_Set_VM_Variable);
    --
    --  Ada.Directories-like procedures
    --
    Enter_HAT_Proc ("Copy_File",      SP_Copy_File);
    Enter_HAT_Proc ("Delete_File",    SP_Delete_File);
    Enter_HAT_Proc ("Rename",         SP_Rename);
    Enter_HAT_Proc ("Set_Directory",  SP_Set_Directory);
    --
    Enter_HAT_Proc ("Shell_Execute",   SP_Shell_Execute_with_Result);
    Enter_HAT_Proc ("Set_Exit_Status", SP_Set_Exit_Status);
    --
    --  Tasking related (from SmallAda)
    --
    Enter_HAT_Proc ("Wait",           SP_Wait);
    Enter_HAT_Proc ("Signal",         SP_Signal);
    Enter_HAT_Proc ("Quantum",        SP_Quantum);
    Enter_HAT_Proc ("Priority",       SP_Priority);
    Enter_HAT_Proc ("InheritP",       SP_InheritP);
    --
    CD.Packages_Table (CD.Packages_Count).last_public_declaration := CD.Id_Count;
  end Define_and_Register_HAT;

end HAC_Sys.Librarian.Built_In_Packages;
