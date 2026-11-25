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
     HAC_Sys.Enter_Def,
     HAC_Sys.Parser.Packages;

with HAT;
with Interfaces;

package body HAC_Sys.Librarian.Built_In_Packages is

  procedure Enter_and_Register_Built_In_Package
    (CD   : in out Co_Defs.Compiler_Data;
     LD   : in out Library_Data;
     name : in     String)
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
    Enter_Library_Level_Def (CD, name, paquetage, NOTYP, 0, is_built_in => True);
    Parser.Packages.Feed_Packages_Table (CD);
    --  Feed library:
    unit.id_index := CD.Id_Count;
    Register_Unit (LD, unit);
  end Enter_and_Register_Built_In_Package;

  procedure Enter_Built_In_Type
    (CD             : in out Co_Defs.Compiler_Data;
     full_ident     : in     String;  --  "Main", "Standard.False", ...
     base_type      : in     Defs.Typen;
     size           : in     Integer;
     discrete_first : in     Defs.HAC_Integer;
     discrete_last  : in     Defs.HAC_Integer)
  is
  begin
    Enter_Library_Level_Def
      (CD             => CD,
       full_ident     => full_ident,
       new_entity     => Co_Defs.type_mark,
       base_type      => base_type,
       size           => size,
       discrete_first => discrete_first,
       discrete_last  => discrete_last,
       is_built_in    => True);
  end Enter_Built_In_Type;

  procedure Enter_Built_In_Enumeration_Type
    (CD             : in out Co_Defs.Compiler_Data;
     full_ident     : in     String;
     enum_items     : in     Defs.Alfa_Array)
  is
    --  Adapted from HAC_Sys.Parser.Type_Def
    enum_count : Natural := 0;
    use Co_Defs, Defs;
    use type HAC_Integer;
  begin
    Enter_Built_In_Type
      (CD             => CD,
       full_ident     => full_ident,
       base_type      => Enums,
       size           => 1,
       discrete_first => 0,
       discrete_last  => enum_items'Length - 1);
    declare
      xTP : Co_Defs.Exact_Subtyp renames CD.id_table (CD.Id_Count).xtyp;
    begin
      xTP.Ref := CD.Id_Count;
      for item of enum_items loop
        enum_count := enum_count + 1;
        Enter_Library_Level_Def
          (CD             => CD,
           full_ident     => A2S (item),
           new_entity     => declared_number_or_enum_item,
           base_type      => xTP.TYP,
           size           => enum_count - 1,  --  RM 3.5.1 (7): position begins with 0.
           discrete_first => HAC_Integer (enum_count - 1),  --  Likely unused...
           discrete_last  => HAC_Integer (enum_count - 1),  --  Likely unused...
           is_built_in    => True);
        declare
          new_enum_item : Identifier_Table_Entry renames CD.id_table (CD.Id_Count);
        begin
          new_enum_item.xtyp          := xTP;
          new_enum_item.is_referenced := False;
        end;
      end loop;
    end;
  end Enter_Built_In_Enumeration_Type;

  --  One-dimensional built-in array.
  procedure Enter_Built_In_Array_Type
    (CD                    : in out Co_Defs.Compiler_Data;
     full_ident            : in     String;
     index_exact_subtyp    : in     Co_Defs.Exact_Subtyp;
     element_exact_subtyp  : in     Co_Defs.Exact_Subtyp;
     element_size          : in     Integer)
  is
    --  Adapted from HAC_Sys.Parser.Type_Def
    new_dim_size          : Defs.HAC_Integer;
    arr_tab_ref, arr_size : Integer;
    use type Defs.HAC_Integer;
  begin
    Enter_Built_In_Type
      (CD             => CD,
       full_ident     => full_ident,
       base_type      => Defs.Arrays,
       size           => 1,  --  Adapted later.
       discrete_first => 0,
       discrete_last  => 0);

    declare
      array_id_entry : Co_Defs.Identifier_Table_Entry renames CD.id_table (CD.Id_Count);
    begin
      Enter_Def.Enter_Array (CD, index_exact_subtyp);
      arr_tab_ref := CD.Arrays_Count;
      new_dim_size := index_exact_subtyp.Discrete_Last - index_exact_subtyp.Discrete_First + 1;
      arr_size := Integer (Defs.HAC_Integer'Max (0, new_dim_size)) * element_size;

      declare
        new_a : Co_Defs.Array_Table_Entry renames CD.Arrays_Table (arr_tab_ref);
      begin
        --  new_a.Index_xTyp already set by Enter_Array.
        new_a.Array_Size   := arr_size;
        new_a.Element_xTyp := element_exact_subtyp;
        new_a.Element_Size := element_size;
        new_a.dimensions   := 1;
      end;

      array_id_entry.xtyp.Ref  := arr_tab_ref;
      array_id_entry.adr_or_sz := Defs.HAC_Integer (arr_size);
    end;
  end Enter_Built_In_Array_Type;

  procedure Define_and_Register_Standard
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Library_Data)
  is
    use Co_Defs, Defs;

    procedure Enter_Std_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In_Type (CD, "Standard." & Name, T, 1, First, Last);
    end Enter_Std_Typ;

  begin
    Enter_Library_Level_Def (CD, "", constant_object, NOTYP, 0);  --  Unreachable Id with invalid Link.
    --
    Enter_and_Register_Built_In_Package (CD, LD, "Standard");
    --
    Enter_Library_Level_Def (CD, "Standard.False", declared_number_or_enum_item, Bools, 0, is_built_in => True);
    Enter_Library_Level_Def (CD, "Standard.True",  declared_number_or_enum_item, Bools, 1, is_built_in => True);
    --
    Enter_Std_Typ ("Character",      Chars, 0, 255);
    Enter_Std_Typ ("Boolean",        Bools, 0, 1);
    CD.built_in.std_bool_index := CD.Id_Count;
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

  procedure Define_and_Register_Interfaces
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Library_Data)
  is
    use Defs, Interfaces;

    procedure Enter_Interfaces_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In_Type (CD, "Interfaces." & Name, T, 1, First, Last);
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

  procedure Define_and_Register_HAT
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Library_Data)
  is
    use Co_Defs, Defs;

    procedure Enter_HAT_Const (Name : String; Value : HAC_Float) is
      Float_Index : Integer;
    begin
      Compiler.PCode_Emit.Enter_or_find_Float (CD, Value, Float_Index);
      Enter_Library_Level_Def
        (CD, HAT_Name & '.' & Name,
         declared_number_or_enum_item, Floats, Float_Index, is_built_in => True);
    end Enter_HAT_Const;

    procedure Enter_HAT_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In_Type (CD, HAT_Name & '.' & Name, T, 1, First, Last);
    end Enter_HAT_Typ;

    procedure Enter_HAT_Enumeration_Type (name : in String; enum_items : Defs.Alfa_Array) is
      hat_enum_items : Defs.Alfa_Array (enum_items'Range);
      use type Defs.Alfa;
    begin
      for i in enum_items'Range loop
        hat_enum_items (i) := S2A (HAT_Name & '.') & enum_items (i);
      end loop;
      Enter_Built_In_Enumeration_Type
        (CD,
         HAT_Name & '.' & name,
         hat_enum_items);
    end Enter_HAT_Enumeration_Type;

    procedure Enter_HAT_Func (Name : String; T : Typen; Code : Defs.SF_Code) is
    begin
      Enter_Library_Level_Def
        (CD,
         HAT_Name & '.' & Name,
         funktion_intrinsic,
         T,
         Defs.SF_Code'Pos (Code),
         is_built_in => True);
    end Enter_HAT_Func;

    procedure Enter_HAT_Proc (Name : String; Code : Defs.SP_Code) is
    begin
      Enter_Library_Level_Def
        (CD,
         HAT_Name & '.' & Name,
         prozedure_intrinsic,
         NOTYP,
         Defs.SP_Code'Pos (Code),
         is_built_in => True);
    end Enter_HAT_Proc;

  begin
    Enter_and_Register_Built_In_Package (CD, LD, HAT_Name);
    --
    Enter_HAT_Typ ("File_Type",            Text_Files, 0, 0);             --  2020.05.17
    Enter_HAT_Typ (HAC_Float_Name,         Floats, 0, 0);                 --  Moved from Std 2021.12.26
    Enter_HAT_Typ ("Semaphore",            Ints, 0, 0);
    Enter_HAT_Typ ("Time",                 Times, 0, 0);
    Enter_HAT_Typ ("VString",              VStrings, 0, 0);               --  2020.05.02
    Enter_HAT_Typ ("File_Type",            Text_Files, 0, 0);             --  2020.05.17

    --  Ada.Directories-like types:
    Enter_HAT_Typ ("Directory_Entry_Type", Directory_Entry_Types, 0, 0);  --  2026.10.20
    Enter_HAT_Typ ("Search_Type", Search_Types, 0, 0);                    --  2026.10.20

    Enter_HAT_Enumeration_Type  --  2026.11.22
      ("File_Kind",
       (S2A ("Directory"),
        S2A ("Ordinary_File"),
        S2A ("Special_File")));
    CD.built_in.hat_file_kind_index :=  CD.Id_Count;

    --  Create: "type Filter_Type is array (HAT.File_Kind) of Boolean;" 2026.11.23
    Enter_Built_In_Array_Type
      (CD                   => CD,
       full_ident           => "HAT.Filter_Type",
       index_exact_subtyp   => CD.id_table (CD.built_in.hat_file_kind_index).xtyp,
       element_exact_subtyp => CD.id_table (CD.built_in.std_bool_index).xtyp,
       element_size         => 1);
    CD.built_in.hat_filter_type_index := CD.Id_Count;

    --
    --  "Standard" functions
    --
    Enter_HAT_Func ("Chr",                 Chars,  SF_T_Val);    --  S'Val : RM 3.5.5 (5)
    Enter_HAT_Func ("Ord",                 Ints,   SF_T_Pos);    --  S'Pos : RM 3.5.5 (2)
    Enter_HAT_Func ("Succ",                Chars,  SF_T_Succ);   --  S'Succ : RM 3.5 (22)
    Enter_HAT_Func ("Pred",                Chars,  SF_T_Pred);   --  S'Pred : RM 3.5 (25)
    --
    Enter_HAT_Func ("Round",               Ints,   SF_Round_Float_to_Int);
    Enter_HAT_Func ("Trunc",               Ints,   SF_Trunc_Float_to_Int);
    --
    Enter_HAT_Func ("Min",                 Ints,   SF_Min_Int);  --  Overloaded for floats
    Enter_HAT_Func ("Max",                 Ints,   SF_Max_Int);  --  Overloaded for floats
    --
    Enter_HAT_Const ("Pi",                 HAT.Pi);
    Enter_HAT_Func ("Sin",                 Floats, SF_Sin);
    Enter_HAT_Func ("Cos",                 Floats, SF_Cos);
    Enter_HAT_Func ("Exp",                 Floats, SF_Exp);
    Enter_HAT_Func ("Log",                 Floats, SF_Log);
    Enter_HAT_Func ("Sqrt",                Floats, SF_Sqrt);
    Enter_HAT_Func ("Arctan",              Floats, SF_Arctan);
    Enter_HAT_Func ("Sgn",                 Ints,   SF_Sgn_Int);  --  Overloaded for floats
    --
    Enter_HAT_Func ("Rand",                Ints,   SF_Random_Int);
    Enter_HAT_Func ("Rnd",                 Floats, SF_Random_Float);
    Enter_HAT_Proc  ("Randomize",          SP_Randomize);
    Enter_HAT_Proc  ("Random_Seed",        SP_Random_Seed);
    Enter_HAT_Func ("Clock",               Times,  SF_Clock);
    --
    Enter_HAT_Func ("Null_VString",        VStrings,            SF_Null_VString);
    Enter_HAT_Func ("Element",             Chars,               SF_Element);
    Enter_HAT_Func ("Index",               Ints,                SF_Index);
    Enter_HAT_Func ("Index_Backward",      Ints,                SF_Index_Backward);
    Enter_HAT_Func ("Length",              Ints,                SF_Length);
    Enter_HAT_Func ("Slice",               VStrings,            SF_Slice);
    Enter_HAT_Func ("To_Lower",            Chars,               SF_To_Lower_Char);
    Enter_HAT_Func ("To_Upper",            Chars,               SF_To_Upper_Char);
    Enter_HAT_Func ("To_VString",          VStrings,            SF_String_Literal_to_VString);
    Enter_HAT_Func ("To_String",           Strings_as_VStrings, SF_VString_to_String);
    --
    Enter_HAT_Func ("Trim_Left",           VStrings, SF_Trim_Left);
    Enter_HAT_Func ("Trim_Right",          VStrings, SF_Trim_Right);
    Enter_HAT_Func ("Trim_Both",           VStrings, SF_Trim_Both);
    --
    Enter_HAT_Func ("Head",                VStrings, SF_Head);
    Enter_HAT_Func ("Head_Before_Match",   VStrings, SF_Head_Before_Match);
    Enter_HAT_Func ("Tail",                VStrings, SF_Tail);
    Enter_HAT_Func ("Tail_After_Match",    VStrings, SF_Tail_After_Match);
    Enter_HAT_Func ("Starts_With",         Bools,    SF_Starts_With);
    Enter_HAT_Func ("Ends_With",           Bools,    SF_Ends_With);
    --
    --  Ada.Calendar-like functions
    --
    Enter_HAT_Func ("Year",                Ints,      SF_Year);
    Enter_HAT_Func ("Month",               Ints,      SF_Month);
    Enter_HAT_Func ("Day",                 Ints,      SF_Day);
    Enter_HAT_Func ("Seconds",             Durations, SF_Seconds);
    --
    --  Attribute-like functions
    --
    Enter_HAT_Func ("Image",               VStrings, SF_Image_Ints);
    Enter_HAT_Func ("Integer_Value",       Ints,     SF_Integer_Value);
    Enter_HAT_Func ("Float_Value",         Floats,   SF_Float_Value);
    --
    --  Ada.Command_Line & Ada.Environment_Variables - like functions
    --
    Enter_HAT_Func ("Argument_Count",      Ints,     SF_Argument_Count);
    Enter_HAT_Func ("Argument",            VStrings, SF_Argument);
    Enter_HAT_Func ("Command_Name",        VStrings, SF_Command_Name);
    Enter_HAT_Func ("Get_Env",             VStrings, SF_Get_Env);
    Enter_HAT_Func ("Get_VM_Variable",     VStrings, SF_Get_VM_Variable);
    --
    --  Ada.Directories-like functions
    --
    Enter_HAT_Func ("Current_Directory",   VStrings, SF_Current_Directory);
    Enter_HAT_Func ("Directory_Exists",    Bools,    SF_Directory_Exists);
    Enter_HAT_Func ("Exists",              Bools,    SF_Exists);
    Enter_HAT_Func ("File_Exists",         Bools,    SF_File_Exists);
    --  This one is *not* in Ada.Directories:
    Enter_HAT_Func ("Directory_Separator", Chars,    SF_Directory_Separator);
    --
    Enter_HAT_Func ("Get_Needs_Skip_Line", Bools, SF_Get_Needs_Skip_Line);
    --
    --  Ada.Text_IO-like subprograms
    --
    Enter_HAT_Proc ("Create",        SP_Create);
    Enter_HAT_Proc ("Open",          SP_Open);
    Enter_HAT_Proc ("Append",        SP_Append);
    Enter_HAT_Proc ("Close",         SP_Close);
    Enter_HAT_Proc ("Get",           SP_Get);
    Enter_HAT_Proc ("Get_Immediate", SP_Get_Immediate);
    Enter_HAT_Proc ("Get_Line",      SP_Get_Line);
    Enter_HAT_Proc ("Skip_Line",     SP_Skip_Line);
    Enter_HAT_Proc ("Put",           SP_Put);
    Enter_HAT_Proc ("Put_Line",      SP_Put_Line);
    Enter_HAT_Proc ("New_Line",      SP_New_Line);
    Enter_HAT_Func ("End_Of_File",   Bools, SF_EOF);
    Enter_HAT_Func ("End_Of_Line",   Bools, SF_EOLN);
    Enter_HAT_Func ("Is_Open",       Bools, SF_Is_Open);
    --
    --  Ada.Environment_Variables-like procedures
    --
    Enter_HAT_Proc ("Set_Env",         SP_Set_Env);
    Enter_HAT_Proc ("Set_VM_Variable", SP_Set_VM_Variable);
    --
    --  Ada.Directories-like subprograms
    --
    Enter_HAT_Proc ("Copy_File",        SP_Copy_File);
    Enter_HAT_Proc ("Create_Directory", SP_Create_Directory);
    Enter_HAT_Proc ("Create_Path",      SP_Create_Path);
    Enter_HAT_Proc ("Delete_Directory", SP_Delete_Directory);
    Enter_HAT_Proc ("Delete_File",      SP_Delete_File);
    Enter_HAT_Proc ("Rename",           SP_Rename);
    Enter_HAT_Proc ("Set_Directory",    SP_Set_Directory);
    Enter_HAT_Proc ("Start_Search",     SP_Start_Search);
    Enter_HAT_Proc ("End_Search",       SP_End_Search);
    Enter_HAT_Func ("More_Entries",     Bools, SF_More_Entries);
    Enter_HAT_Proc ("Get_Next_Entry",   SP_Get_Next_Entry);
    Enter_HAT_Func ("Simple_Name",      VStrings, SF_Simple_Name_Dir_Entry);
    Enter_HAT_Func ("Full_Name",        VStrings, SF_Full_Name_Dir_Entry);
    Enter_HAT_Func ("Kind",             Enums, SF_Full_Name_Dir_Entry);  --  !! specific enum !!
    Enter_HAT_Func ("Size",             Ints,  SF_Size_Dir_Entry);  --  Integer_64 in hat.ads

    Enter_HAT_Proc ("Shell_Execute",   SP_Shell_Execute_with_Result);
    Enter_HAT_Proc ("Set_Exit_Status", SP_Set_Exit_Status);
    --
    --  Ada.Strings.Unbounded-like procedures
    --
    Enter_HAT_Proc ("Delete", SP_Delete);
    --
    --  Tasking related (from SmallAda)
    --
    Enter_HAT_Proc ("Wait",     SP_Wait);
    Enter_HAT_Proc ("Signal",   SP_Signal);
    Enter_HAT_Proc ("Quantum",  SP_Quantum);
    Enter_HAT_Proc ("Priority", SP_Priority);
    Enter_HAT_Proc ("InheritP", SP_InheritP);
    --
    CD.Packages_Table (CD.Packages_Count).last_public_declaration := CD.Id_Count;
  end Define_and_Register_HAT;

end HAC_Sys.Librarian.Built_In_Packages;
