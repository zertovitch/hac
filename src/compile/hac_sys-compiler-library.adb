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

with HAC_Sys.Parser.Enter_Def,
     HAC_Sys.PCode,
     HAC_Sys.UErrors;

with Ada.Characters.Handling;

package body HAC_Sys.Compiler.Library is

  procedure Register_Unit (
    CD        : in out Compiler_Data;
    Full_Name : in     String;
    Kind      : in     Unit_Kind;
    Is_New    :    out Boolean
  )
  is
    use Library_Name_Mapping;
    VFN : constant VString := To_VString (Full_Name);
  begin
    Is_New := CD.lib_map.Find (VFN) = No_Element;
    if Is_New then
      declare
        New_Unit : Library_Unit;
      begin
        New_Unit.Status := Void;
        New_Unit.Kind   := Kind;
        CD.lib_map.Insert (VFN, New_Unit);
      end;
    end if;
  end Register_Unit;

  ----------------------
  --  Built-in units  --
  ----------------------

  procedure Enter_Built_In (
    CD             : in out Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Entity_Kind;
    Base_Type      : in     Typen;
    Size           : in     Integer;
    Discrete_First : in     HAC_Integer := 0;
    Discrete_Last  : in     HAC_Integer := 0
  )
  is
    use Ada.Characters.Handling;
    Alfa_Ident       : constant Alfa := To_Alfa (Full_Ident);
    Alfa_Ident_Upper : constant Alfa := To_Alfa (To_Upper (Full_Ident));
  begin
    CD.Id_Count            := CD.Id_Count + 1;  --  Enter standard identifier
    CD.IdTab (CD.Id_Count) :=
     (
      Name           => Alfa_Ident_Upper,
      Name_with_case => Alfa_Ident,
      Link           => CD.Id_Count - 1,
      Entity         => New_Entity,
      Read_only      => True,
      xTyp           => (TYP => Base_Type, Ref => 0),
      Block_Ref      => 0,
      Normal         => True,
      LEV            => 0,
      Adr_or_Sz      => Size,
      Discrete_First => Discrete_First,
      Discrete_Last  => Discrete_Last
    );
    CD.Blocks_Table (0).Last_Id_Idx := CD.Id_Count;
  end Enter_Built_In;

  procedure Apply_USE_Clause (
    CD       : in out Compiler_Data;
    Level    : in     Nesting_level;
    Pkg_Idx  : in     Natural
  )
  is
    use Parser.Enter_Def, UErrors;
    Pkg_UName     : constant String := To_String (CD.IdTab (Pkg_Idx).Name);
    Pkg_UName_Dot : constant String := Pkg_UName & '.';
    Pkg_Initial   : constant Character := Pkg_UName (Pkg_UName'First);
  begin
    pragma Assert (Pkg_Idx /= No_Id);
    if CD.IdTab (Pkg_Idx).Entity /= Paquetage then
      Error (CD, err_syntax_error, "Package name expected", True);
    end if;
    --  The specification begins immediately after the package name.
    for i in Pkg_Idx + 1 .. CD.Id_Count loop
      exit when Initial (CD.IdTab (i).Name) /= Pkg_Initial;
      declare
        Full_UName : constant String := To_String (CD.IdTab (i).Name);
        Full_Name  : String (Full_UName'Range);
        Start : Positive;
      begin
        if Full_UName'Length > Pkg_UName_Dot'Length
          and then Full_UName (Full_UName'First .. Full_UName'First - 1 + Pkg_UName_Dot'Length) =
                   Pkg_UName_Dot
        then
          --  We have spotted, e.g., "STANDARD.FALSE" with the matching prefix "STANDARD."
          Start := Full_UName'First + Pkg_UName_Dot'Length;
          Full_Name := To_String (CD.IdTab (i).Name_with_case);
          --  Here we enter, e.g. the "FALSE", "False" pair.
          Enter (CD, Level,
            To_Alfa (Full_UName (Start .. Full_UName'Last)),
            To_Alfa (Full_Name (Start .. Full_Name'Last)),
            Alias
          );
          CD.IdTab (CD.Id_Count).Adr_or_Sz := i;  --  i = Aliased entity's index.
        else
          exit;  --  We have left the public part of the specification.
        end if;
      end;
    end loop;
  end Apply_USE_Clause;

  procedure Apply_WITH_Standard (CD : in out Compiler_Data) is
    procedure Enter_Std_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In (CD, "Standard." & Name, TypeMark, T, 1, First, Last);
    end Enter_Std_Typ;
    Is_New : Boolean;
  begin
    Register_Unit (CD, "Standard", Package_Unit, Is_New);
    --
    if Is_New then
      Enter_Built_In (CD, "", Variable, NOTYP, 0);  --  Unreachable Id with invalid Link.
      --
      Enter_Built_In (CD, "Standard", Paquetage, NOTYP, 0);
      Enter_Built_In (CD, "Standard.False", Declared_Number_or_Enum_Item, Bools, 0);
      Enter_Built_In (CD, "Standard.True",  Declared_Number_or_Enum_Item, Bools, 1);
      --
      Enter_Std_Typ (HAC_Float_Name,   Floats, 0, 0);
      Enter_Std_Typ ("Character",      Chars, 0, 255);
      Enter_Std_Typ ("Boolean",        Bools, 0, 1);
      Enter_Std_Typ (HAC_Integer_Name, Ints, HAC_Integer'First, HAC_Integer'Last);
      --
      --  The "String" type identifier is treated separately in the Type_Definition parser
      --  and returns a constrained array of Character.
      --  Here we just reserve the "String" identifier at level 0, with a bogus base type,
      --  String_Literals, which is actually used only for string literals like "abcd".
      Enter_Std_Typ ("String",         String_Literals, 0, 0);
      CD.String_Id_Index := CD.Id_Count;
      --
      Enter_Std_Typ ("VString",        VStrings, 0, 0);    --  2020.05.02
      Enter_Std_Typ ("File_Type",      Text_Files, 0, 0);  --  2020.05.17
      Enter_Std_Typ ("Natural",        Ints, 0, HAC_Integer'Last);
      Enter_Std_Typ ("Positive",       Ints, 1, HAC_Integer'Last);
      Enter_Std_Typ ("Time",           Times, 0, 0);
      Enter_Std_Typ ("Duration",       Durations, 0, 0);
    end if;
  end Apply_WITH_Standard;

  procedure Apply_WITH_HAC_Pack (CD : in out Compiler_Data) is

    procedure Enter_Std_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In (CD, "HAC_Pack." & Name, TypeMark, T, 1, First, Last);
    end Enter_Std_Typ;

    procedure Enter_Std_Funct (Name : String; T : Typen; Code : PCode.SF_Code) is
    begin
      Enter_Built_In (CD, "HAC_Pack." & Name, Funktion_Intrinsic, T, PCode.SF_Code'Pos (Code));
    end Enter_Std_Funct;

    procedure Enter_Std_Proc (Name : String; Code : PCode.SP_Code) is
    begin
      Enter_Built_In (CD, "HAC_Pack." & Name, Prozedure_Intrinsic, NOTYP, PCode.SP_Code'Pos (Code));
    end Enter_Std_Proc;

    Is_New : Boolean;
    use PCode;
  begin
    Register_Unit (CD, HAC_Pack_Name, Package_Unit, Is_New);
    --
    if Is_New then
      Enter_Built_In (CD, HAC_Pack_Name, Paquetage, NOTYP, 0);
      --
      Enter_Std_Typ ("Semaphore", Ints, 0, 0);
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
      --  Ada.Text_IO-like procedures
      --
      Enter_Std_Proc ("Create",         SP_Create);
      Enter_Std_Proc ("Open",           SP_Open);
      Enter_Std_Proc ("Append",         SP_Append);
      Enter_Std_Proc ("Close",          SP_Close);
      Enter_Std_Proc ("Get",            SP_Get);
      Enter_Std_Proc ("Get_Immediate",  SP_Get_Immediate);
      Enter_Std_Proc ("Get_Line",       SP_Get_Line);
      Enter_Std_Proc ("Skip_Line",      SP_Skip_Line);
      Enter_Std_Proc ("Put",            SP_Put);
      Enter_Std_Proc ("Put_Line",       SP_Put_Line);
      Enter_Std_Proc ("New_Line",       SP_New_Line);
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
      --  Tasking related (from SmallAda)
      --
      Enter_Std_Proc ("Wait",           SP_Wait);
      Enter_Std_Proc ("Signal",         SP_Signal);
      Enter_Std_Proc ("Quantum",        SP_Quantum);
      Enter_Std_Proc ("Priority",       SP_Priority);
      Enter_Std_Proc ("InheritP",       SP_InheritP);
    end if;
  end Apply_WITH_HAC_Pack;

end HAC_Sys.Compiler.Library;
