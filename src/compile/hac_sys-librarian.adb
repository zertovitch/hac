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

with HAL;

with Ada.Characters.Handling;

package body HAC_Sys.Librarian is

  procedure Register_Unit (
    LD        : in out Li_Defs.Library_Data;
    Full_Name : in     String;
    Kind      : in     Li_Defs.Unit_Kind;
    Is_New    :    out Boolean
  )
  is
    use Li_Defs, Li_Defs.Library_Name_Mapping;
    VFN : constant HAL.VString := HAL.To_VString (Full_Name);
  begin
    Is_New := LD.Map.Find (VFN) = No_Element;
    if Is_New then
      declare
        New_Unit : Library_Unit;
      begin
        New_Unit.Status := Void;
        New_Unit.Kind   := Kind;
        LD.Library.Append (New_Unit);
        LD.Map.Insert (VFN, LD.Library.Last_Index);
      end;
    end if;
  end Register_Unit;

  procedure Register_Built_In (LD : in out Li_Defs.Library_Data) is
    Is_New : Boolean;
  begin
    Register_Unit (LD, "Standard", Li_Defs.Package_Unit, Is_New);
    Register_Unit (LD, Defs.HAL_Name, Li_Defs.Package_Unit, Is_New);
  end Register_Built_In;

  ----------------------
  --  Built-in units  --
  ----------------------

  procedure Enter_Built_In (
    CD             : in out Co_Defs.Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Co_Defs.Entity_Kind;
    Base_Type      : in     Defs.Typen;
    Size           : in     Integer;
    Discrete_First : in     Defs.HAC_Integer := 0;
    Discrete_Last  : in     Defs.HAC_Integer := 0
  )
  is
    use Ada.Characters.Handling, Defs;
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
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural
  )
  is
    use Co_Defs, Defs, Parser.Enter_Def, UErrors;
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

  procedure Apply_WITH_Standard (CD : in out Co_Defs.Compiler_Data) is
    use Co_Defs, Defs;
    procedure Enter_Std_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In (CD, "Standard." & Name, TypeMark, T, 1, First, Last);
    end Enter_Std_Typ;
  begin
      Enter_Built_In (CD, "", Variable, NOTYP, 0);  --  Unreachable Id with invalid Link.
      --
      Enter_Built_In (CD, "Standard", Paquetage, NOTYP, 0);
      --
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
      Enter_Std_Typ ("Natural",  Ints, 0, HAC_Integer'Last);
      Enter_Std_Typ ("Positive", Ints, 1, HAC_Integer'Last);
      Enter_Std_Typ ("Duration", Durations, 0, 0);
  end Apply_WITH_Standard;

  procedure Apply_WITH_HAL (CD : in out Co_Defs.Compiler_Data) is
    use Co_Defs, Defs;

    procedure Enter_HAL_Typ (Name : String; T : Typen; First, Last : HAC_Integer) is
    begin
      Enter_Built_In (CD, HAL_Name & '.' & Name, TypeMark, T, 1, First, Last);
    end Enter_HAL_Typ;

    procedure Enter_HAL_Funct (Name : String; T : Typen; Code : PCode.SF_Code) is
    begin
      Enter_Built_In (CD, HAL_Name & '.' & Name, Funktion_Intrinsic, T, PCode.SF_Code'Pos (Code));
    end Enter_HAL_Funct;

    procedure Enter_HAL_Proc (Name : String; Code : PCode.SP_Code) is
    begin
      Enter_Built_In (CD, HAL_Name & '.' & Name, Prozedure_Intrinsic, NOTYP, PCode.SP_Code'Pos (Code));
    end Enter_HAL_Proc;

    use PCode;
  begin
      Enter_Built_In (CD, HAL_Name, Paquetage, NOTYP, 0);
      --
      Enter_HAL_Typ ("File_Type", Text_Files, 0, 0);  --  2020.05.17
      Enter_HAL_Typ ("Semaphore", Ints, 0, 0);
      Enter_HAL_Typ ("Time",      Times, 0, 0);
      Enter_HAL_Typ ("VString",   VStrings, 0, 0);    --  2020.05.02
      --
      --  Standard functions
      --
      Enter_HAL_Funct ("Chr",                 Chars,  SF_T_Val);    --  S'Val : RM 3.5.5 (5)
      Enter_HAL_Funct ("Ord",                 Ints,   SF_T_Pos);    --  S'Pos : RM 3.5.5 (2)
      Enter_HAL_Funct ("Succ",                Chars,  SF_T_Succ);   --  S'Succ : RM 3.5 (22)
      Enter_HAL_Funct ("Pred",                Chars,  SF_T_Pred);   --  S'Pred : RM 3.5 (25)
      Enter_HAL_Funct ("Round",               Ints,   SF_Round_Float_to_Int);
      Enter_HAL_Funct ("Trunc",               Ints,   SF_Trunc_Float_to_Int);
      Enter_HAL_Funct ("Sin",                 Floats, SF_Sin);
      Enter_HAL_Funct ("Cos",                 Floats, SF_Cos);
      Enter_HAL_Funct ("Exp",                 Floats, SF_Exp);
      Enter_HAL_Funct ("Log",                 Floats, SF_Log);
      Enter_HAL_Funct ("Sqrt",                Floats, SF_Sqrt);
      Enter_HAL_Funct ("Arctan",              Floats, SF_Arctan);
      Enter_HAL_Funct ("End_Of_File",         Bools,  SF_EOF);
      Enter_HAL_Funct ("End_Of_Line",         Bools,  SF_EOLN);
      Enter_HAL_Funct ("Rand",                Ints,   SF_Random_Int);
      Enter_HAL_Funct ("Rnd",                 Floats, SF_Random_Float);
      Enter_HAL_Funct ("Clock",               Times,  SF_Clock);
      --
      Enter_HAL_Funct ("Element",             Chars,    SF_Element);
      Enter_HAL_Funct ("Index",               Ints,     SF_Index);
      Enter_HAL_Funct ("Index_Backward",      Ints,     SF_Index_Backward);
      Enter_HAL_Funct ("Length",              Ints,     SF_Length);
      Enter_HAL_Funct ("Slice",               VStrings, SF_Slice);
      Enter_HAL_Funct ("To_Lower",            Chars,    SF_To_Lower_Char);
      Enter_HAL_Funct ("To_Upper",            Chars,    SF_To_Upper_Char);
      Enter_HAL_Funct ("To_VString",          VStrings, SF_Literal_to_VString);
      --
      Enter_HAL_Funct ("Trim_Left",           VStrings, SF_Trim_Left);
      Enter_HAL_Funct ("Trim_Right",          VStrings, SF_Trim_Right);
      Enter_HAL_Funct ("Trim_Both",           VStrings, SF_Trim_Both);
      --
      Enter_HAL_Funct ("Head",                VStrings, SF_Head);
      Enter_HAL_Funct ("Tail",                VStrings, SF_Tail);
      Enter_HAL_Funct ("Starts_With",         Bools,    SF_Starts_With);
      Enter_HAL_Funct ("Ends_With",           Bools,    SF_Ends_With);
      --
      --  Ada.Calendar-like functions
      --
      Enter_HAL_Funct ("Year",                Ints,      SF_Year);
      Enter_HAL_Funct ("Month",               Ints,      SF_Month);
      Enter_HAL_Funct ("Day",                 Ints,      SF_Day);
      Enter_HAL_Funct ("Seconds",             Durations, SF_Seconds);
      --
      Enter_HAL_Funct ("Image",               VStrings, SF_Image_Ints);
      Enter_HAL_Funct ("Image_Attribute",     VStrings, SF_Image_Attribute_Floats);
      Enter_HAL_Funct ("Integer_Value",       Ints,     SF_Integer_Value);
      Enter_HAL_Funct ("Float_Value",         Floats,   SF_Float_Value);
      --
      Enter_HAL_Funct ("Argument_Count",      Ints,     SF_Argument_Count);
      Enter_HAL_Funct ("Argument",            VStrings, SF_Argument);
      Enter_HAL_Funct ("Command_Name",        VStrings, SF_Command_Name);
      Enter_HAL_Funct ("Get_Env",             VStrings, SF_Get_Env);
      Enter_HAL_Funct ("Directory_Separator", Chars,    SF_Directory_Separator);
      --
      --  Ada.Directories-like functions
      --
      Enter_HAL_Funct ("Current_Directory",   VStrings, SF_Current_Directory);
      Enter_HAL_Funct ("Directory_Exists",    Bools,    SF_Directory_Exists);
      Enter_HAL_Funct ("Exists",              Bools,    SF_Exists);
      Enter_HAL_Funct ("File_Exists",         Bools,    SF_File_Exists);
      --
      Enter_HAL_Funct ("Get_Needs_Skip_Line", Bools, SF_Get_Needs_Skip_Line);
      --
      --  Ada.Text_IO-like procedures
      --
      Enter_HAL_Proc ("Create",         SP_Create);
      Enter_HAL_Proc ("Open",           SP_Open);
      Enter_HAL_Proc ("Append",         SP_Append);
      Enter_HAL_Proc ("Close",          SP_Close);
      Enter_HAL_Proc ("Get",            SP_Get);
      Enter_HAL_Proc ("Get_Immediate",  SP_Get_Immediate);
      Enter_HAL_Proc ("Get_Line",       SP_Get_Line);
      Enter_HAL_Proc ("Skip_Line",      SP_Skip_Line);
      Enter_HAL_Proc ("Put",            SP_Put);
      Enter_HAL_Proc ("Put_Line",       SP_Put_Line);
      Enter_HAL_Proc ("New_Line",       SP_New_Line);
      --
      --  Ada.Environment_Variables-like procedures
      --
      Enter_HAL_Proc ("Set_Env",        SP_Set_Env);
      --
      --  Ada.Directories-like procedures
      --
      Enter_HAL_Proc ("Copy_File ",     SP_Copy_File);
      Enter_HAL_Proc ("Delete_File ",   SP_Delete_File);
      Enter_HAL_Proc ("Rename ",        SP_Rename);
      Enter_HAL_Proc ("Set_Directory ", SP_Set_Directory);
      --
      Enter_HAL_Proc ("Shell_Execute",   SP_Shell_Execute_with_Result);
      Enter_HAL_Proc ("Set_Exit_Status", SP_Set_Exit_Status);
      --
      --  Tasking related (from SmallAda)
      --
      Enter_HAL_Proc ("Wait",           SP_Wait);
      Enter_HAL_Proc ("Signal",         SP_Signal);
      Enter_HAL_Proc ("Quantum",        SP_Quantum);
      Enter_HAL_Proc ("Priority",       SP_Priority);
      Enter_HAL_Proc ("InheritP",       SP_InheritP);
  end Apply_WITH_HAL;

end HAC_Sys.Librarian;