with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner;

package body HAC_Sys.Parser.Standard_Functions is

  use Compiler.PCode_Emit, Defs, Expressions, Helpers, PCode, Scanner;

  SF_Args : constant array (SF_Code) of Natural :=
     (SF_Niladic            => 0,
      SF_Min_Max_Int |
      SF_Element |
      SF_Head | SF_Tail |
      SF_Head_Before_Match |
      SF_Tail_After_Match |
      SF_Starts_With |
      SF_Ends_With |
      SF_Int_Times_Char |
      SF_Int_Times_VStr     => 2,
      SF_Index |
      SF_Index_Backward |
      SF_Slice              => 3,
      others                => 1
     );

  procedure Standard_Function (
    CD          : in out Co_Defs.Compiler_Data;
    Level       :        Defs.Nesting_level;
    FSys        :        Defs.Symset;
    Ident_Index :        Integer;
    Code        :        PCode.SF_Code;
    Return_Typ  :    out Co_Defs.Exact_Subtyp
  )
  is
    use Co_Defs;
    Max_Args : constant := 3;
    Args : Natural := SF_Args (Code);
    Expected : array (1 .. Max_Args) of Typ_Set;       --  Expected type of the function's arguments
    Actual   : array (1 .. Max_Args) of Exact_Subtyp;  --  Actual type from argument expression
    Code_Adjusted : SF_Code := Code;
    do_SF_emit : Boolean := True;
    X : Exact_Subtyp;
    --
    procedure Prepare_Accepted_Parameter_Types is
      VString_or_Chars_Set     : constant Typ_Set := VStrings_Set or Chars_Set;
      Any_String_Set           : constant Typ_Set :=
        VStrings_Set or Arrays_Set or Str_Lit_Set or Str_as_VStr_Set;
      Any_String_or_Chars_Set  : constant Typ_Set := Any_String_Set or Chars_Set;
      Chars_or_Strings_Set     : constant Typ_Set := Chars_Set or Arrays_Set or Str_Lit_Set;
    begin
      case Code is
        when SF_Abs_Int =>
          Expected (1) := Numeric_Typ_Set;
        when SF_T_Val =>  --  S'Val : RM 3.5.5 (5)
          Expected (1) := Ints_Set;
        when SF_T_Pos =>  --  S'Pos : RM 3.5.5 (2)
          Expected (1) := Discrete_Typ;
        when SF_T_Succ | SF_T_Pred =>  -- S'Succ, S'Pred : RM 3.5 (22, 25)
          Expected (1) := Discrete_Typ;
        when SF_Round_Float_to_Int | SF_Trunc_Float_to_Int |
             SF_Sin | SF_Cos | SF_Exp | SF_Log | SF_Sqrt | SF_Arctan
          =>
          Expected (1) := Numeric_Typ_Set;
        when SF_Image_Ints =>
          Expected (1) := Numeric_Typ_Set or Times_Set or Durations_Set;
        when SF_Image_Attribute_Floats =>
          Expected (1) := Floats_Set;
        when SF_Random_Int | SF_Argument =>
          Expected (1) := Ints_Set;
        when SF_Min_Max_Int =>
          Expected (1 .. 2) := (Numeric_Typ_Set, Numeric_Typ_Set);
        when SF_Element | SF_Head | SF_Tail =>
          Expected (1 .. 2) := (VStrings_Set, Ints_Set);
        when SF_Length |
             SF_Trim_Left .. SF_Trim_Both |
             SF_Float_Value | SF_Integer_Value
          =>
          Expected (1) := VStrings_Set;
        when SF_Slice =>
          Expected (1 .. 3) := (VStrings_Set, Ints_Set, Ints_Set);
        when SF_To_Lower_Char | SF_To_Upper_Char =>
          Expected (1) := VString_or_Chars_Set;
        when SF_Literal_to_VString =>
          Expected (1) := Chars_or_Strings_Set;
        when SF_VString_to_String =>
          Expected (1) := VStrings_Set;
        when SF_Index | SF_Index_Backward =>
          --  Index (OS, +"Windows"[, 3]), Index (OS, "Windows"[, 3]) or Index (OS, 'W'[, 3])
          Expected (1 .. 3) := (VStrings_Set, Any_String_or_Chars_Set, Ints_Set);
        when SF_Starts_With | SF_Ends_With |
             SF_Head_Before_Match | SF_Tail_After_Match =>
          Expected (1 .. 2) := (VStrings_Set, Any_String_or_Chars_Set);
        when SF_Year .. SF_Seconds =>
          Expected (1) := Times_Set;
        when SF_Directory_Exists | SF_Exists | SF_File_Exists |
             SF_Get_Env  | SF_Get_VM_Variable =>
          --  Get_Env (+"PATH")  _or_  Get_Env ("PATH")
          Expected (1) := Any_String_Set;
        when SF_Niladic =>
          null;  --  Zero argument -> no argument type to check.
        when SF_File_or_Console_Information =>
          null;  --  Arguments are parsed separately.
        when SF_Is_Open =>
          Expected (1) := Txt_Fil_Set;
        when others =>
          null;
          --  Here we have functions that are never parsed
          --  E.g. SF_Abs_Float, parsed as SF_Abs_Int, or "&" operators.
      end case;
    end Prepare_Accepted_Parameter_Types;
    --
    procedure Parse_Arguments is
      TYP_of_arg : Typen;
    begin
      for a in 1 .. Args loop
        Expression (CD, Level, FSys + RParent + Comma, Actual (a));
        TYP_of_arg := Actual (a).TYP;
        if Expected (a) (TYP_of_arg) then
          null;  --  All right so far: argument type is in the admitted set of types.
        elsif Actual (a).TYP /= NOTYP then
          Type_Mismatch (
            CD, err_argument_to_std_function_of_wrong_type,
            Found    => Actual (a),
            Expected => Expected (a)
          );
        end if;
        if (Code = SF_Index or Code = SF_Index_Backward)
           and then a = 2
           and then CD.Sy = RParent
        then
          Args := 2;  --  Alright: Index, Index_Backard without From.
          exit;
        elsif a < Args then
          Need (CD, Comma, err_COMMA_missing);
        end if;
      end loop;
    end Parse_Arguments;
    --
    procedure Parse_File_Information_Function (FIF_Code : SF_Code) is
      file_parameter : Boolean;
    begin
      if CD.Sy = LParent then
        --  End_Of_File (...), End_Of_Line (...).
        InSymbol (CD);
        Expression (CD, Level, FSys + RParent + Comma, X);
        if X.TYP /= Text_Files then
          Type_Mismatch (CD, err_syntax_error, Found => X, Expected => Txt_Fil_Set);
        end if;
        file_parameter := True;
        Need (CD, RParent, err_closing_parenthesis_missing);
      else
        --  Niladic End_Of_File, End_Of_Line without parameter.
        file_parameter := False;
      end if;
      Emit_2 (CD, k_HAT_Function, Boolean'Pos (file_parameter), SF_Code'Pos (FIF_Code));
    end Parse_File_Information_Function;
    --
    procedure Adjustments_to_Parameter_Types is  --  Here is the actual overloading implemented.
    begin
      case Code is
        when SF_Abs_Int =>  --  Abs (NB: in Ada it's an operator, not a function)
          Return_Typ := Actual (1);
          if Actual (1).TYP = Floats then
            Code_Adjusted := SF_Abs_Float;
          end if;
        when SF_T_Succ | SF_T_Pred =>  -- S'Succ, S'Pred : RM 3.5 (22, 25)
          Return_Typ := Actual (1);
        when SF_Round_Float_to_Int | SF_Trunc_Float_to_Int |
             SF_Sin | SF_Cos | SF_Exp | SF_Log | SF_Sqrt | SF_Arctan
          =>
          if Ints_Set (Actual (1).TYP) then
            Forbid_Type_Coercion (CD, Found => Actual (1), Expected => Construct_Root (Floats));
            Emit_1 (CD, k_Integer_to_Float, 0);  --  Ghost of SmallAda
          end if;
        when SF_Min_Max_Int =>
          Return_Typ := Actual (1);
          if Actual (1).TYP = Floats then
            Code_Adjusted := (if Code = SF_Min_Int then SF_Min_Float else SF_Max_Float);
          end if;
          if Actual (2).TYP /= Actual (1).TYP then
            Type_Mismatch (CD, err_parameter_types_do_not_match, Actual (2), Actual (1));
          end if;
        when SF_Image_Ints =>
          case Actual (1).TYP is
            when Floats    => Code_Adjusted := SF_Image_Floats;
            when Times     => Code_Adjusted := SF_Image_Times;
            when Durations => Code_Adjusted := SF_Image_Durations;
            when others => null;
          end case;
        when SF_To_Lower_Char =>               --  To_Lower (Item : Character) return Character;
          Return_Typ := Actual (1);
          if Actual (1).TYP = VStrings then    --  To_Lower (Item : VString) return VString;
            Code_Adjusted := SF_To_Lower_VStr;
          end if;
        when SF_To_Upper_Char =>               --  To_Upper (Item : Character) return Character;
          Return_Typ := Actual (1);
          if Actual (1).TYP = VStrings then    --  To_Upper (Item : VString) return VString;
            Code_Adjusted := SF_To_Upper_VStr;
          end if;
        when SF_Index | SF_Index_Backward |
             SF_Starts_With | SF_Ends_With |
             SF_Head_Before_Match | SF_Tail_After_Match =>
          if Code in SF_Index_Any_Direction then
            if Args = 2 then
              Emit_1 (CD, k_Push_Discrete_Literal, 0);  --  We push a non-positive value for `From`.
            end if;
            Emit (CD, k_Pop_to_Temp);
            --  `From` is now temporarily removed from the stack. `Pattern` is at the top.
          end if;
          --  Second parameter can be a Character, a String, or a VString.
          Check_any_String_and_promote_to_VString (CD, Actual (2), True);
          if Code in SF_Index_Any_Direction then
            Emit (CD, k_Push_Temp);
            --  `From` is now back at the stack top.
          end if;
        when SF_Directory_Exists | SF_Exists | SF_File_Exists |
             SF_Get_Env | SF_Get_VM_Variable =>
          --  Get_Env ("PATH")  becomes  Get_Env (+"PATH")
          Check_any_String_and_promote_to_VString (CD, Actual (1), False);
        when SF_Literal_to_VString =>
          --  Explicit call to the `To_VString` function, identical to the unary "+".
          --  See Simple_Expression in Parser.Expressions
          Check_any_String_and_promote_to_VString (CD, Actual (1), True);
          do_SF_emit := False;  --  Conversion code is already emitted.
        when SF_Niladic =>
          null;  --  No arguments, nothing to adjust
        when others =>
          null;  --  Nothing to adjust regarding parameter types.
      end case;
    end Adjustments_to_Parameter_Types;
    --
  begin
    Return_Typ := CD.IdTab (Ident_Index).xtyp;
    --
    Prepare_Accepted_Parameter_Types;
    --
    if Code in SF_File_or_Console_Information then
      Parse_File_Information_Function (Code);
    else
      if Args > 0 then
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Parse_Arguments;
      end if;
      Adjustments_to_Parameter_Types;
      if do_SF_emit then
        Emit_Std_Funct (CD, Code_Adjusted);
      end if;
      if Args > 0 then
        Need (CD, RParent, err_closing_parenthesis_missing);
      end if;
    end if;
  end Standard_Function;

end HAC_Sys.Parser.Standard_Functions;
