with HAC.Compiler.PCode_Emit,
     HAC.Parser.Expressions,
     HAC.Parser.Helpers,
     HAC.Scanner;

package body HAC.Parser.Standard_Functions is

  use Compiler.PCode_Emit, Defs, Expressions, Helpers, PCode, Scanner;

  SF_Args : constant array (SF_Code) of Natural :=
    ( SF_Niladic            => 0,
      SF_Element |
      SF_Head | SF_Tail |
      SF_Starts_With |
      SF_Ends_With |
      SF_Index |
      SF_Int_Times_Char |
      SF_Int_Times_VStr     => 2,
      SF_Slice              => 3,
      others                => 1
    );

  procedure Standard_Function (
    CD          : in out Compiler_Data;
    Level       :        PCode.Nesting_level;
    FSys        :        Defs.Symset;
    Ident_Index :        Integer;
    Code        :        PCode.SF_Code;
    Return_Typ  :    out Exact_Typ
  )
  is
    Max_Args : constant := 3;
    Args : constant Natural := SF_Args (Code);
    Expected : array (1 .. Max_Args) of Typ_Set;    --  Expected type of the function's arguments
    Actual   : array (1 .. Max_Args) of Exact_Typ;  --  Actual type from argument expression
    Code_Adjusted : SF_Code := Code;
    X : Exact_Typ;
    --
    procedure Prepare_Accepted_Parameter_Types is
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
          Expected (1) := Numeric_Typ_Set or Times_Set;
        when SF_Image_Attribute_Floats =>
          Expected (1) := Floats_Set;
        when SF_Random_Int | SF_Argument =>
          Expected (1) := Ints_Set;
        when SF_Element | SF_Head | SF_Tail =>
          Expected (1 .. 2) := (VStrings_Set, Ints_Set);
        when SF_Length |
             SF_Trim_Left .. SF_Trim_Both |
             SF_Float_Value | SF_Integer_Value
          =>
          Expected (1) := VStrings_Set;
        when SF_Slice =>
          Expected (1 .. 3):= (VStrings_Set, Ints_Set, Ints_Set);
        when SF_To_Lower_Char | SF_To_Upper_Char =>
          Expected (1) := VStrings_or_Chars_Set;
        when SF_Index | SF_Starts_With | SF_Ends_With =>
          --  Index (OS, +"Windows")  _or_  Index (OS, "Windows")
          Expected (1 .. 2) := (VStrings_Set, VStrings_or_Str_Lit_Set);
        when SF_Get_Env | SF_Shell_Execute =>
          --  Get_Env (+"PATH")  _or_  Get_Env ("PATH")
          Expected (1) := VStrings_or_Str_Lit_Set;
        when SF_Niladic =>
          null;  --  Zero argument -> no argument type to check.
        when SF_File_Information =>
          null;  --  Arguments are parsed separately.
        when others =>
          null;
          --  Here we have functions that are never parsed
          --  E.g. SF_Abs_Float, parsed as SF_Abs_Int, or "&" operators.
      end case;
    end Prepare_Accepted_Parameter_Types;
    --
    procedure Parse_Arguments is
    begin
      for a in 1 .. Args loop
        Expression (CD, Level, FSys + RParent + Comma, Actual (a));
        if Expected (a) (Actual (a).TYP) then
          null;  --  All right so far: argument type is in the admitted set of types.
        elsif Actual (a).TYP /= NOTYP then
          Type_Mismatch (
            CD, err_argument_to_std_function_of_wrong_type,
            Found    => Actual (a),
            Expected => Expected (a)
          );
        end if;
        if a < Args then
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
      Emit2 (CD, k_Standard_Functions, Boolean'Pos (file_parameter), SF_Code'Pos (FIF_Code));
    end Parse_File_Information_Function;
    --
    procedure Adjustments_to_Parameter_Types is
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
            Forbid_Type_Coercion (CD, Found => Actual (1), Expected => (Floats, 0));
            Emit1 (CD, k_Integer_to_Float, 0);  --  Ghost of SmallAda
          end if;
        when SF_Image_Ints =>
          case Actual (1).TYP is
            when Floats => Code_Adjusted := SF_Image_Floats;
            when Times  => Code_Adjusted := SF_Image_Times;
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
        when SF_Index | SF_Starts_With | SF_Ends_With =>
          --  Index (OS, +"Windows")  _or_  Index (OS, "Windows")
          if Actual (2).TYP = String_Literals then
            Emit_Std_Funct (CD, SF_Literal_to_VString);
          end if;
        when SF_Get_Env | SF_Shell_Execute =>
          --  Get_Env ("PATH")  ->  Get_Env (+"PATH")
          if Actual (1).TYP = String_Literals then
            Emit_Std_Funct (CD, SF_Literal_to_VString);
          end if;
        when others =>
          null;  --  Nothing to adjust regarding parameter types.
      end case;
    end Adjustments_to_Parameter_Types;
    --
  begin
    Return_Typ := CD.IdTab (Ident_Index).xTyp;
    --
    Prepare_Accepted_Parameter_Types;
    --
    if Code in SF_File_Information then
      Parse_File_Information_Function (Code);
    else
      if Args > 0 then
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Parse_Arguments;
      end if;
      Adjustments_to_Parameter_Types;
      Emit_Std_Funct (CD, Code_Adjusted);
      if Args > 0 then
        Need (CD, RParent, err_closing_parenthesis_missing);
      end if;
    end if;
  end Standard_Function;

end HAC.Parser.Standard_Functions;
