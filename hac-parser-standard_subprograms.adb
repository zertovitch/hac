with HAC.Parser.Expressions; use HAC.Parser.Expressions;
with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser.Standard_Subprograms is

  SF_Args : constant array (SF_Code) of Natural :=
    ( SF_Niladic            => 0,
      SF_Element |
      SF_Index |
      SF_Int_Times_Char |
      SF_Int_Times_VStr     => 2,
      SF_Slice              => 3,
      others                => 1
    );

  procedure Standard_Function (
    CD          : in out Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    Ident_Index :        Integer;
    Code        :        SF_Code;
    Return_Typ  :    out Exact_Typ
  )
  is
    Max_Args : constant := 3;
    Args : constant Natural := SF_Args (Code);
    Expected : array (1 .. Max_Args) of Typ_Set;    --  Expected type of the function's arguments
    Actual   : array (1 .. Max_Args) of Exact_Typ;  --  Actual type from argument expression
    Code_Adjusted : SF_Code := Code;
    IFP  : Integer;
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
  begin
    Return_Typ := (CD.IdTab (Ident_Index).TYP, CD.IdTab (Ident_Index).Ref);
    --
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
      when SF_Random_Int | SF_Argument =>
        Expected (1) := Ints_Set;
      when SF_Element =>
        Expected (1 .. 2) := (VStrings_Set, Ints_Set);
      when SF_Length |
           SF_Trim_Left .. SF_Trim_Both =>
        Expected (1) := VStrings_Set;
      when SF_Slice =>
        Expected (1 .. 3):= (VStrings_Set, Ints_Set, Ints_Set);
      when SF_To_Lower_Char | SF_To_Upper_Char =>
        Expected (1) := VStrings_or_Chars_Set;
      when SF_Index =>
        Expected (1 .. 2) := (VStrings_Set, VStrings_Set);
      when SF_Get_Env =>
        Expected (1) := VStrings_or_Str_Lit_Set;  --  Get_Env ("PATH")  _or_  Get_Env (+"PATH")
      when SF_Niladic =>
        null;  --  Zero argument -> no argument type to check.
      when SF_EOF | SF_EOLN =>
        null;  --  Arguments are parsed separately.
      when others =>
        null;
        --  Here we have functions that are never parsed
        --  E.g. SF_Abs_Float, parsed as SF_Abs_Int, or "&" operators.
    end case;
    --
    --  Parameter parsing
    --
    if Code in SF_EOF .. SF_EOLN then
      --  Very special case...
      Need (CD, LParent, err_missing_an_opening_parenthesis);
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
      elsif Equal (CD.Id, "INPUT") then  --  Standard_Input
        Emit2 (CD, k_Standard_Functions, 0, SF_Code'Pos (Code));
      else
        IFP := Get_File_Pointer (CD, CD.Id);
        if IFP = No_File_Index then  --  NB: bug fix: was 0 instead of -1...
          Error (CD, err_undefined_identifier);
        else
          Emit2 (CD, k_Standard_Functions, IFP, SF_Code'Pos (Code));
        end if;
      end if;
      InSymbol (CD);
      Need (CD, RParent, err_closing_parenthesis_missing);
    else
      if Args > 0 then
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Parse_Arguments;
      end if;
      --
      --  Adjustments
      --
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
            Forbid_Type_Coercion (CD,
              "value is of integer type; floating-point is expected as parameter");
            Emit1 (CD, k_Integer_to_Float, 0);
          end if;
        when SF_To_Lower_Char =>
          Return_Typ := Actual (1);
          if Actual (1).TYP = VStrings then      --  To_Lower (Item : VString) return VString;
            Code_Adjusted := SF_To_Lower_VStr;
          end if;
        when SF_To_Upper_Char =>
          Return_Typ := Actual (1);
          if Actual (1).TYP = VStrings then      --  To_Upper (Item : VString) return VString;
            Code_Adjusted := SF_To_Upper_VStr;
          end if;
        when SF_Get_Env =>
          if Actual (1).TYP = String_Literals then
            Emit_Std_Funct (CD, SF_Literal_to_VString);
          end if;
        when others =>
          null;  --  Nothing
      end case;
      --
      Emit_Std_Funct (CD, Code_Adjusted);
      --
      if Args > 0 then
        Need (CD, RParent, err_closing_parenthesis_missing);
      end if;
    end if;
  end Standard_Function;

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    N       :        SP_Code
  )
  is
    I                 : Integer;
    F                 : Opcode;
    X, Y              : Exact_Typ;
    do_first_InSymbol : Boolean := True;
  begin
    case N is
      when SP_Get | SP_Get_Line =>
        if CD.Sy = LParent then
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);  -- Schoening
          if I = No_File_Index then
            Emit1 (CD, k_Set_current_file_pointer, 0);
            do_first_InSymbol := False;
          else -- First parameter is a file variable
            Emit1 (CD, k_Set_current_file_pointer, I);
            InSymbol (CD);
            if CD.Sy /= Comma then
              if CD.Sy = RParent then
                goto SKIP1b; -- skip the loop
              else
                Error (CD, err_identifier_missing);
              end if;
            end if;
          end if;
          loop
            if do_first_InSymbol then
              InSymbol (CD);
            end if;
            do_first_InSymbol := True;
            if CD.Sy /= IDent then
              Error (CD, err_identifier_missing);
            else
              I := Locate_Identifier (CD, CD.Id, Level);
              InSymbol (CD);
              if I /= 0 then
                if CD.IdTab (I).Obj /= Variable then
                  Error (CD, err_variable_missing);
                else
                  X.TYP := CD.IdTab (I).TYP;
                  X.Ref := CD.IdTab (I).Ref;
                  if CD.IdTab (I).Normal then
                    F := k_Load_Address;
                  else
                    F := k_Push_Value;
                  end if;
                  Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr);
                  if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                    Selector (CD, Level, FSys + Comma_RParent, X);
                  end if;
                  if X.TYP = Ints or
                     X.TYP = Floats or
                     X.TYP = Chars or
                     X.TYP = NOTYP
                  then
                    Emit1 (CD, k_Read, Typen'Pos (X.TYP));
                  else
                    Error (CD, err_illegal_parameters_to_Put);
                  end if;
                end if;
              end if;
            end if;
            Test (CD, Comma_RParent, FSys, err_incorrectly_used_symbol);
            exit when CD.Sy /= Comma;
          end loop;
          <<SKIP1b>>
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;
        if N = SP_Get_Line then
          Emit (CD, k_Get_Newline);
        end if;

      when SP_Put | SP_Put_Line =>
        if CD.Sy = LParent then
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);   -- Schoening
          if I = No_File_Index then
            Emit1 (CD, k_Set_current_file_pointer, 0);
            do_first_InSymbol := False;
          else -- First parameter is a file variable
            Emit1 (CD, k_Set_current_file_pointer, I);
            InSymbol (CD);
            if CD.Sy /= Comma then
              if CD.Sy = RParent then
                goto Label_21; -- skip the loop
              else
                Error (CD, err_identifier_missing);
              end if;
            end if;
          end if;
          loop
            if do_first_InSymbol then
              InSymbol (CD);
            end if;
            do_first_InSymbol := True;
            --
            Expression (CD, Level, FSys + Colon_Comma_RParent, X);
            if X.TYP = Enums then
              X.TYP := Ints;  --  Ow... Silent S'Pos
            end if;
            if (X.TYP not in Standard_Typ) and X.TYP /= String_Literals then
              Error (CD, err_illegal_parameters_to_Put);
            end if;
            if CD.Sy = Colon then  --  ':' Pascal-ism (Write/WriteLn) !!
              InSymbol (CD);
              Expression (CD, Level, FSys + Colon_Comma_RParent, Y);
              if Y.TYP /= Ints then
                Error (CD, err_parameter_must_be_Integer);
              end if;
              if CD.Sy = Colon then  --  ':' Pascal-ism (Write/WriteLn) !!
                if X.TYP /= Floats then
                  Error (CD, err_parameter_must_be_of_type_Float);
                end if;
                InSymbol (CD);
                Expression (CD, Level, FSys + Comma_RParent, Y);
                if Y.TYP /= Ints then
                  Error (CD, err_parameter_must_be_Integer);
                end if;
                Emit (CD, k_Write_Float);
              else
                Emit1 (CD, k_Write_Formatted, Typen'Pos (X.TYP));
              end if;
            elsif X.TYP = String_Literals then
              Emit (CD, k_Write_String);
            else
              Emit1 (CD, k_Write_Unformatted, Typen'Pos (X.TYP));
            end if;
            exit when CD.Sy /= Comma;
          end loop;
          <<Label_21>>
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;
        if N = SP_Put_Line then
          Emit (CD, k_Put_Newline);
        end if;

      when SP_New_Line =>
        Emit (CD, k_Put_Newline);

      when SP_Wait | SP_Signal =>
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          if CD.Sy /= IDent then
            Error (CD, err_undefined_identifier);
          else
            I := Locate_Identifier (CD, CD.Id, Level);
            InSymbol (CD);
            if I /= 0 then
              if CD.IdTab (I).Obj /= Variable then
                Error (CD, err_variable_missing);
              else
                X.TYP := CD.IdTab (I).TYP;
                X.Ref := CD.IdTab (I).Ref;
                if CD.IdTab (I).Normal then
                  F := k_Load_Address;
                else
                  F := k_Push_Value;
                end if;
                Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr);
                if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                  Selector (CD, Level, FSys + RParent, X);
                end if;
                if X.TYP = Ints then
                  if N = SP_Wait then
                    Emit (CD, k_Wait_Semaphore);
                  else
                    Emit (CD, k_Signal_Semaphore);
                  end if;
                else
                  Error (CD, err_parameter_must_be_Integer);
                end if;
              end if;
            end if;
          end if;
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;

      when SP_Reset | SP_Rewrite | SP_Close =>
        -- Schoening
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);
          if I = No_File_Index then
            Error (CD, err_identifier_missing);
          else
            Emit2 (CD, k_File_I_O, I, SP_Code'Pos (N));
          end if;
          InSymbol (CD);
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;

      when SP_CursorAt =>
        -- Cramer
        if CD.Sy /= LParent then
          Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
        else
          begin
            InSymbol (CD);
            Expression (CD, Level, Colon_Comma_LParent_RParent_Semicolon, X);
            if X.TYP /= Ints then
              Skip (CD, Semicolon, err_parameter_must_be_Integer);
            end if;
            if CD.Sy /= Comma then
              Skip (CD, Semicolon, err_COMMA_missing);
            else
              InSymbol (CD);
              Expression (CD, Level, Colon_Comma_LParent_RParent_Semicolon, X);
              if X.TYP /= Ints then
                Skip (CD, Semicolon, err_parameter_must_be_Integer);
              end if;
              if CD.Sy = Comma then
                Skip (CD, Semicolon, err_number_of_parameters_do_not_match);
              elsif CD.Sy /= RParent then
                Skip (CD, Semicolon, err_closing_parenthesis_missing);
              else
                Emit (CD, k_Cursor_At);
                InSymbol (CD);
              end if;
            end if;
          end;
        end if;

      when SP_Quantum =>
        -- Cramer
        if CD.Sy /= LParent then
          Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          Expression (CD, Level, RParent_Set, X);
          if X.TYP /= Floats then
            Skip (CD, Semicolon, err_parameter_must_be_of_type_Float);
          end if;
          if CD.Sy /= RParent then
            Skip (CD, Semicolon, err_closing_parenthesis_missing);
          else
            Emit (CD, k_Set_Quantum_Task);
            InSymbol (CD);
          end if;
        end if;

      when SP_Priority =>
        -- Cramer
        if CD.Sy /= LParent then
          Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          Expression (CD, Level, RParent_Set, X);
          if X.TYP /= Ints then
            Skip (CD, Semicolon, err_parameter_must_be_Integer);
          end if;
          if CD.Sy /= RParent then
            Skip (CD, Semicolon, err_closing_parenthesis_missing);
          else
            Emit (CD, k_Set_Task_Priority);
            InSymbol (CD);
          end if;
        end if;
        --
      when SP_InheritP =>
        -- Cramer
        if CD.Sy /= LParent then
          Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          Boolean_Expression (CD, Level, RParent_Set, X);
          if CD.Sy /= RParent then
            Skip (CD, Semicolon, err_closing_parenthesis_missing);
          else
            Emit (CD, k_Set_Task_Priority_Inheritance);
            InSymbol (CD);
          end if;
        end if;
        --
      when SP_Set_Env =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        for a in 1 .. 2 loop
          Expression (CD, Level, RParent_Set, X);  --  We push the arguments in the stack.
          if VStrings_or_Str_Lit_Set (X.TYP) then
            --  Set_Env ("HAC_Var", "Hello");       <-  String_Literals
            --  Set_Env (+"HAC_Var", +"Hello");     <-  VStrings
            if X.TYP = String_Literals then
              Emit_Std_Funct (CD, SF_Literal_to_VString);
            end if;
          else
            Type_Mismatch (
              CD, err_parameter_types_do_not_match,
              Found    => X,
              Expected => VStrings_or_Str_Lit_Set
            );
          end if;
          if a < 2 then
            Need (CD, Comma, err_COMMA_missing);
          end if;
        end loop;
        Emit2 (CD, k_File_I_O, I, SP_Code'Pos (N));
        Need (CD, RParent, err_closing_parenthesis_missing);
    end case;
  end Standard_Procedure;

end HAC.Parser.Standard_Subprograms;
