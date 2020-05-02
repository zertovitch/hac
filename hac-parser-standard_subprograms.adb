with HAC.Parser.Expressions; use HAC.Parser.Expressions;
with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser.Standard_Subprograms is

  procedure Standard_Function (
    CD          : in out Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    Ident_Index :        Integer;
    SF_Code     :        Integer;
    X           :    out Exact_Typ
  )
  is
    T_Argument : Typ_Set;  --  Expected type of the function's argument
    N : Integer := SF_Code;
    IFP : Integer;
  begin
    case N is
      when SF_EOF .. SF_EOLN =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        if CD.Sy /= IDent then
          Error (CD, err_identifier_missing);
        elsif Equal (CD.Id, "INPUT") then  --  Standard_Input
          Emit2 (CD, k_Standard_Functions, 0, N);
        else
          IFP := Get_File_Pointer (CD, CD.Id);
          if IFP = No_File_Index then  --  NB: bug fix: was 0 instead of -1...
            Error (CD, err_undefined_identifier);
          else
            Emit2 (CD, k_Standard_Functions, IFP, N);
          end if;
        end if;
        InSymbol (CD);
        X.TYP := CD.IdTab (Ident_Index).TYP;
        Need (CD, RParent, err_closing_parenthesis_missing);
      when SF_Niladic =>
        Emit1 (CD, k_Standard_Functions, N);
      when others =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expression (CD, Level, FSys + RParent, X);
        case N is
          when SF_Abs =>  --  Abs (NB: in Ada it's an operator, not a function)
            T_Argument       := Numeric_Typ_Set;
            CD.IdTab (Ident_Index).TYP := X.TYP;  --  !! Redefines the function's return type
            if X.TYP = Floats then
              N := N + 1;
            end if;
          when SF_T_Val =>  --  S'Val : RM 3.5.5 (5)
            T_Argument := Ints_Typ;
          when SF_T_Pos =>  --  S'Pos : RM 3.5.5 (2)
            T_Argument := Discrete_Typ;
          when SF_T_Succ | SF_T_Pred =>  -- S'Succ, S'Pred : RM 3.5 (22, 25)
            T_Argument := Discrete_Typ;
            CD.IdTab (Ident_Index).TYP := X.TYP;  --  !! Redefines the function's return type
          when SF_Round_Float_to_Int | SF_Trunc_Float_to_Int |
               SF_Sin | SF_Cos | SF_Exp | SF_Log | SF_Sqrt | SF_Arctan
            =>
            T_Argument := Numeric_Typ_Set;
            if Ints_Typ (X.TYP) then
              Forbid_Type_Coercion (CD, "value is of integer type; floating-point is expected here");
              Emit1 (CD, k_Integer_to_Float, 0);
            end if;
          when SF_Random_Int =>
            T_Argument := Ints_Typ;
          when others =>
            null;
        end case;  --  N
        --
        if T_Argument (X.TYP) then
          Emit1 (CD, k_Standard_Functions, N);
        elsif X.TYP /= NOTYP then
          Error (CD, err_argument_to_std_function_of_wrong_type);
        end if;
        X.TYP := CD.IdTab (Ident_Index).TYP;
        Need (CD, RParent, err_closing_parenthesis_missing);
    end case;
  end Standard_Function;

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    N       :        Integer
  )
  is
    I                 : Integer;
    F                 : Opcode;
    X, Y              : Exact_Typ;
    do_first_InSymbol : Boolean := True;
  begin
    case N is  --  Numbers: see EnterStdFcns in HAC.Compiler
      when 1 | 2 =>  -- GET, GET_LINE
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
                    Emit1 (CD, k_Read, Typs'Pos (X.TYP));
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
        if N = 2 then
          Emit (CD, k_Get_Newline);
        end if;

      when 3 | 4 =>          -- PUT, PUT_LINE
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
                Emit1 (CD, k_Write_2, Typs'Pos (X.TYP));
              end if;
            elsif X.TYP = String_Literals then
              Emit (CD, k_Write_String);
            else
              Emit1 (CD, k_Write_1, Typs'Pos (X.TYP));
            end if;
            exit when CD.Sy /= Comma;
          end loop;
          <<Label_21>>
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;
        if N = 4 then
          Emit (CD, k_Put_Newline);
        end if;

      when 5 | 6 =>                  -- Wait, SIGNAL
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
                  if N = 5 then
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

      when 7 | 8 | 9 =>    -- reset, Rewrite, Close
        -- Schoening
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);
          if I = No_File_Index then
            Error (CD, err_identifier_missing);
          else
            Emit2 (CD, k_File_I_O, I, N);
          end if;
          InSymbol (CD);
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;  -- reset

      when 10 =>        -- CursorAt
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
        end if;                -- CursorAt

      when 11 =>                   -- Quantum
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
        end if;                -- Quantum

      when 12 =>                   -- Set Priority
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
        end if;                -- Priority
        --
      when 13 =>                   -- Set Priority Inheritance,INHERITP
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
        end if;                -- Inheritp
        --
      when others =>
        null;
    end case;
  end Standard_Procedure;

end HAC.Parser.Standard_Subprograms;
