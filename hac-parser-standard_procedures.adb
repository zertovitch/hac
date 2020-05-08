with HAC.Parser.Expressions; use HAC.Parser.Expressions;
with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser.Standard_Procedures is

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    Code    :        SP_Code
  )
  is
    I                 : Integer;
    F                 : Opcode;
    X, Y              : Exact_Typ;
    do_first_InSymbol : Boolean := True;
  begin
    case Code is
      when SP_Get | SP_Get_Line =>
        if CD.Sy = LParent then
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);  --  Schoening
          if I = No_File_Index then
            Emit1 (CD, k_Set_current_file_pointer, 0);
            do_first_InSymbol := False;
          else  --  First parameter is a file variable
            Emit1 (CD, k_Set_current_file_pointer, I);
            InSymbol (CD);
            if CD.Sy /= Comma then
              if CD.Sy = RParent then
                goto SKIP1b;  --  skip the loop
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
                     X.TYP = VStrings
                  then
                    Emit1 (CD, k_Read, Typen'Pos (X.TYP));
                  elsif X.TYP = NOTYP then
                    null;  --  Error(s) already appeared in the parsing.
                  else
                    Error (CD, err_illegal_parameters_to_Get);
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
        if Code = SP_Get_Line
          and X.TYP /= VStrings  --  A string is already got via a Get_Line.
        then
          Emit (CD, k_Skip_Line);
        end if;

      when SP_Skip_Line =>
        Emit (CD, k_Skip_Line);

      when SP_Put | SP_Put_Line =>
        if CD.Sy = LParent then
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);   -- Schoening
          if I = No_File_Index then
            Emit1 (CD, k_Set_current_file_pointer, 0);
            do_first_InSymbol := False;
          else  --  First parameter is a file variable
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
        if Code = SP_Put_Line then
          Emit (CD, k_New_Line);
        end if;

      when SP_New_Line =>
        Emit (CD, k_New_Line);

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
                  if Code = SP_Wait then
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
        --  Schoening
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          I := Get_File_Pointer (CD, CD.Id);
          if I = No_File_Index then
            Error (CD, err_identifier_missing);
          else
            Emit2 (CD, k_File_I_O, I, SP_Code'Pos (Code));
          end if;
          InSymbol (CD);
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;

      when SP_CursorAt =>
        --  Cramer
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
        --  Cramer
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
        --  Cramer
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
        --  Cramer
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
        for arg in 1 .. 2 loop
          Expression (CD, Level, RParent_Set, X);  --  We push the arguments in the stack.
          if VStrings_or_Str_Lit_Set (X.TYP) then
            --  Set_Env ("HAC_Var", "Hello");       <-  2 String_Literals
            --  Set_Env (+"HAC_Var", +"Hello");     <-  2 VStrings
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
          if arg < 2 then
            Need (CD, Comma, err_COMMA_missing);
          end if;
        end loop;
        Emit2 (CD, k_File_I_O, I, SP_Code'Pos (Code));
        Need (CD, RParent, err_closing_parenthesis_missing);
    end case;
  end Standard_Procedure;

end HAC.Parser.Standard_Procedures;
