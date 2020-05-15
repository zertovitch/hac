with HAC.Parser.Expressions; use HAC.Parser.Expressions;
with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser.Standard_Procedures is

  type Def_param_type is array (Typen, 1 .. 3) of Integer;

  invalid : constant := -1;

  def_param : constant Def_param_type :=
    (Ints    =>  (IIO.Default_Width,   IIO.Default_Base,   invalid),
     Floats  =>  (RIO.Default_Fore,    RIO.Default_Aft,    RIO.Default_Exp),
     Bools   =>  (BIO.Default_Width,   invalid,            invalid),
     others  =>  (others => invalid));

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
    --
    procedure Parse_Gets is
    --  Parse Get & Co after an eventual File parameter
    begin
      --  The "out" variable for Get, Get_Immediate, Get_Line.
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
      else
        I := Locate_Identifier (CD, CD.Id, Level);
        InSymbol (CD);
        if I /= 0 then
          if CD.IdTab (I).Obj /= Variable then
            Error (CD, err_variable_missing);
          else
            X := CD.IdTab (I).xTyp;
            if CD.IdTab (I).Normal then
              F := k_Push_Address;
            else
              F := k_Push_Value;
            end if;
            Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr_or_Sz);
            if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
              Selector (CD, Level, FSys + Comma_RParent, X);
            end if;
            if X.TYP = Ints or
               X.TYP = Floats or
               X.TYP = Chars or
               X.TYP = VStrings
            then
              Emit2 (CD, k_Read, Boolean'Pos (Code = SP_Get_Immediate), Typen'Pos (X.TYP));
            elsif X.TYP = NOTYP then
              null;  --  Error(s) already appeared in the parsing.
            else
              Error (CD, err_illegal_parameters_to_Get);
            end if;
          end if;
        end if;
      end if;
    end Parse_Gets;
    --
    procedure Parse_Puts is
    --  Parse Put & Co after an eventual File parameter
      Format_Params : Natural := 0;
    begin
      Expression (CD, Level, FSys + Colon_Comma_RParent, X);
      --  Now X is the type of the item to Put.
      if X.TYP = Enums then
        X.TYP := Ints;  --  Ow... Silent S'Pos. We keep this hack until 'Image is done.
      end if;
      if (X.TYP not in Standard_Typ) and X.TYP /= String_Literals then
        Error (CD, err_illegal_parameters_to_Put);
      end if;
      for Param in 1 .. 3 loop
        exit when CD.Sy /= Comma;
        InSymbol (CD);
        Format_Params := Format_Params + 1;
        --  Here we parse:
        --    Width, Base    for Put ([F,] I [, Width [, Base]]);
        --    Fore, Aft, Exp for Put ([F,] R [, Fore[, Aft[, Exp]]]);
        --    Width          for Put ([F,] B [, Width]);
        Expression (CD, Level, FSys + Colon_Comma_RParent, Y);
        if Y.TYP /= Ints then
          Error (CD, err_parameter_must_be_Integer);
        end if;
      end loop;
      --  Check given / default parameters (nice short common solution, isn't it ?)
      for Param in 1 .. Format_Params loop
        --  First we check if the programmer didn't put too many
        --  (then, undefined) parameters.
        if def_param (X.TYP, Param) = invalid then
          Error (CD, err_illegal_parameters_to_Put);
        end if;
      end loop;
      if X.TYP = String_Literals then
        Emit (CD, k_Write_String_Literal);
      else
        for Param in Format_Params + 1 .. 3 loop
          --  Send default parameters to the stack.
          --  In order to have a fixed number of parameters in all cases,
          --  we push also the "invalid" ones. See Do_Write_Formatted
          --  to have an idea on how everybody is retrieved from the stack.
          Emit1 (CD, k_Load_Discrete_Literal, def_param (X.TYP, Param));
        end loop;
        Emit1 (CD, k_Write_Formatted, Typen'Pos (X.TYP));
      end if;
    end Parse_Puts;
    --
    procedure Set_Abstract_Console is
    begin
      Emit1 (CD, k_File_I_O, SP_Code'Pos (SP_Push_Abstract_Console));
    end;
    --
  begin
    case Code is
      when SP_Get | SP_Get_Immediate | SP_Get_Line =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Set_Abstract_Console;
        Parse_Gets;
        Need (CD, RParent, err_closing_parenthesis_missing);
        --
        if Code = SP_Get_Line
          and X.TYP /= VStrings  --  A string is already got via an external Get_Line.
        then
          Set_Abstract_Console;
          Emit (CD, k_Skip_Line);
        end if;

      when SP_Skip_Line =>
        Set_Abstract_Console;
        Emit (CD, k_Skip_Line);

      when SP_Put | SP_Put_Line =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Set_Abstract_Console;
        Parse_Puts;
        Need (CD, RParent, err_closing_parenthesis_missing);
        --
        if Code = SP_Put_Line then
          Set_Abstract_Console;
          Emit (CD, k_New_Line);
        end if;

      when SP_New_Line =>
        Set_Abstract_Console;
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
                X := CD.IdTab (I).xTyp;
                if CD.IdTab (I).Normal then
                  F := k_Push_Address;
                else
                  F := k_Push_Value;
                end if;
                Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr_or_Sz);
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
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          Expression (CD, Level, FSys + Colon_Comma_RParent, X);
          if X.TYP /= Text_Files then
            Type_Mismatch (CD, err_syntax_error, Found => X, Expected => Txt_Fil_Set);
          end if;
          if Code = SP_Reset or Code = SP_Rewrite then
            Need (CD, Comma, err_COMMA_missing);
            Expression (CD, Level, FSys + Colon_Comma_RParent, X);
            Type_Mismatch (CD, err_syntax_error, Found => X, Expected => VStrings_Set);
          end if;
          Emit2 (CD, k_File_I_O, I, SP_Code'Pos (Code));
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

      when SP_Push_Abstract_Console =>
        null;  --  Used by Get, Put, ... without file parameter.
    end case;
  end Standard_Procedure;

end HAC.Parser.Standard_Procedures;
