with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Calls,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Standard_Procedures is

  use Calls, Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;

  type Def_param_type is array (Typen, 1 .. 3) of Integer;

  invalid : constant := -1;

  def_param : constant Def_param_type :=
    (Ints    =>  (IIO.Default_Width,   IIO.Default_Base,   invalid),
     Floats  =>  (RIO.Default_Fore,    RIO.Default_Aft,    RIO.Default_Exp),
     Bools   =>  (BIO.Default_Width,   invalid,            invalid),
     others  =>  (others => invalid));

  procedure Standard_Procedure (
    CD      : in out Co_Defs.Compiler_Data;
    Level   :        Defs.Nesting_level;
    FSys    :        Defs.Symset;
    Code    :        PCode.SP_Code
  )
  is

    procedure HAT_Procedure_Call (FIO_Code : SP_Code; Param : Operand_2_Type := 0) is
    begin
      Emit_2 (CD, k_HAT_Procedure, SP_Code'Pos (FIO_Code), Param);
    end HAT_Procedure_Call;
    --
    procedure Set_Abstract_Console is
    begin
      HAT_Procedure_Call (SP_Push_Abstract_Console);
    end Set_Abstract_Console;
    --
    procedure Parse_Gets (Code : PCode.SP_Code) is
      --  Parse Get & Co including an eventual File parameter
      Found : Exact_Subtyp;
      with_file : Boolean;
      String_Length_Encoding : Operand_2_Type := 0;
      use type Operand_2_Type;
    begin
      Need (CD, LParent, err_missing_an_opening_parenthesis);
      Push_by_Reference_Parameter (CD, Level, FSys, Found);
      with_file := Found.TYP = Text_Files;
      if with_file then
        Emit (CD, k_Dereference);  --  File handle's value on the stack.
        Need (CD, Comma, err_COMMA_missing);
        Push_by_Reference_Parameter (CD, Level, FSys, Found);
      end if;
      --  The "out" variable for Get, Get_Immediate, Get_Line
      --  has been pushed by reference now.
      if Found.TYP = NOTYP then
        null;  --  Error(s) already appeared in the parsing.
      elsif Text_IO_Get_Item_Set (Found.TYP) then
        if Found.TYP = Arrays then
          --  Array: it must be a fixed-sized String here.
          if Is_Char_Array (CD, Found) then
            String_Length_Encoding := (2 ** Typen'Size) *
              Operand_2_Type (CD.Arrays_Table (Found.Ref).Array_Size);
          else
            Error (CD, err_illegal_parameters_to_Get);
          end if;
        end if;
        HAT_Procedure_Call
          ((if with_file then (if Code = SP_Get_Line then SP_Get_Line_F else SP_Get_F) else Code),
           Typen'Pos (Found.TYP) + String_Length_Encoding);
      else
        Error (CD, err_illegal_parameters_to_Get);
      end if;
      Need (CD, RParent, err_closing_parenthesis_missing);
    end Parse_Gets;
    --
    procedure Parse_Puts (Code : PCode.SP_Code) is
      --  Parse Put & Co including an eventual File parameter
      Item_Typ, Format_Param_Typ : Exact_Subtyp;
      Format_Params : Natural := 0;
      with_file : Boolean;
    begin
      Need (CD, LParent, err_missing_an_opening_parenthesis);
      Expression (CD, Level, FSys + Colon_Comma_RParent, Item_Typ);
      with_file := Item_Typ.TYP = Text_Files;
      if with_file then
        Need (CD, Comma, err_COMMA_missing);
        Expression (CD, Level, FSys + Colon_Comma_RParent, Item_Typ);
      end if;
      --
      --  Here we have tha actual thing to "Put": a character, (v)string, a number.
      --
      if Item_Typ.TYP in Standard_Typ | Special_Strings then
        null;  --  Good, Put[_Line] can do it all "as is"!
      elsif Is_Char_Array (CD, Item_Typ) then
        --  Address is already pushed; we need to push the string's length.
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (CD.Arrays_Table (Item_Typ.Ref).Array_Size));
      else
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
        Expression (CD, Level, FSys + Colon_Comma_RParent, Format_Param_Typ);
        if Format_Param_Typ.TYP /= Ints then
          Error (CD, err_parameter_must_be_Integer);
        end if;
      end loop;
      --  Check given / default parameters (nice short common solution, isn't it ?)
      for Param in 1 .. Format_Params loop
        --  First we check if the programmer didn't put too many
        --  (then, undefined) parameters.
        if def_param (Item_Typ.TYP, Param) = invalid then
          Error (CD, err_illegal_parameters_to_Put);
        end if;
      end loop;
      if Item_Typ.TYP = String_Literals or else Is_Char_Array (CD, Item_Typ) then
        --  With String_Literals and String's, we have *two* values pushed on the stack.
        Format_Params := Format_Params + 1;
      end if;
      for Param in Format_Params + 1 .. 3 loop
        --  Send default parameters to the stack.
        --  In order to have a fixed number of parameters in all cases,
        --  we push also the "invalid" ones. See Do_Write_Formatted
        --  to have an idea on how everybody is retrieved from the stack.
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (def_param (Item_Typ.TYP, Param)));
      end loop;
      HAT_Procedure_Call
       ((if with_file then (if Code = SP_Put_Line then SP_Put_Line_F else SP_Put_F) else Code),
        Typen'Pos (Item_Typ.TYP));
      Need (CD, RParent, err_closing_parenthesis_missing);
    end Parse_Puts;
    --
    X, Y, Z : Exact_Subtyp;
  begin
    case Code is
      when SP_Get | SP_Get_Immediate | SP_Get_Line =>
        Parse_Gets (Code);

      when SP_Put | SP_Put_Line =>
        Parse_Puts (Code);

      when SP_New_Line | SP_Skip_Line =>
        if CD.Sy = LParent then
          InSymbol (CD);
          Expression (CD, Level, FSys + Colon_Comma_RParent, X);
          case X.TYP is
            when Text_Files =>
              if CD.Sy = Comma then
                --  "New_Line (File, Spacing);"
                InSymbol (CD);
                Expression (CD, Level, FSys + RParent, Y);
                if Y.TYP /= Ints then
                  Type_Mismatch (CD, err_syntax_error, Found => Y, Expected => Ints_Set);
                end if;
              else
                --  "New_Line (File);"
                Emit_1 (CD, k_Push_Discrete_Literal, 1);  --  Push default value, Spacing := 1
              end if;
            when Ints =>
              --  "New_Line (Spacing);"
              Set_Abstract_Console;
              Emit (CD, k_Swap);  --  File (the console) has to be before Spacing on the stack.
            when others =>
              Type_Mismatch (CD, err_syntax_error, Found => X, Expected => Txt_Fil_Set or Ints_Set);
          end case;
          Need (CD, RParent, err_closing_parenthesis_missing);
        else  --  "New_Line;"
          Set_Abstract_Console;
          Emit_1 (CD, k_Push_Discrete_Literal, 1);  --  Push default value, Spacing := 1
        end if;
        HAT_Procedure_Call (Code);

      when SP_Wait | SP_Signal =>
        if CD.Sy /= LParent then
          Error (CD, err_missing_an_opening_parenthesis);
        else
          InSymbol (CD);
          Push_by_Reference_Parameter (CD, Level, FSys, X);
          if X.TYP = Ints then
            if Code = SP_Wait then
              Emit (CD, k_Wait_Semaphore);
            else
              Emit (CD, k_Signal_Semaphore);
            end if;
          else
            Error (CD, err_parameter_must_be_Integer);
          end if;
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;

      when SP_Open | SP_Create | SP_Append | SP_Close =>
        if CD.Sy = LParent then
          InSymbol (CD);
          Push_by_Reference_Parameter (CD, Level, FSys + Colon_Comma_RParent, X);
          if X.TYP /= Text_Files then
            Type_Mismatch (CD, err_syntax_error, Found => X, Expected => Txt_Fil_Set);
          end if;
          if Code = SP_Open or Code = SP_Create or Code = SP_Append then
            --  Parse file name.
            Need (CD, Comma, err_COMMA_missing);
            Expression (CD, Level, FSys + Colon_Comma_RParent, X);
            Check_any_String_and_promote_to_VString (CD, X, False);
          end if;
          HAT_Procedure_Call (Code);
          Need (CD, RParent, err_closing_parenthesis_missing);
        else
          Error (CD, err_missing_an_opening_parenthesis);
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
      when SP_Set_Env | SP_Set_VM_Variable | SP_Copy_File | SP_Rename =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        for arg in 1 .. 2 loop
          Expression (CD, Level, Colon_Comma_RParent, X);  --  We push the arguments in the stack.
          --  Set_Env ( "HAC_Var",  "Hello");     <-  2 String's
          --  Set_Env (+"HAC_Var", +"Hello");     <-  2 VString's
          --  Set_Env (+"HAC_Var",  "Hello");
          --  Set_Env ( "HAC_Var", +"Hello");
          Check_any_String_and_promote_to_VString (CD, X, False);
          if arg < 2 then
            Need (CD, Comma, err_COMMA_missing);
          end if;
        end loop;
        HAT_Procedure_Call (Code);
        Need (CD, RParent, err_closing_parenthesis_missing);

      when SP_Delete_File | SP_Set_Directory =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expression (CD, Level, RParent_Set, X);  --  We push the argument in the stack.
        Check_any_String_and_promote_to_VString (CD, X, False);
        HAT_Procedure_Call (Code);
        Need (CD, RParent, err_closing_parenthesis_missing);

      when SP_Shell_Execute =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expression (CD, Level, Comma_RParent, X);  --  We push the argument in the stack.
        Check_any_String_and_promote_to_VString (CD, X, False);
        --  ` Shell_Execute (cmd `  has been parsed at this point.
        if CD.Sy = Comma then
          InSymbol (CD);
          Push_by_Reference_Parameter (CD, Level, RParent_Set, Y);
          case Y.TYP is
            when VStrings =>
              --  Shell_Execute (cmd, output);
              HAT_Procedure_Call (SP_Shell_Execute_Output);
            when Ints =>
              if CD.Sy = Comma then
                InSymbol (CD);
                Push_by_Reference_Parameter (CD, Level, RParent_Set, Z);
                --  Shell_Execute (cmd, result, output);
                HAT_Procedure_Call (SP_Shell_Execute_Result_Output);
                if Z.TYP /= VStrings then
                  Type_Mismatch (CD, err_parameter_types_do_not_match,
                    Found => Z, Expected => VStrings_Set);
                end if;
              else
                --  Shell_Execute (cmd, result);
                HAT_Procedure_Call (SP_Shell_Execute_with_Result);
              end if;
            when others =>
              Type_Mismatch (CD, err_parameter_types_do_not_match,
                Found => Y, Expected => VStrings_Set or Ints_Set);
          end case;
        else
          --  Shell_Execute (cmd);
          HAT_Procedure_Call (SP_Shell_Execute_without_Result);
        end if;
        Need (CD, RParent, err_closing_parenthesis_missing);

      when SP_Set_Exit_Status =>
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expression (CD, Level, Comma_RParent, X);  --  We push the argument in the stack.
        if X.TYP /= Ints then
          Skip (CD, Semicolon, err_parameter_must_be_Integer);
        end if;
        HAT_Procedure_Call (SP_Set_Exit_Status);
        Need (CD, RParent, err_closing_parenthesis_missing);

      when SP_Push_Abstract_Console =>
        null;  --  Internal: used by Get, Put, etc. without file parameter.
      when SP_Get_F | SP_Get_Line_F |
           SP_Put_F | SP_Put_Line_F =>
        null;  --  "Fronted" by SP_Get, SP_Get_Line,... Used by VM.
    end case;
  end Standard_Procedure;

end HAC_Sys.Parser.Standard_Procedures;
