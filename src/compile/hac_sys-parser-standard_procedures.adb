with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Calls,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Standard_Procedures is

  use Calls, Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;

  type Default_Extra_Put_Parameter_Type is array (Typen, 1 .. 3) of Integer;

  invalid : constant := -1;

  default_extra_put_param : constant Default_Extra_Put_Parameter_Type :=
    (Ints    =>  (IIO.Default_Width,   IIO.Default_Base,   invalid),
     Floats  =>  (RIO.Default_Fore,    RIO.Default_Aft,    RIO.Default_Exp),
     Bools   =>  (BIO.Default_Width,   invalid,            invalid),
     others  =>  (others => invalid));
  --  NB: for String_Literals on the target HAC VM, two values are pushed on the stack.

  procedure Standard_Procedure
    (CD      : in out Co_Defs.Compiler_Data;
     context : in     Defs.Flow_Context;
     FSys    : in     Defs.Symset;
     Code    : in     Defs.SP_Code)
  is

    procedure HAT_Procedure_Call (FIO_Code : SP_Code; Param : Operand_2_Type := 0) is
    begin
      CD.target.Emit_HAT_Builtin_Procedure (FIO_Code, Param);
    end HAT_Procedure_Call;

    procedure Set_Abstract_Console is
    begin
      HAT_Procedure_Call (SP_Push_Abstract_Console);
    end Set_Abstract_Console;

    procedure Need_Left_Parenthesis is
    begin
      Need (CD, LParent, err_missing_an_opening_parenthesis);
    end Need_Left_Parenthesis;

    procedure Need_Comma is
    begin
      Need (CD, Comma, err_COMMA_missing);
    end Need_Comma;

    procedure Need_Right_Parenthesis is
    begin
      Need (CD, RParent, err_closing_parenthesis_missing);
    end Need_Right_Parenthesis;

    procedure Parse_Gets (Code : SP_Code) is
      --  Parse Get & Co including an possible File parameter
      Found : Exact_Subtyp;
      with_file : Boolean;
      String_Length_Encoding : Operand_2_Type := 0;
      use type Operand_2_Type;
    begin
      Need_Left_Parenthesis;
      Push_Parameter_by_Reference (CD, context, FSys, "File", param_in_out, Found);
      with_file := Found.TYP = Text_Files;
      if with_file then
        --  We place the file handle's value on the stack:
        Emit (CD, k_Dereference);
        Need_Comma;
        --  Now the variable for "Get (file, variable);":
        Push_Parameter_by_Reference (CD, context, FSys, "", param_out, Found);
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
          ((if with_file then
              (if Code = SP_Get_Line then SP_Get_Line_File else SP_Get_File)
            else Code),
           Typen'Pos (Found.TYP) + String_Length_Encoding);
      else
        Error (CD, err_illegal_parameters_to_Get);
      end if;
      Need_Right_Parenthesis;
    end Parse_Gets;
    --
    procedure Parse_Puts (Code : SP_Code) is
      --  Parse Put & Co including an possible File parameter
      Item_Typ, Format_Param_Typ : Exact_Subtyp;
      Format_Params : Natural := 0;
      with_file : Boolean;
    begin
      Need_Left_Parenthesis;
      Expression (CD, context, FSys + Colon_Comma_RParent, Item_Typ);
      with_file := Item_Typ.TYP = Text_Files;
      if with_file then
        Need_Comma;
        Expression (CD, context, FSys + Colon_Comma_RParent, Item_Typ);
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
        In_Symbol (CD);
        Format_Params := Format_Params + 1;
        --  Here we parse:
        --    Width, Base    for Put ([F,] I [, Width [, Base]]);
        --    Fore, Aft, Exp for Put ([F,] R [, Fore[, Aft[, Exp]]]);
        --    Width          for Put ([F,] B [, Width]);
        Expression (CD, context, FSys + Colon_Comma_RParent, Format_Param_Typ);
        Check_Integer (CD, Format_Param_Typ.TYP);
      end loop;
      --  Check given / default parameters (nice short common solution, isn't it ?)
      for Param in 1 .. Format_Params loop
        --  First we check if the programmer didn't put too many
        --  (then, undefined) parameters.
        if default_extra_put_param (Item_Typ.TYP, Param) = invalid then
          Error (CD, err_illegal_parameters_to_Put);
        end if;
      end loop;
      if Item_Typ.TYP = String_Literals or else Is_Char_Array (CD, Item_Typ) then
        --  With String_Literals and String's, we have *two* values pushed on the stack.
        Format_Params := Format_Params + 1;
      end if;
      for Param in Format_Params + 1 .. 3 loop
        --  Send default parameters to the stack.
        exit when default_extra_put_param (Item_Typ.TYP, Param) = invalid;
        --  See Do_Write_Formatted on HAC VM or the code associated with SP_Put on
        --  other targets via Emit_HAT_Builtin_Procedure to have an idea on how
        --  everybody is retrieved from the stack.
        CD.target.Emit_Push_Discrete_Literal
          (Operand_2_Type (default_extra_put_param (Item_Typ.TYP, Param)));
      end loop;
      HAT_Procedure_Call
       ((if with_file then
           (if Code = SP_Put_Line then SP_Put_Line_File else SP_Put_File)
         else Code),
        Typen'Pos (Item_Typ.TYP));
      Need_Right_Parenthesis;
    end Parse_Puts;

    type Param_Passing_Mode is (by_value, by_reference);

    type Param is record
      mode : Param_Passing_Mode;
      kind : Parameter_Kind;
      typ  : Typen;
    end record;

    type Param_List is array (Positive range <>) of Param;

    procedure Parse_Call_to_Single_Profile_Procedure (pl : Param_List) is
      --  This fits non-overloaded procedures.
      X : Exact_Subtyp;
    begin
      if pl'Length = 0 then
        return;  --  No parameter
      else
        Need_Left_Parenthesis;
        for i in pl'Range loop
          --  Parse the passed parameter's variable (when by reference)
          --  or expression (when by value) and emit corresponding VM code
          --  that will have in fine an address or a value pushed on the stack.
          case pl (i).mode is
            when by_reference =>
              Push_Parameter_by_Reference (CD, context, FSys, "", param_in_out, X);
            when by_value =>
              Expression (CD, context, Comma_RParent, X);
          end case;
          --  Check the base type.
          if X.TYP /= pl (i).typ then
            Type_Mismatch
              (CD, err_parameter_types_do_not_match,
               Found => X, Expected => Singleton (pl (i).typ));
          end if;
          --  Parse the comma separating current parameter from next one.
          if i < pl'Last then
            Need_Comma;
          end if;
        end loop;
        Need_Right_Parenthesis;
      end if;
    end Parse_Call_to_Single_Profile_Procedure;

    procedure Parse_Call_to_Single_Profile_HAT_Procedure (pl : Param_List) is
    begin
      Parse_Call_to_Single_Profile_Procedure (pl);
      HAT_Procedure_Call (Code);
    end Parse_Call_to_Single_Profile_HAT_Procedure;

    procedure Push_Parameter_by_Reference_and_Check
      (name     : String;
       mode     : Co_Defs.Parameter_Kind;
       expected : Typen)
     is
       found : Co_Defs.Exact_Subtyp;
     begin
        Push_Parameter_by_Reference
          (CD, context, FSys + Colon_Comma_RParent, name, mode, found);
        if found.TYP /= expected then
          Type_Mismatch (CD, err_parameter_types_do_not_match, Found => found, Expected => expected);
        end if;
     end Push_Parameter_by_Reference_and_Check;

    X, Y, Z : Exact_Subtyp;

  begin
    case Code is
      when SP_Get | SP_Get_Immediate | SP_Get_Line =>
        Parse_Gets (Code);

      when SP_Put | SP_Put_Line =>
        Parse_Puts (Code);

      when SP_New_Line | SP_Skip_Line =>
        if CD.Sy = LParent then
          In_Symbol (CD);
          Expression (CD, context, FSys + Colon_Comma_RParent, X);
          case X.TYP is
            when Text_Files =>
              if CD.Sy = Comma then
                --  "New_Line (File, Spacing);"
                In_Symbol (CD);
                Expression (CD, context, FSys + RParent, Y);
                Check_Integer (CD, Y.TYP);
              else
                --  "New_Line (File);"
                Emit_1 (CD, k_Push_Discrete_Literal, 1);  --  Push default value, Spacing := 1
              end if;
            when Ints =>
              --  "New_Line (Spacing);"
              Set_Abstract_Console;
              Emit (CD, k_Swap);  --  File (the console) has to be before Spacing on the stack.
            when others =>
              Type_Mismatch (CD, err_parameter_types_do_not_match, Found => X, Expected => Text_Files_Set or Ints_Set);
          end case;
          Need_Right_Parenthesis;
        else  --  "New_Line;"
          Set_Abstract_Console;
          Emit_1 (CD, k_Push_Discrete_Literal, 1);  --  Push default value, Spacing := 1
        end if;
        HAT_Procedure_Call (Code);

      when SP_Randomize =>
        HAT_Procedure_Call (Code);

      when SP_Random_Seed =>
        Parse_Call_to_Single_Profile_HAT_Procedure ((1 => (by_value, param_in, Ints)));

      when SP_Wait | SP_Signal =>
        Parse_Call_to_Single_Profile_Procedure ((1 => (by_value, param_in, Ints)));
        if Code = SP_Wait then
          Emit (CD, k_Wait_Semaphore);
        else
          Emit (CD, k_Signal_Semaphore);
        end if;

      when SP_Open | SP_Create | SP_Append | SP_Close =>
        Need_Left_Parenthesis;
        Push_Parameter_by_Reference_and_Check ("File", param_in_out, Text_Files);
        if Code in SP_Open | SP_Create | SP_Append then
          --  Parse file name.
          Need_Comma;
          Expression (CD, context, FSys + Colon_Comma_RParent, X);
          Check_any_String_and_promote_to_VString (CD, X, False);
        end if;
        HAT_Procedure_Call (Code);
        Need_Right_Parenthesis;

      when SP_Quantum =>
        --  Cramer
        Parse_Call_to_Single_Profile_Procedure ((1 => (by_value, param_in, Floats)));
        Emit (CD, k_Set_Quantum_Task);

      when SP_Priority =>
        --  Cramer
        Parse_Call_to_Single_Profile_Procedure ((1 => (by_value, param_in, Ints)));
        Emit (CD, k_Set_Task_Priority);

      when SP_InheritP =>
        --  Cramer
        Parse_Call_to_Single_Profile_Procedure ((1 => (by_value, param_in, Bools)));
        Emit (CD, k_Set_Task_Priority_Inheritance);
        --
      when SP_Set_Env | SP_Set_VM_Variable | SP_Copy_File | SP_Rename =>
        Need_Left_Parenthesis;
        for arg in 1 .. 2 loop
          Expression (CD, context, Colon_Comma_RParent, X);  --  We push the arguments in the stack.
          --  Set_Env ( "HAC_Var",  "Hello");     <-  2 String's
          --  Set_Env (+"HAC_Var", +"Hello");     <-  2 VString's
          --  Set_Env (+"HAC_Var",  "Hello");
          --  Set_Env ( "HAC_Var", +"Hello");
          Check_any_String_and_promote_to_VString (CD, X, False);
          if arg < 2 then
            Need_Comma;
          end if;
        end loop;
        HAT_Procedure_Call (Code);
        Need_Right_Parenthesis;

      when SP_Create_Directory | SP_Create_Path | SP_Delete_Directory |
           SP_Delete_File | SP_Set_Directory
           =>
        Need_Left_Parenthesis;
        Expression (CD, context, RParent_Set, X);  --  We push the argument in the stack.
        Check_any_String_and_promote_to_VString (CD, X, False);
        HAT_Procedure_Call (Code);
        Need_Right_Parenthesis;

      when SP_Start_Search =>
        Need_Left_Parenthesis;
        Push_Parameter_by_Reference_and_Check ("Search", param_in_out,  Search_Types);
        Need_Comma;
        --  Directory, Pattern:
        for arg in 1 .. 2 loop
          Expression (CD, context, Colon_Comma_RParent, X);
          Check_any_String_and_promote_to_VString (CD, X, False);
          Need_Comma;
        end loop;
        Expression (CD, context, Colon_Comma_RParent, X);
        Emit_1 (CD, k_Load_Block, Operand_2_Type (CD.Arrays_Table (X.Ref).Array_Size));
        --  Filter:
        if X.TYP /= Arrays or else X.Ref /= CD.id_table (CD.built_in.hat_filter_type_index).xtyp.Ref then
          --  Must be the array definition of HAT.Filter_Type, not even an identical type.
          Type_Mismatch
            (CD,
             err_parameter_types_do_not_match,
             Found => X,
             Expected => CD.id_table (CD.built_in.hat_filter_type_index).xtyp);
        end if;
        HAT_Procedure_Call (Code);
        Need_Right_Parenthesis;

      when SP_End_Search =>
        Parse_Call_to_Single_Profile_HAT_Procedure ((1 => (by_reference, param_in_out, Search_Types)));

      when SP_Get_Next_Entry =>
        Parse_Call_to_Single_Profile_HAT_Procedure
          (((by_reference, param_in_out, Search_Types),
            (by_reference, param_out,    Directory_Entry_Types)));

      when SP_Shell_Execute =>
        Need_Left_Parenthesis;
        Expression (CD, context, Comma_RParent, X);  --  We push the argument in the stack.
        Check_any_String_and_promote_to_VString (CD, X, False);
        --  ` Shell_Execute (cmd `  has been parsed at this point.
        if CD.Sy = Comma then
          In_Symbol (CD);
          Push_Parameter_by_Reference
            (CD, context, RParent_Set, "Command", param_in_out, Y);
          case Y.TYP is
            when VStrings =>
              --  Shell_Execute (cmd, output);
              HAT_Procedure_Call (SP_Shell_Execute_Output);
            when Ints =>
              if CD.Sy = Comma then
                In_Symbol (CD);
                Push_Parameter_by_Reference
                  (CD, context, RParent_Set, "Result", param_out, Z);
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
        Need_Right_Parenthesis;

      when SP_Set_Exit_Status =>
        Parse_Call_to_Single_Profile_HAT_Procedure
          ((1 => (by_value, param_in, Ints)));  --  Exit code

      when SP_Delete =>
        Parse_Call_to_Single_Profile_HAT_Procedure
          (((by_reference, param_in_out, VStrings),  --  Source
            (by_value,     param_in,     Ints),      --  From
            (by_value,     param_in,     Ints)));    --  Through

      when SP_Push_Abstract_Console =>
        null;  --  Internal: used by Get, Put, etc. without file parameter.

      when SP_Get_File | SP_Get_Line_File |
           SP_Put_File | SP_Put_Line_File =>
        --  Theses cases are "fronted" by SP_Get, SP_Get_Line,...
        --  They are used by the VM.
        null;
    end case;

  end Standard_Procedure;

end HAC_Sys.Parser.Standard_Procedures;
