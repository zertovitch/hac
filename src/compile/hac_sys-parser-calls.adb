with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Calls is

  use Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;
  use type HAC_Integer;

  procedure Push_Parameter_by_Value
    (CD       : in out Co_Defs.Compiler_Data;
     level    :        Defs.Nesting_Level;
     fsys     :        Defs.Symset;
     expected :        Co_Defs.Exact_Subtyp)
  is
    X : Exact_Subtyp;
  begin
    --  Expression does all the job of parsing and, for
    --  atomic types, emitting the right "push" instructions.
    Expression (CD, level, fsys + Colon_Comma_RParent, X);
    --  What is left is:
    --    - checking types
    --    - for composite types, emit an instruction for pushing
    --        the contents on the stack.
    if X.TYP = expected.TYP then
      if X.Ref /= expected.Ref then
        Type_Mismatch (CD, err_parameter_types_do_not_match, X, expected);
      elsif X.TYP = Arrays then
        Emit_1 (CD, k_Load_Block, Operand_2_Type (CD.Arrays_Table (X.Ref).Array_Size));
      elsif X.TYP = Records then
        Emit_1 (CD, k_Load_Block, Operand_2_Type (CD.Blocks_Table (X.Ref).VSize));
      end if;
    elsif X.TYP = Ints and expected.TYP = Floats then
      Forbid_Type_Coercion (CD, X, expected);
      Emit_1 (CD, k_Integer_to_Float, 0);  --  Left as a "souvenir" of SmallAda...
    elsif X.TYP /= NOTYP then
      Type_Mismatch (CD, err_parameter_types_do_not_match, X, expected);
    end if;
  end Push_Parameter_by_Value;

  procedure Push_Parameter_by_Reference
    (CD       : in out Co_Defs.Compiler_Data;
     level    :        Defs.Nesting_Level;
     fsys     :        Defs.Symset;
     name     :        String;
     mode     :        Co_Defs.Parameter_Kind;
     found    :    out Co_Defs.Exact_Subtyp)
  is
    K : Integer;
  begin
    found := Undefined;
    if CD.Sy = IDent then
      K := Locate_CD_Id (CD, level);
      InSymbol (CD);
      if K = No_Id then
        null;  --  Error already issued due to undefined identifier
      elsif CD.IdTab (K).entity not in Object_Kind then
        Error (CD, err_variable_missing, name, severity => major);
      elsif CD.IdTab (K).entity = constant_object then
        Error
          (CD, err_cannot_modify_constant_or_in_parameter,
           ": passed to OUT or IN OUT parameter");
      else
        found := CD.IdTab (K).xtyp;

        --  Update the reference analysis for the variable.
        --  The concerned flags (.is_read, .is_written) are
        --  raised from `no` to `maybe`.
        --  The usage of the subprogram's parameters is
        --  checked at the end of the subprogram's compilation
        --  (see Formal_Parameter_List for startup values).
        --
        case mode is
          when param_in =>
            Raise_to_Maybe (CD.IdTab (K).is_read);
          when param_in_out =>
            Raise_to_Maybe (CD.IdTab (K).is_read);
            Raise_to_Maybe (CD.IdTab (K).is_written);
          when param_out =>
            Raise_to_Maybe (CD.IdTab (K).is_written);
        end case;

        Emit_2
          (CD,
           (if CD.IdTab (K).normal then
              k_Push_Address           --  Push "v'Access".
            else
              k_Push_Discrete_Value),  --  Push "(a.all)'Access", that is, a (a is an access type).
           Operand_1_Type (CD.IdTab (K).lev),
           Operand_2_Type (CD.IdTab (K).adr_or_sz));

        if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
          Selector (CD, level, fsys + Colon_Comma_RParent, found);
        end if;
      end if;
    else
      Error (CD, err_variable_missing, name, severity => major);
    end if;
  end Push_Parameter_by_Reference;

  ------------------------------------------------------------------
  -----------------------------------------Subprogram_or_Entry_Call-
  procedure Subprogram_or_Entry_Call
    (CD          : in out Co_Defs.Compiler_Data;
     level       :        Defs.Nesting_Level;
     fsys        :        Defs.Symset;
     ident_index :        Integer;
     call_type   :        PCode.Operand_1_Type)
  is
    --****************************************************************
    --  Generate ObjCode for subprogram or Task Entry Call
    --  CallType specifies type of Call
    --    = 0 then standard subprogram Call,       CallSTDP
    --    = 1 then standard Task Entry Call,       CallSTDE
    --    = 2 then timed Task Entry Call,          CallTMDE
    --    = 3 then conditional Task Entry Call,    CallCNDE
    --****************************************************************
    last_param, current_param : Index;
    found, expected : Exact_Subtyp;
    block_idx : Index;
  begin
    Emit_1 (CD, k_Mark_Stack, Operand_2_Type (ident_index));
    block_idx := CD.IdTab (ident_index).block_or_pkg_ref;
    current_param := CD.Blocks_Table (block_idx).First_Param_Id_Idx - 1;
    last_param    := CD.Blocks_Table (block_idx).Last_Param_Id_Idx;
    if CD.Sy = LParent then  --  Actual parameter list
      loop
        InSymbol (CD);
        if current_param >= last_param then
          Error
            (CD,
             err_number_of_parameters_do_not_match,
             ": too many actual parameters",
             severity => major);
        else
          current_param := current_param + 1;
          expected := CD.IdTab (current_param).xtyp;
          if CD.IdTab (current_param).normal then
            ------------------------------------------------------
            --  Value parameter                                 --
            --  Only IN mode; value is passed by value (copy).  --
            ------------------------------------------------------
            Push_Parameter_by_Value (CD, level, fsys, expected);
          else
            ------------------------------------
            --  Variable (Name) parameter     --
            --  This is passed by reference.  --
            ------------------------------------
            Push_Parameter_by_Reference
              (CD,
               level,
               fsys,
               A2S (CD.IdTab (current_param).name_with_case),
               CD.IdTab (current_param).decl_kind,
               found);
            if Exact_Typ (found) /= Exact_Typ (expected) then
              Type_Mismatch (CD, err_parameter_types_do_not_match, found, expected);
            end if;
          end if;
          if CD.Sy = Finger then
            Error
              (CD,
               err_not_yet_implemented,
               "positional association",
               severity => major);
          end if;
        end if;
        Test (CD, Comma_RParent, fsys, err_incorrectly_used_symbol);
        exit when CD.Sy /= Comma;
      end loop;
      Need (CD, RParent, err_closing_parenthesis_missing);
    end if;
    if current_param < last_param then
      Error
        (CD,
         err_number_of_parameters_do_not_match,
         ": too few actual parameters",
         severity => major);
    end if;
    --
    Emit_2 (CD, k_Call, call_type, Operand_2_Type (CD.Blocks_Table (CD.IdTab (ident_index).block_or_pkg_ref).PSize - 1));
    if call_type /= Normal_Procedure_Call then  --  Some for of entry call
      Emit_1 (CD, k_Exit_Call, Operand_2_Type (call_type));  --  Return from Entry Call
    end if;
    --
    if CD.IdTab (ident_index).lev < level then
      Emit_2 (CD,
        k_Update_Display_Vector,
        Operand_1_Type (CD.IdTab (ident_index).lev),
        Operand_2_Type (level)
      );
    end if;
  end Subprogram_or_Entry_Call;

  ------------------------------------------------------------------
  -------------------------------------------------------Entry_Call-
  procedure Entry_Call
    (CD          : in out Co_Defs.Compiler_Data;
     level       :        Defs.Nesting_Level;
     fsys        :        Defs.Symset;
     i           :        Integer;
     call_type   :        PCode.Operand_1_Type)
  is
    --  Hathorn
    Addr, J : Integer;
    use type Alfa;
  begin
    if CD.Sy = Period then
      InSymbol (CD);                  --  Task Entry Selector
      if CD.Sy = IDent then
        J := CD.Blocks_Table (CD.IdTab (i).block_or_pkg_ref).Last_Id_Idx;
        CD.IdTab (0).name := CD.Id;
        while CD.IdTab (J).name /= CD.Id loop
          J := CD.IdTab (J).link;
        end loop;
        --
        if J = 0 then
          Error (CD, err_undefined_identifier, A2S (CD.Id_with_case));
        end if;
        --
        Addr := J;
        InSymbol (CD);
        Subprogram_or_Entry_Call (CD, level, fsys, Addr, call_type);
      else
        Error_then_Skip (CD, Semicolon, err_identifier_missing);
      end if;
    else
      Error_then_Skip (CD, Semicolon, err_incorrectly_used_symbol);
    end if;
  end Entry_Call;

end HAC_Sys.Parser.Calls;
