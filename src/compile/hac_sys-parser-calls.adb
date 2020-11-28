with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Calls is

  use Compiler.PCode_Emit, Expressions, Helpers, PCode, Scanner, UErrors;

  procedure Push_and_Check_by_Value_Parameter (
    CD       : in out Compiler_Data;
    Level    :        HAC_Sys.PCode.Nesting_level;
    FSys     :        Symset;
    Expected :        Exact_Typ
  )
  is
    X : Exact_Typ;
  begin
    --  Expression does all the job of parsing and
    --  emitting the right "push" instructions.
    Expression (CD, Level, FSys + Colon_Comma_RParent, X);
    --  We just need to check types:
    if X.TYP = Expected.TYP then
      if X.Ref /= Expected.Ref then
        Type_Mismatch (CD, err_parameter_types_do_not_match, X, Expected);
      elsif X.TYP = Arrays then
        Emit1 (CD, k_Load_Block, CD.Arrays_Table (X.Ref).Array_Size);
      elsif X.TYP = Records then
        Emit1 (CD, k_Load_Block, CD.Blocks_Table (X.Ref).VSize);
      end if;
    elsif X.TYP = Ints and Expected.TYP = Floats then
      Forbid_Type_Coercion (CD, X, Expected);
      Emit1 (CD, k_Integer_to_Float, 0);  --  Left as a "souvenir" of SmallAda...
    elsif X.TYP /= NOTYP then
      Type_Mismatch (CD, err_parameter_types_do_not_match, X, Expected);
    end if;
  end Push_and_Check_by_Value_Parameter;

  procedure Push_by_Reference_Parameter (
    CD       : in out Compiler_Data;
    Level    :        HAC_Sys.PCode.Nesting_level;
    FSys     :        Symset;
    Found    :    out Exact_Typ  --  Funny note: Found is itself pushed by reference...
  )
  is
    K : Integer;
    F : Opcode;
  begin
    Found := Type_Undefined;
    if CD.Sy /= IDent then
      Error (CD, err_identifier_missing);
    else
      K := Locate_Identifier (CD, CD.Id, Level);
      InSymbol (CD);
      if K = No_Id then
        null;  --  Error already issued due to missing identifier
      elsif CD.IdTab (K).Obj /= Variable then
        Error (CD, err_variable_missing);
      elsif CD.IdTab (K).Read_only then
        Error (
          CD, err_cannot_modify_constant_or_in_parameter,
          ": passed to OUT or IN OUT parameter"
        );
      else
        Found := CD.IdTab (K).xTyp;
        if CD.IdTab (K).Normal then
          F := k_Push_Address;  --  Push "v'Access".
        else
          F := k_Push_Value;    --  Push "(v.all)'Access", that is, v (v is an access type).
        end if;
        Emit2 (CD, F, CD.IdTab (K).LEV, CD.IdTab (K).Adr_or_Sz);
        if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
          Selector (CD, Level, FSys + Colon_Comma_RParent, Found);
        end if;
      end if;
    end if;
  end Push_by_Reference_Parameter;

  ------------------------------------------------------------------
  -----------------------------------------Subprogram_or_Entry_Call-
  procedure Subprogram_or_Entry_Call (
    CD          : in out Compiler_Data;
    Level       :        HAC_Sys.PCode.Nesting_level;
    FSys        :        Symset;
    I           :        Integer;
    CallType    :        HAC_Sys.PCode.Operand_1_Type
  )
  is
    --****************************************************************
    --  Generate ObjCode for subprogram or Task Entry Call
    --  CallType specifies type of Call
    --    = 0 then standard subprogram Call,       CallSTDP
    --    = 1 then standard Task Entry Call,       CallSTDE
    --    = 2 then timed Task Entry Call,          CallTMDE
    --    = 3 then conditional Task Entry Call,    CallCNDE
    --****************************************************************
    Last_Param, CP : Integer;
    Found, Expected : Exact_Typ;
  begin
    Emit1 (CD, k_Mark_Stack, I);
    Last_Param := CD.Blocks_Table (CD.IdTab (I).Block_Ref).Last_Param_Id_Idx;
    CP    := I;
    if CD.Sy = LParent then  --  Actual parameter list
      loop
        InSymbol (CD);
        if CP >= Last_Param then
          Error (CD, err_number_of_parameters_do_not_match, ": too many actual parameters");
        else
          CP := CP + 1;
          Expected := CD.IdTab (CP).xTyp;
          if CD.IdTab (CP).Normal then
            --------------------------------------------------
            --  Value parameter (IN)                        --
            --  Currently we pass it only by value (copy).  --
            --------------------------------------------------
            Push_and_Check_by_Value_Parameter (CD, Level, FSys, Expected);
          else
            -----------------------------------------------
            --  Variable (Name) parameter (IN OUT, OUT)  --
            --  This is passed by reference              --
            -----------------------------------------------
            Push_by_Reference_Parameter (CD, Level, FSys, Found);
            if Found /= Expected then
              Type_Mismatch (CD, err_parameter_types_do_not_match, Found, Expected);
            end if;
          end if;
        end if;
        Test (CD, Comma_RParent, FSys, err_incorrectly_used_symbol);
        exit when CD.Sy /= Comma;
      end loop;
      Need (CD, RParent, err_closing_parenthesis_missing);
    end if;
    if CP < Last_Param then
      Error (CD, err_number_of_parameters_do_not_match, ": too few actual parameters");
    end if;
    --
    Emit2 (CD, k_Call, CallType, CD.Blocks_Table (CD.IdTab (I).Block_Ref).PSize - 1);
    if CallType /= Standard_Procedure_Call then  --  Some for of entry call
      Emit1 (CD, k_Exit_Call, Operand_2_Type (CallType));  --  Return from Entry Call
    end if;
    --
    if CD.IdTab (I).LEV < Level then
      Emit2 (CD, k_Update_Display_Vector, CD.IdTab (I).LEV, Operand_2_Type (Level));
    end if;
  end Subprogram_or_Entry_Call;

  ------------------------------------------------------------------
  -------------------------------------------------------Entry_Call-
  procedure Entry_Call (
    CD          : in out Compiler_Data;
    Level       :        Nesting_level;
    FSys        :        Symset;
    I           :        Integer;
    CallType    :        HAC_Sys.PCode.Operand_1_Type
  )
  is -- Hathorn
    Addr, J : Integer;
  begin
    if CD.Sy /= Period then
      Skip (CD, Semicolon, err_incorrectly_used_symbol);
    else
      InSymbol (CD);                  --  Task Entry Selector
      if CD.Sy /= IDent then
        Skip (CD, Semicolon, err_identifier_missing);
      else
        J := CD.Blocks_Table (CD.IdTab (I).Block_Ref).Last_Id_Idx;
        CD.IdTab (0).Name := CD.Id;
        while CD.IdTab (J).Name /= CD.Id loop
          J := CD.IdTab (J).Link;
        end loop;
        --
        if J = 0 then
          Error (CD, err_undefined_identifier);
        end if;
        --
        Addr := J;
        InSymbol (CD);
        Subprogram_or_Entry_Call (CD, Level, FSys, Addr, CallType);
      end if;
    end if;
  end Entry_Call;

end HAC_Sys.Parser.Calls;
