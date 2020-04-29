with HAC.Compiler;           use HAC.Compiler;
with HAC.Parser.Expressions; use HAC.Parser.Expressions;
with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser.Calls is

  ------------------------------------------------------------------
  -----------------------------------------Subprogram_or_Entry_Call-
  procedure Subprogram_or_Entry_Call (
    CD          : in out HAC.Compiler.Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    I, CallType :        Integer
  )
  is
    --****************************************************************
    -- Generate ObjCode for subprogram or Task Entry Call
    -- CallType specifies type of Call
    --   = 0 then standard subprogram Call,       CallSTDP
    --   = 1 then standard Task Entry Call,       CallSTDE
    --   = 2 then timed Task Entry Call,          CallTMDE
    --   = 3 then conditional Task Entry Call,    CallCNDE
    --****************************************************************
    X            : Exact_Type;
    LastP, CP, K : Integer;
  begin
    Emit1 (CD, k_Mark_Stack, I);
    LastP := CD.Blocks_Table (CD.IdTab (I).Ref).LastPar;
    CP    := I;
    if CD.Sy = LParent then  --  Actual parameter list
      loop
        InSymbol (CD);
        if CP >= LastP then
          Error (CD, err_number_of_parameters_do_not_match, ": too many actual parameters");
        else
          CP := CP + 1;
          if CD.IdTab (CP).Normal then           --  Value parameter (IN)
            Expression (CD, Level, FSys + Colon_Comma_RParent, X);
            if X.TYP = CD.IdTab (CP).TYP then
              if X.Ref /= CD.IdTab (CP).Ref then
                Error (CD, err_parameter_types_do_not_match);
              elsif X.TYP = Arrays then
                Emit1 (CD, k_Load_Block, CD.Arrays_Table (X.Ref).Size);
              elsif X.TYP = Records then
                Emit1 (CD, k_Load_Block, CD.Blocks_Table (X.Ref).VSize);
              end if;
            elsif X.TYP = Ints and CD.IdTab (CP).TYP = Floats then
              Forbid_Type_Coercion (CD, "value is integer, parameter is floating-point");
              Emit1 (CD, k_Integer_to_Float, 0);
            elsif X.TYP /= NOTYP then
              Error (CD, err_parameter_types_do_not_match);
            end if;
          else              -- Variable (Name) parameter (IN OUT, OUT)
            if CD.Sy /= IDent then
              Error (CD, err_identifier_missing);
            else
              K := Locate_Identifier (CD, CD.Id, Level);
              InSymbol (CD);
              if K = 0 then
                null;  --  Error already issued due to missing identifier
              elsif CD.IdTab (K).Obj /= Variable then
                Error (CD, err_variable_missing);
              elsif CD.IdTab (K).Read_only then
                Error (
                  CD, err_cannot_modify_constant_or_in_parameter,
                  ": passed to OUT or IN OUT parameter"
                );
              else
                X.TYP := CD.IdTab (K).TYP;
                X.Ref := CD.IdTab (K).Ref;
                if CD.IdTab (K).Normal then
                  Emit2 (CD, k_Load_Address, CD.IdTab (K).LEV, CD.IdTab (K).Adr);
                else
                  Emit2 (CD, k_Push_Value, CD.IdTab (K).LEV, CD.IdTab (K).Adr);
                end if;
                 if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                  Selector (CD, Level, FSys + Colon_Comma_RParent, X);
                end if;
                if (X.TYP /= CD.IdTab (CP).TYP) or
                   (X.Ref /= CD.IdTab (CP).Ref)
                then
                  Error (CD, err_parameter_types_do_not_match);
                end if;
              end if;
            end if;
          end if;
        end if;
        Test (CD, Comma_RParent, FSys, err_incorrectly_used_symbol);
        exit when CD.Sy /= Comma;
      end loop;
      Need (CD, RParent, err_closing_parenthesis_missing);
    end if;
    if CP < LastP then
      Error (CD, err_number_of_parameters_do_not_match, ": too few actual parameters");
    end if;
    --
    Emit2 (CD, k_Call, CallType, CD.Blocks_Table (CD.IdTab (I).Ref).PSize - 1);
    if CallType /= CallSTDP then  --  Some for of entry call
      Emit1 (CD, k_Exit_Call, CallType);  --  Return from Entry Call
    end if;
    --
    if CD.IdTab (I).LEV < Level then
      Emit2 (CD, k_Update_Display_Vector, CD.IdTab (I).LEV, Level);
    end if;
  end Subprogram_or_Entry_Call;

  ------------------------------------------------------------------
  -------------------------------------------------------Entry_Call-
  procedure Entry_Call (
    CD          : in out HAC.Compiler.Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    I, CallType :        Integer
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
        J                 := CD.Blocks_Table (CD.IdTab (I).Ref).Last;
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

end HAC.Parser.Calls;
