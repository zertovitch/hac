with HAC.Parser.Calls,
     HAC.Parser.Enter_Def,
     HAC.Parser.Expressions,
     HAC.Parser.Helpers,
     HAC.Parser.Standard_Procedures,
     HAC.Parser.Tasking,
     HAC.Scanner,
     HAC.UErrors;

package body HAC.Parser is

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block (
    CD                   : in out Compiler.Compiler_Data;
    FSys                 :        Data.Symset;
    Is_a_function        :        Boolean;        --  RETURN [Value] statement expected
    Is_a_block_statement :        Boolean;        --  5.6 Block Statements
    Initial_Level        :        PCode.Nesting_level;
    Prt                  :        Integer;
    Block_ID             :        Data.Alfa;  --  Name of this block (if any)
    Block_ID_with_case   :        Data.Alfa
  )
  is
    use Calls, Compiler, Data, Enter_Def, Expressions, Helpers, PCode, UErrors;
    --
    Level : Nesting_level := Initial_Level;
    procedure InSymbol is begin Scanner.InSymbol (CD); end;

    Dx      : Integer;  -- data allocation Index
    MaxDX   : Integer;
    PRB     : Integer;  -- B-Index of this procedure
    ICode   : Integer;  -- Size of initialization ObjCode generated

    ------------------------------------------------------------------
    ----------------------------------Number_Declaration_or_Enum_Item-
    procedure Number_Declaration_or_Enum_Item (FSys_ND : Symset; C : out Constant_Rec) is
      --  This covers number declarations (RM 3.3.2) and enumeration items (RM 3.5.1).
      --  Additionally this compiler does on-the-fly declarations for static values:
      --  bounds in ranges (FOR, ARRAY), and values in CASE statements.
      --  Was: Constant in the Pascal compiler.
      X, Sign : Integer;
      signed : Boolean := False;
    begin
      C.TP := Type_Undefined;
      C.I  := 0;
      Test (CD, Constant_Definition_Begin_Symbol, FSys_ND, err_illegal_symbol_for_a_number_declaration);
      if not Constant_Definition_Begin_Symbol (CD.Sy) then
        return;
      end if;
      if CD.Sy = CharCon then  --  Untyped character constant, occurs only in ranges.
        C.TP := (Chars, 0);
        C.I  := CD.INum;
        InSymbol;
      else
        Sign := 1;
        if Plus_Minus (CD.Sy) then
          signed := True;
          if CD.Sy = Minus then
            Sign := -1;
          end if;
          InSymbol;
        end if;
        if CD.Sy = IDent then
          --  Number defined using another one: "minus_pi : constant := -pi;"
          --  ... or, we have an enumeration item.
          X := Locate_Identifier (CD, CD.Id, Level);
          if X /= 0 then
            if CD.IdTab (X).Obj /= Declared_Number_or_Enum_Item then
              Error (CD, err_illegal_constant_or_constant_identifier);
            else
              C.TP := CD.IdTab (X).xTyp;
              if C.TP.TYP = Floats then
                C.R := HAC_Float (Sign) * CD.Float_Constants_Table (CD.IdTab (X).Adr_or_Sz);
              else
                C.I := Sign * CD.IdTab (X).Adr_or_Sz;
                if signed and then C.TP.TYP not in Numeric_Typ then
                  Error (CD, err_numeric_constant_expected);
                end if;
              end if;
            end if;
          end if;  --  X /= 0
          InSymbol;
        elsif CD.Sy = IntCon then
          C.TP := (Ints, 0);
          C.I  := Sign * CD.INum;
          InSymbol;
        elsif CD.Sy = FloatCon then
          C.TP := (Floats, 0);
          C.R  := HAC_Float (Sign) * CD.RNum;
          InSymbol;
        else
          Skip (CD, FSys_ND, err_illegal_symbol_for_a_number_declaration);
        end if;
      end if;
      Test (CD, FSys_ND, Empty_Symset, err_incorrectly_used_symbol);
    end Number_Declaration_or_Enum_Item;

    ------------------------------------------------------------------
    -----------------------------------Type_Definition - RM 3.2.1 (4)-

    procedure Type_Definition (FSys_TD : Symset; xTP : out Exact_Typ; Sz : out Integer) is

      procedure Array_Typ (
        Arr_Tab_Ref, Arr_Size      : out Integer;
        String_Constrained_Subtype :     Boolean
      )
      is
        Element_Exact_Typ : Exact_Typ;
        Element_Size      : Integer;
        Lower_Bound,
        Higher_Bound      : Constant_Rec;
      begin
        Number_Declaration_or_Enum_Item (OF_RANGE_Double_Dot_RParent + FSys_TD, Lower_Bound);
        --
        if Lower_Bound.TP.TYP = Floats then
          Error (CD, err_illegal_array_bounds, "a float type is not expected for a bound");
          Lower_Bound.TP := (Ints, 0);
          Lower_Bound.I  := 0;
        end if;
        Need (CD, Range_Double_Dot_Symbol, err_expecting_double_dot);
        --
        Number_Declaration_or_Enum_Item (Comma_OF_RParent + FSys_TD, Higher_Bound);
        --
        if Higher_Bound.TP /= Lower_Bound.TP then
          Error (CD, err_illegal_array_bounds, "bound types do not match");
          Higher_Bound.I := Lower_Bound.I;
        end if;
        Enter_Array (CD, Lower_Bound.TP, Lower_Bound.I, Higher_Bound.I);
        Arr_Tab_Ref := CD.Arrays_Count;
        if String_Constrained_Subtype then
          --  We define String (L .. H) exactly as an "array (L .. H) of Character".
          Element_Exact_Typ := (TYP => Chars, Ref => 0);
          Element_Size := 1;
          Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
        elsif CD.Sy = Comma then
          --  Multidimensional array is equivalant to:  array (range_1) of array (range_2,...).
          InSymbol;  --  Consume ',' symbol.
          Element_Exact_Typ.TYP := Arrays;  --  Recursion for next array dimension.
          Array_Typ (Element_Exact_Typ.Ref, Element_Size, False);
        else
          Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
          Need (CD, OF_Symbol, err_missing_OF);         --  "of"  in  "array (...) of Some_Type"
          Type_Definition (FSys_TD, Element_Exact_Typ, Element_Size);  --  "Some_Type"
        end if;
        Arr_Size := (Higher_Bound.I - Lower_Bound.I + 1) * Element_Size;
        declare
          New_A : ATabEntry renames CD.Arrays_Table (Arr_Tab_Ref);
        begin
          New_A.Array_Size   := Arr_Size;  --  Index_xTyp, Low, High already set by Enter_Array.
          New_A.Element_xTyp := Element_Exact_Typ;
          New_A.Element_Size := Element_Size;
        end;
      end Array_Typ;

      procedure Enumeration_Typ is  --  RM 3.5.1 Enumeration Types
        enum_count : Natural := 0;
      begin
        xTP := (TYP => Enums, Ref => CD.Id_Count);
        loop
          InSymbol;  --  Consume '(' symbol.
          if CD.Sy = IDent then
            enum_count := enum_count + 1;
            Enter (CD, Level, CD.Id, CD.Id_with_case, Declared_Number_or_Enum_Item);
            declare
              New_Enum_Item : IdTabEntry renames CD.IdTab (CD.Id_Count);
            begin
              New_Enum_Item.Read_only := True;
              New_Enum_Item.xTyp      := xTP;
              New_Enum_Item.Adr_or_Sz := enum_count - 1;  --  RM 3.5.1 (7): position begins with 0.
            end;
          else
            Error (CD, err_identifier_missing);
          end if;
          InSymbol;
          exit when CD.Sy /= Comma;
        end loop;
        Sz := 1;  --  Was: enum_count -> caused stack corruption on calls to P (e: in Enum).
        --  TBD : store enum_count somewhere, for T'Last.
        Need (CD, RParent, err_closing_parenthesis_missing);
      end Enumeration_Typ;

      procedure Record_Typ is
        Field_Exact_Typ : Exact_Typ;
        Field_Size, Offset, T0, T1 : Integer;
      begin
        InSymbol;  --  Consume RECORD symbol.
        Enter_Block (CD, CD.Id_Count);
        xTP := (TYP => Records, Ref => CD.Blocks_Count);
        if Level = Nesting_Level_Max then
          Fatal (LEVELS);  --  Exception is raised there.
        end if;
        Level              := Level + 1;
        CD.Display (Level) := CD.Blocks_Count;
        Offset             := 0;
        --
        loop
          if CD.Sy /= IDent then
            Error (CD, err_identifier_missing, stop_on_error => True);
          else  --  RM 3.8 Component declaration
            T0 := CD.Id_Count;
            Enter_Variables (CD, Level);
            Need (CD, Colon, err_colon_missing);  --  ':'  in  "a, b, c : Integer;"
            T1 := CD.Id_Count;
            Type_Definition (FSys_TD + Comma_END_IDent_Semicolon, Field_Exact_Typ, Field_Size);
            while T0 < T1 loop
              T0                      := T0 + 1;
              CD.IdTab (T0).xTyp      := Field_Exact_Typ;
              CD.IdTab (T0).Adr_or_Sz := Offset;
              Offset                  := Offset + Field_Size;
            end loop;
          end if;
          Need (CD, Semicolon, err_semicolon_missing, Forgive => Comma);
          Ignore_Extra_Semicolons (CD);
          exit when CD.Sy = END_Symbol;
        end loop;
        --
        CD.Blocks_Table (xTP.Ref).VSize := Offset;
        Sz                              := Offset;
        CD.Blocks_Table (xTP.Ref).PSize := 0;
        InSymbol;
        Need (CD, RECORD_Symbol, err_RECORD_missing);  --  (END) RECORD
        Level := Level - 1;
      end Record_Typ;

      procedure String_Sub_Typ is
        --  Prototype of constraining an array type: String -> String (1 .. 26)
        --  !! Need to implement general constraints...
      begin
        InSymbol;
        Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
        xTP.TYP := Arrays;
        Array_Typ (xTP.Ref, Sz, String_Constrained_Subtype => True);
      end String_Sub_Typ;

      procedure Sub_Typ is
        Ident_Index : constant Integer := Locate_Identifier (CD, CD.Id, Level);
      begin
        if Ident_Index /= No_Id then
          if CD.IdTab (Ident_Index).Obj = TypeMark then
            xTP := CD.IdTab (Ident_Index).xTyp;
            Sz  := CD.IdTab (Ident_Index).Adr_or_Sz;
            if xTP.TYP = NOTYP then
              Error (CD, err_undefined_type);
            end if;
          else
            Error (CD, err_missing_a_type_identifier);
          end if;
        end if;
        InSymbol;
      end Sub_Typ;

    begin
      xTP := Type_Undefined;
      Sz := 0;
      Test (CD, Type_Begin_Symbol, FSys_TD, err_missing_ARRAY_RECORD_or_ident);
      if Type_Begin_Symbol (CD.Sy) then
        case CD.Sy is
          when IDent =>
            if Equal (CD.Id, "STRING") then
              String_Sub_Typ;
            else
              Sub_Typ;
            end if;
          when ARRAY_Symbol =>
            InSymbol;
            Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
            xTP.TYP := Arrays;
            Array_Typ (xTP.Ref, Sz, String_Constrained_Subtype => False);
          when RECORD_Symbol =>
            Record_Typ;
          when LParent =>
            Enumeration_Typ;
          when others =>
            null;
        end case;  --  CD.Sy
        Test (CD, FSys_TD, Empty_Symset, err_incorrectly_used_symbol);
      end if;
    end Type_Definition;

    ------------------------------------------------------------------
    --------------------------------------------Formal_Parameter_List-
    procedure Formal_Parameter_List is
      Sz, X, T0 : Integer;
      ValParam  : Boolean;
      xTP       : Exact_Typ := Type_Undefined;
    begin
      InSymbol;  --  Consume '(' symbol.
      Sz := 0;
      Test (CD, IDent_Set, FSys + RParent, err_identifier_missing, stop_on_error => True);
      --
      while CD.Sy = IDent loop
        T0 := CD.Id_Count;
        Enter_Variables (CD, Level);
        --
        if CD.Sy = Colon then  --  The ':'  in  "function F (x, y : in Real) return Real;"
          InSymbol;
          if CD.Sy = IN_Symbol then
            InSymbol;
          end if;
          if Is_a_function then  --  If I am a function, no InOut parms allowed
            ValParam := True;
          elsif CD.Sy /= OUT_Symbol then
            ValParam := True;
          else
            InSymbol;
            ValParam := False;
          end if;
          if CD.Sy /= IDent then
            Error (CD, err_identifier_missing);
          else
            X := Locate_Identifier (CD, CD.Id, Level);
            InSymbol;
            if X /= No_Id then
              if CD.IdTab (X).Obj = TypeMark then
                xTP := CD.IdTab (X).xTyp;
                if ValParam then
                  Sz := CD.IdTab (X).Adr_or_Sz;
                else
                  Sz := 1;
                end if;
              else
                Error (CD, err_missing_a_type_identifier, stop_on_error => True);
              end if;
            end if;  --  X /= No_Id
          end if;
          Test (CD, Comma_IDent_RParent_Semicolon, FSys, err_semicolon_missing, stop_on_error => True);
          while T0 < CD.Id_Count loop
            T0 := T0 + 1;
            declare
              r : IdTabEntry renames CD.IdTab (T0);
            begin
              r.xTyp      := xTP;
              r.Normal    := ValParam;
              r.Read_only := ValParam;
              r.Adr_or_Sz := Dx;
              r.LEV       := Level;
              Dx          := Dx + Sz;
            end;
          end loop;  --  while T0 < CD.Id_Count
        else
          Error (CD, err_colon_missing, stop_on_error => True);
        end if;
        if CD.Sy /= RParent then
          Need (CD, Semicolon, err_semicolon_missing, Forgive => Comma);
          Ignore_Extra_Semicolons (CD);
          Test (CD, IDent_Set, FSys + RParent, err_incorrectly_used_symbol);
        end if;
      end loop;  --  while Sy = IDent
      --
      if CD.Sy = RParent then
        InSymbol;
        Test (CD, After_Subprogram_Parameters, FSys, err_incorrectly_used_symbol);
      else
        Error (CD, err_closing_parenthesis_missing);
      end if;
    end Formal_Parameter_List;

    ------------------------------------------------------------------
    -------------------------------------------------Type_Declaration-
    procedure Type_Declaration is
      xTyp : Exact_Typ;
      Sz, T1 : Integer;
    begin
      InSymbol;
      Test (CD, IDent_Set, Semicolon_Set, err_identifier_missing);
      Enter (CD, Level, CD.Id, CD.Id_with_case, TypeMark);
      T1 := CD.Id_Count;
      InSymbol;
      Need (CD, IS_Symbol, err_IS_missing);
      xTyp := Type_Undefined;
      Sz := 0;
      Type_Definition (Comma_IDent_Semicolon + FSys, xTyp, Sz);
      CD.IdTab (T1).xTyp      := xTyp;
      CD.IdTab (T1).Adr_or_Sz := Sz;
      --
      Test_Semicolon (CD, FSys);
    end Type_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer; Check_read_only : Boolean);

    ------------------------------------------------------------------
    --------------------------------------------------Var_Declaration-
    procedure Var_Declaration is
      --  This procedure processes both Variable and Constant declarations.
      procedure Single_Var_Declaration is
        T0, T1, Sz, T0i, LC0, LC1 : Integer;
        xTyp                      : Exact_Typ;
        is_constant, is_typed,
        is_untyped_constant       : Boolean;
        C                         : Constant_Rec;
        I_dummy                   : Integer;
      begin
        T0 := CD.Id_Count;
        Enter_Variables (CD, Level);
        Need (CD, Colon, err_colon_missing);  --  ':'   in   "x, y : Integer;"
        T1 := CD.Id_Count;
        --
        if CD.Sy = IDent then
          I_dummy := Locate_Identifier (CD, CD.Id, Level, stop_on_error => True);
        end if;
        Test (CD, Type_Begin_Symbol + CONSTANT_Symbol, Semicolon_Set, err_incorrectly_used_symbol);
        --
        is_constant := False;
        if CD.Sy = CONSTANT_Symbol then  --  Consume "constant" in "x : constant ...;"
          is_constant := True;
          InSymbol;
        end if;
        --
        is_typed := False;
        if Type_Begin_Symbol (CD.Sy) then  --  Here, a type name or an anonymous type definition
          is_typed := True;
          Type_Definition (Becomes_Comma_IDent_Semicolon + FSys, xTyp, Sz);
        end if;
        Test (CD, Becomes_EQL_Semicolon, Empty_Symset, err_incorrectly_used_symbol);
        --
        if CD.Sy = EQL then
          Error (CD, err_EQUALS_instead_of_BECOMES);
          CD.Sy := Becomes;
        end if;
        --
        is_untyped_constant := is_constant and not is_typed;
        --
        if is_untyped_constant then
          --  Numeric constant: we parse the number here ("k : constant := 123.0").
          if CD.Sy = Becomes then
            InSymbol;
            Number_Declaration_or_Enum_Item (Comma_IDent_Semicolon + FSys, C);
          else
            Error (CD, err_BECOMES_missing);
          end if;
        end if;
        --
        T0i := T0;
        if is_constant or is_typed then  --  All correct cases: untyped variables were caught.
          --  Update identifier table
          while T0 < T1 loop
            T0 := T0 + 1;
            declare
              r : IdTabEntry renames CD.IdTab (T0);
            begin
              r.Read_only := is_constant;
              if is_untyped_constant then
                r.Obj := Declared_Number_or_Enum_Item;  --  r was initially a Variable.
                r.xTyp := C.TP;
                case C.TP.TYP is
                  when Floats =>
                    Enter_or_find_Float (CD, C.R, r.Adr_or_Sz);
                  when Ints =>
                    r.Adr_or_Sz := C.I;
                  when others =>
                    Error (CD, err_numeric_constant_expected);
                    --  "boo : constant := True;" or "x: constant := 'a';" are wrong in Ada.
                    r.Adr_or_Sz := C.I;
                end case;
              else  --  A variable or a typed constant
                r.xTyp      := xTyp;
                r.Adr_or_Sz := Dx;
                Dx          := Dx + Sz;
              end if;
            end;
          end loop;  --  While T0 < T1
        end if;
        --
        if is_constant and is_typed then
          --  For typed constants, the ":=" is required and consumed with the Assignment below.
          Test (CD, Becomes_EQL, Empty_Symset, err_BECOMES_missing);
          if CD.Sy = EQL then
            Error (CD, err_EQUALS_instead_of_BECOMES);
            CD.Sy := Becomes;
          end if;
        end if;
        --
        if CD.Sy = Becomes and not is_untyped_constant then
          --  Create constant or variable initialization ObjCode
          LC0 := CD.LC;
          Assignment (T1, Check_read_only => False);
          T0 := T0i;
          while T0 < T1 - 1 loop
            T0 := T0 + 1;
            Emit2 (CD, k_Push_Address, Operand_1_Type (CD.IdTab (T0).LEV), CD.IdTab (T0).Adr_or_Sz);
            Emit2 (CD, k_Push_Value,   Operand_1_Type (CD.IdTab (T1).LEV), CD.IdTab (T1).Adr_or_Sz);
            Emit (CD, k_Store);
          end loop;
          --
          LC1 := CD.LC;
          --  reset ObjCode pointer as if ObjCode had not been generated
          CD.LC := LC0;
          --  Copy ObjCode to end of ObjCode table in reverse order
          ICode := ICode + (LC1 - LC0);  --  Size of initialization ObjCode
          while LC0 < LC1 loop
            CD.ObjCode (CD.CMax) := CD.ObjCode (LC0);
            CD.CMax              := CD.CMax - 1;
            LC0                  := LC0 + 1;
          end loop;
        end if;
        Test_Semicolon (CD, FSys);
      end Single_Var_Declaration;
    begin
      while CD.Sy = IDent loop
        Single_Var_Declaration;
      end loop;
    end Var_Declaration;

    ------------------------------------------------------------------
    --------------------------------------------Proc_Func_Declaration-
    procedure Proc_Func_Declaration is
      IsFun: constant Boolean := CD.Sy = FUNCTION_Symbol;
    begin
      InSymbol;
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
        CD.Id := Empty_Alfa;
      end if;
      declare
        Id_subprog_with_case : constant Alfa := CD.Id_with_case;
      begin
        if IsFun then
          Enter (CD, Level, CD.Id, CD.Id_with_case, Funktion);
        else
          Enter (CD, Level, CD.Id, CD.Id_with_case, Prozedure);
        end if;
        InSymbol;
        Block (CD, FSys, IsFun, False, Level + 1, CD.Id_Count,
               CD.IdTab (CD.Id_Count).Name, Id_subprog_with_case);
      end;
      if IsFun then
        Emit1 (CD, k_Exit_Function, End_Function_without_Return);
      else
        Emit1 (CD, k_Exit_Call, CallSTDP);
      end if;
    end Proc_Func_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer; Check_read_only : Boolean) is
      X, Y : Exact_Typ;
      F    : Opcode;
      procedure Issue_Type_Mismatch_Error is
      begin
        Type_Mismatch (CD, err_types_of_assignment_must_match, Found => Y, Expected => X);
      end Issue_Type_Mismatch_Error;
    begin
      pragma Assert (CD.IdTab (I).Obj = Variable);
      X := CD.IdTab (I).xTyp;
      if CD.IdTab (I).Normal then
        F := k_Push_Address;
      else
        F := k_Push_Value;
      end if;
      Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr_or_Sz);
      if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
        Selector (CD, Level, Becomes_EQL + FSys, X);
      end if;
      if CD.Sy = Becomes then
        InSymbol;
      elsif CD.Sy = EQL then
        --  Common mistake by BASIC or C programmers.
        Error (CD, err_EQUALS_instead_of_BECOMES);
        InSymbol;
      else
        Error (CD, err_BECOMES_missing);
      end if;
      if Check_read_only and then CD.IdTab (I).Read_only then
        Error (CD, err_cannot_modify_constant_or_in_parameter);
      end if;
      Expression (CD, Level, Semicolon_Set, Y);
      if X.TYP = Y.TYP then
        if X.TYP in Standard_Typ then
          Emit (CD, k_Store);
        elsif X.Ref /= Y.Ref then
          Issue_Type_Mismatch_Error;
        else
          case X.TYP is
            when Arrays =>
              Emit1 (CD, k_Copy_Block, CD.Arrays_Table (X.Ref).Array_Size);
            when Records =>
              Emit1 (CD, k_Copy_Block, CD.Blocks_Table (X.Ref).VSize);
            when Enums =>
              --  Behaves like a "Standard_Typ".
              --  We have checked that X.Ref = Y.Ref (same actual type).
              Emit (CD, k_Store);
            when others =>
              raise Internal_error with "Assignment: X := Y on unsupported Typ ?";
          end case;
        end if;
      elsif X.TYP = Floats and Y.TYP = Ints then
        Forbid_Type_Coercion (CD, "integer value assigned to floating-point variable");
        Emit1 (CD, k_Integer_to_Float, 0);
        Emit (CD, k_Store);
      elsif Is_Char_Array (CD, X) and Y.TYP = String_Literals then
        Emit1 (CD, k_String_Literal_Assignment, CD.Arrays_Table (X.Ref).Array_Size);
      elsif X.TYP = VStrings and then (Y.TYP = String_Literals or else Is_Char_Array (CD, Y)) then
        Error (CD, err_string_to_vstring_assignment);
      elsif X.TYP = NOTYP then
        if CD.Err_Count = 0 then
          raise Internal_error with "Assignment: assigned variable (X) is typeless";
        else
          null;  --  All right, there were already enough compilation error messages...
        end if;
      elsif Y.TYP = NOTYP then
        if CD.Err_Count = 0 then
          raise Internal_error with "Assignment: assigned value (Y) is typeless";
        else
          null;  --  All right, there were already enough compilation error messages...
        end if;
      else
        Issue_Type_Mismatch_Error;
        --  NB: We are in the X.TYP /= Y.TYP case.
      end if;
    end Assignment;

    ------------------------------------------------------------------
    --------------------------------------------------------Statement--
    procedure Statement (FSys_St : Symset) is

      procedure Multi_Statement (Sentinal : Symset) is   -- Hathorn
        nxtSym : Symset;
      begin
        if Sentinal (CD.Sy) then -- GdM 15-Aug-2014: there should be at least one statement.
          Error (CD, err_statement_expected);
        end if;
        nxtSym := Sentinal + Statement_Begin_Symbol;
        loop
          Statement (nxtSym); --MRC,was: UNTIL (Sy IN Sentinal);
          exit when Sentinal (CD.Sy) or CD.Err_Count > 0;
        end loop;
      end Multi_Statement;

      procedure Accept_Statement is            -- Hathorn

        procedure Accept_Call is
        begin   --  !!  Check to make sure parameters match with Entry Statement
          if CD.Sy = Semicolon then
            return;
          end if;
          if CD.Sy = LParent then          -- <--- temporary
            while not (CD.Sy = DO_Symbol or CD.Sy = RParent) loop
              InSymbol;
            end loop; -- !! should check no. and
          end if;    -- Types of parms.
          if CD.Sy = RParent then
            InSymbol;
          end if;
        end Accept_Call;

        I_Entry : Integer;
      begin  --  Accept_Statement
        InSymbol;
        I_Entry := Locate_Identifier (CD, CD.Id, Level);
        if CD.IdTab (I_Entry).Obj /= aEntry then
          Error (CD, err_use_Small_Sp);
        end if;
        InSymbol;
        Accept_Call;
        Emit1 (CD, k_Accept_Rendezvous, I_Entry);
        if CD.Sy = DO_Symbol then
          if Level = Nesting_Level_Max then
            Fatal (LEVELS);  --  Exception is raised there.
          end if;
          Level              := Level + 1;
          CD.Display (Level) := CD.IdTab (I_Entry).Block_Ref;
          InSymbol;
          Multi_Statement (END_Set);
          Test_END_Symbol (CD);
          if CD.Sy = IDent then
            if CD.Id /= CD.IdTab (I_Entry).Name then
              Error (CD, err_incorrect_block_name);
            end if;
            InSymbol;
          end if;
          Level := Level - 1;
        end if;
        Emit1 (CD, k_End_Rendezvous, I_Entry);
      end Accept_Statement;

      procedure Exit_Statement is
        --  Generate an absolute branch statement with a dummy end loop address
        X : Exact_Typ;
      begin
        pragma Assert (CD.Sy = EXIT_Symbol);
        InSymbol;  --  Consume EXIT symbol.
        if CD.Sy = WHEN_Symbol then  --  Conditional Exit
          InSymbol;
          Boolean_Expression (CD, Level, Semicolon_Set, X);
          Emit1 (CD, k_Conditional_Jump, CD.LC + 2);  --  Conditional jump around Exit
        end if;
        Emit1 (CD, k_Jump, dummy_address);  --  Unconditional jump with dummy address to be patched
      end Exit_Statement;

      procedure IF_Statement is
        X        : Exact_Typ;
        LC0, LC1 : Integer;
      begin
        InSymbol;
        Boolean_Expression (CD, Level, FSys_St + DO_THEN, X);
        LC1 := CD.LC;
        Emit (CD, k_Conditional_Jump);
        Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
        Multi_Statement (ELSE_ELSIF_END);
        LC0 := CD.LC;
        --
        while CD.Sy = ELSIF_Symbol loop  --  Added Hathorn
          InSymbol;
          Emit1 (CD, k_Jump, dummy_address);  --  Unconditional jump with dummy address to be patched
          CD.ObjCode (LC1).Y := CD.LC;        --  Patch the previous conditional jump
          Boolean_Expression (CD, Level, FSys_St + DO_THEN, X);
          LC1 := CD.LC;
          Emit (CD, k_Conditional_Jump);
          Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
          Multi_Statement (ELSE_ELSIF_END);  --  Statements after "ELSIF .. THEN".
        end loop;
        --
        if CD.Sy = ELSE_Symbol then
          InSymbol;
          Emit1 (CD, k_Jump, dummy_address);  --  Jump to "END IF" - dummy address to be patched.
          CD.ObjCode (LC1).Y := CD.LC;
          Multi_Statement (END_Set);  --  Statements after "ELSE".
        else
          CD.ObjCode (LC1).Y := CD.LC;
        end if;
        Need (CD, END_Symbol, err_END_missing);         --  END (IF)
        Need (CD, IF_Symbol,  err_missing_closing_IF);  --  (END) IF
        --  Go back and patch the dummy addresses in unconditional jumps
        Patch_Addresses (CD.ObjCode (LC0 .. CD.LC));
      end IF_Statement;

      procedure LOOP_Statement (FCT_Loop_End : Opcode; B : Integer) is    -- Hathorn
        LC0 : constant Integer := CD.LC;
      begin
        if CD.Sy = LOOP_Symbol then
          InSymbol;
        else
          Skip (CD, Statement_Begin_Symbol, err_missing_closing_IF);
        end if;
        Multi_Statement (END_Set);
        Emit1 (CD, FCT_Loop_End, B);
        Need (CD, END_Symbol,  err_END_missing);           --  END (LOOP)
        Need (CD, LOOP_Symbol, err_closing_LOOP_missing);  --  (END) LOOP
        --  Go back and patch the dummy addresses generated by Exit statements.
        Patch_Addresses (CD.ObjCode (LC0 .. CD.LC));
      end LOOP_Statement;

      procedure RETURN_Statement is           -- Hathorn
        --  Generate a procedure or function return Statement, calculate return value if req'D.
        X, Y : Exact_Typ;
        F : Opcode;
        Block_Idx : Integer;
        procedure Issue_Type_Mismatch_Error is
        begin
          Type_Mismatch (CD, err_type_of_return_statement_doesnt_match, Found => Y, Expected => X);
        end Issue_Type_Mismatch_Error;
      begin
        if Block_ID = CD.Main_Program_ID then
          Error (CD, err_illegal_return_statement_from_main); -- !! but... this is legal in Ada !!
        end if;
        Block_Idx := Locate_Identifier (CD, Block_ID, Level);
        InSymbol;
        if CD.Sy = Semicolon then
          if Is_a_function then
            Error (CD, err_functions_must_return_a_value);
          end if;
        else
          if not Is_a_function then
            Error (CD, err_procedures_cannot_return_a_value, stop_on_error => True);
          end if;
          --  Calculate return value (destination: X; expression: Y).
          if CD.IdTab (Block_Idx).Block_Ref = CD.Display (Level) then
            X := CD.IdTab (Block_Idx).xTyp;
            if CD.IdTab (Block_Idx).Normal then
              F := k_Push_Address;
            else
              F := k_Push_Value;
            end if;
            Emit2 (CD, F, CD.IdTab (Block_Idx).LEV + 1, 0);
            --
            Expression (CD, Level, Semicolon_Set, Y);
            if X.TYP = Y.TYP then
              if (X.TYP in Standard_Typ) or else (X.TYP = Enums and then X = Y) then
                Emit (CD, k_Store);
              elsif X.Ref /= Y.Ref then
                Issue_Type_Mismatch_Error;
              end if;
            elsif X.TYP = Floats and Y.TYP = Ints then
              Forbid_Type_Coercion (CD, "integer type value returned as floating-point");
              Emit1 (CD, k_Integer_to_Float, 0);
              Emit (CD, k_Store);
            elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
              Issue_Type_Mismatch_Error;
            end if;
          else
            Error (CD, err_illegal_return_statement_from_main);
          end if;       -- !! but... this is legal in Ada !!
        end if;
        if Is_a_function then
          Emit1 (CD, k_Exit_Function, CallSTDP);
        else
          Emit1 (CD, k_Exit_Call, CallSTDP);
        end if;
      end RETURN_Statement;

      procedure Delay_Statement is            -- Cramer. Generate a Task delay.
        Y : Exact_Typ;
      begin
        InSymbol;
        if CD.Sy = Semicolon then
          Skip (CD, Semicolon, err_missing_expression_for_delay);
        else                  -- calculate delay value
          Expression (CD, Level, Semicolon_Set, Y);
          if Y.TYP /= Floats then
            Error (CD, err_wrong_type_in_DELAY);
          end if;
        end if;
        Emit (CD, k_Delay);
      end Delay_Statement;

      procedure CASE_Statement is
        X         : Exact_Typ;
        I, J, LC1 : Integer;
        type CASE_Label_Value is record
          Val : Integer;  --  value of a choice in a CASE statement
          LC  : Integer;  --  instruction address
        end record;
        CaseTab : array (1 .. Cases_Max) of CASE_Label_Value;
        ExitTab : array (1 .. Cases_Max) of Integer;
        others_flag : Boolean := False;

        procedure CASE_Label is
          Lab : Constant_Rec;
          K   : Integer;
        begin
          Number_Declaration_or_Enum_Item (FSys_St + Alt_Finger_THEN, Lab);
          if Lab.TP /= X then
            Type_Mismatch (
              CD, err_case_label_not_same_type_as_case_clause,
              Found    => Lab.TP,
              Expected => X
            );
          elsif I = Cases_Max then
            Fatal (Case_Labels);  --  Exception is raised there.
          else
            I               := I + 1;
            CaseTab (I).Val := Lab.I;
            CaseTab (I).LC  := CD.LC;
            K               := 0;
            loop
              K := K + 1;
              exit when CaseTab (K).Val = Lab.I;
            end loop;
            if K < I then
              Error (CD, err_duplicate_case_choice_value);
            end if;
          end if;
        end CASE_Label;

        procedure One_CASE is
        begin
          pragma Assert (CD.Sy = WHEN_Symbol);  --  One_Case called only on WHEN_Symbol.
          InSymbol;
          if Constant_Definition_Begin_Symbol (CD.Sy) then
            if others_flag then  --  Normal choice list *atfer* the "others" choice.
              Error (CD, err_case_others_alone_last);
            end if;
            CASE_Label;
            while CD.Sy = Alt loop
              InSymbol;  --  Consume '|' symbol.
              if CD.Sy = OTHERS_Symbol then  --  "others" mixed with normal choices.
                Error (CD, err_case_others_alone_last);
              else
                CASE_Label;
              end if;
            end loop;
          elsif CD.Sy = OTHERS_Symbol then        -- Hathorn
            if others_flag then  --  Duplicate "others".
              Error (CD, err_case_others_alone_last);
            end if;
            others_flag := True;
            if I = Cases_Max then
              Fatal (Case_Labels);  --  Exception is raised there.
            end if;
            I               := I + 1;
            CaseTab (I).Val := 0;
            CaseTab (I).LC  := -CD.LC;
            InSymbol;
          end if;
          if CD.Sy = THEN_Symbol then  --  Happens when converting IF statements to CASE.
            Error (CD, err_THEN_instead_of_Arrow, stop_on_error => True);
            InSymbol;
          else
            Need (CD, Finger, err_FINGER_missing);
          end if;
          Multi_Statement (END_WHEN);
          J           := J + 1;
          ExitTab (J) := CD.LC;
          Emit (CD, k_Jump);
        end One_CASE;

      begin  --  CASE_Statement
        InSymbol;
        I := 0;
        J := 0;
        Expression (CD, Level, FSys_St + Colon_Comma_IS_OF, X);
        if not Discrete_Typ (X.TYP) then
          Error (CD, err_bad_type_for_a_case_statement);
        end if;
        LC1 := CD.LC;
        Emit (CD, k_CASE_Switch_1);
        if CD.Sy = IS_Symbol then  --  Was OF_Symbol in SmallAda! I.e. "case x OF when 1 => ..."
          InSymbol;
        elsif CD.Sy = OF_Symbol then
          Error (CD, err_OF_instead_of_IS);  --  Common mistake by Pascal programmers
          InSymbol;
        else
          Error (CD, err_IS_missing);
        end if;

        if CD.Sy /= WHEN_Symbol then
          Error (CD, err_WHEN_missing, stop_on_error => True);
        end if;
        loop  --  All cases are parsed here.
          One_CASE;
          exit when CD.Sy /= WHEN_Symbol;
        end loop;

        CD.ObjCode (LC1).Y := CD.LC;  --  Set correct instruction address for k_CASE_Switch_1 above.
        for K in 1 .. I loop
          if CaseTab (K).LC > 0 then
            Emit2 (CD, k_CASE_Switch_2, 1, CaseTab (K).Val);
            Emit1 (CD, k_CASE_Switch_2,    CaseTab (K).LC);
          else
            --  "WHEN OTHERS =>" case
            Emit2 (CD, k_CASE_Switch_2, -1, CaseTab (K).Val);  --!! val = 0
            Emit1 (CD, k_CASE_Switch_2,    -CaseTab (K).LC);
          end if;
        end loop;
        --  Bogus instruction for having the interpreter exiting the k_CASE_Switch_2 loop.
        Emit1 (CD, k_Jump, 0);
        --
        for K in 1 .. J loop
          CD.ObjCode (ExitTab (K)).Y  := CD.LC;
        end loop;
        Need (CD, END_Symbol,  err_END_missing);           --  END (CASE)
        Need (CD, CASE_Symbol, err_missing_closing_CASE);  --  (END) CASE
      end CASE_Statement;

      procedure WHILE_Statement is  --  RM 5.5 (8)
        X : Exact_Typ;
        LC_Cond_Eval, LC_Cond_Jump : Integer;
      begin
        InSymbol;  --  Consume WHILE symbol.
        LC_Cond_Eval := CD.LC;
        Boolean_Expression (CD, Level, FSys_St + DO_LOOP, X);
        LC_Cond_Jump := CD.LC;
        Emit (CD, k_Conditional_Jump);
        LOOP_Statement (k_Jump, LC_Cond_Eval);
        CD.ObjCode (LC_Cond_Jump).Y := CD.LC;
      end WHILE_Statement;

      procedure FOR_Statement is  --  RM 5.5 (9)
        FOR_Lower_Bound,
        FOR_Upper_Bound : Exact_Typ;
        FOR_Begin       : Opcode;    --  Forward or Reverse
        LC_FOR_Begin, Previous_Last       : Integer;
      begin
        InSymbol;  --  Consume FOR symbol.
        if CD.Sy = IDent then
          if CD.Id_Count = Id_Table_Max then
            Fatal (IDENTIFIERS);  --  Exception is raised there.
          end if;
          --  Declare local loop control Variable  --  added Hathorn
          Previous_Last := CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx;
          CD.Id_Count := CD.Id_Count + 1;
          CD.IdTab (CD.Id_Count) :=        --  Loop parameter: the "i" in  "for i in 1..10 loop"
             (  Name           => CD.Id,
                Name_with_case => CD.Id_with_case,
                Link           => Previous_Last,
                Obj            => Variable,
                Read_only      => True,
                xTyp           => Type_Undefined,
                Block_Ref      => 0,
                Normal         => True,
                LEV            => Level,
                Adr_or_Sz      => Dx
             );
          CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx  := CD.Id_Count;
          Dx := Dx + 1;
          if Dx > MaxDX then
            MaxDX := Dx;
          end if;
          CD.Blocks_Table (CD.Display (Level)).VSize := MaxDX;
        else
          Skip (CD, Fail_after_FOR + FSys_St, err_identifier_missing);
        end if;
        --
        Emit2 (CD, k_Push_Address, CD.IdTab (CD.Id_Count).LEV, CD.IdTab (CD.Id_Count).Adr_or_Sz);
        InSymbol;
        FOR_Begin := k_FOR_Forward_Begin;
        if CD.Sy = IN_Symbol then         --       "IN"  in  "for i in reverse 1 .. 10 loop"
          InSymbol;
          if CD.Sy = REVERSE_Symbol then  --  "REVERSE"  in  "for i in reverse 1 .. 10 loop"
            FOR_Begin := k_FOR_Reverse_Begin;
            InSymbol;
          end if;
          --  !!  Here we should have a more general syntax:
          --      discrete_subtype_definition RM 3.6 (6) which is either
          --      a subtype_indication 3.2.2 (3) : name [constraint] like "Color [range red .. blue]"
          --      or a range 3.5 (3): "low .. high" or range_attribute_reference: A'Range
          --
          Expression (CD, Level, END_LOOP_RANGE_Double_Dot + FSys_St, FOR_Lower_Bound);
          CD.IdTab (CD.Id_Count).xTyp := FOR_Lower_Bound;
          if not Discrete_Typ (FOR_Lower_Bound.TYP) then
            Error (CD, err_control_variable_of_the_wrong_type);
          end if;
          if CD.Sy = Range_Double_Dot_Symbol then                          --  ".."
            InSymbol;
            Expression (CD, Level, FSys_St + LOOP_Symbol, FOR_Upper_Bound);
            if FOR_Upper_Bound /= FOR_Lower_Bound then
              Type_Mismatch (
                CD, err_first_and_last_must_have_matching_types,
                Found    => FOR_Upper_Bound,
                Expected => FOR_Lower_Bound
              );
            end if;
          else
            Skip (CD, END_LOOP_Semicolon + FSys_St, err_expecting_double_dot);
          end if;
        else
          Skip (CD, FSys_St + LOOP_Symbol, err_IN_missing);
        end if;
        LC_FOR_Begin := CD.LC;
        Emit (CD, FOR_Begin);
        LOOP_Statement (For_END (FOR_Begin), CD.LC);
        CD.ObjCode (LC_FOR_Begin).Y := CD.LC;  --  Address of the code just after the loop's end.
        --  Forget the iterator variable:
        CD.Id_Count := CD.Id_Count - 1;
        CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := Previous_Last;
        Dx := Dx - 1;
      end FOR_Statement;

      procedure Select_Statement is
        procedure Select_Error (N : Compile_Error) is
        begin
          Skip (CD, Semicolon, N);
        end Select_Error;

        --  Either a Timed or Conditional Entry Call.

        procedure Qualified_Entry_Call is
          I, J, IStart, IEnd : Integer;
          patch              : array (0 .. 4) of Integer;
          O                  : Order;
          Y                  : Exact_Typ;
        begin
          I := Locate_Identifier (CD, CD.Id, Level);
          if CD.IdTab (I).Obj = aTask then
            InSymbol;
            Entry_Call (CD, Level, FSys_St, I, -1);
            if CD.ObjCode (CD.LC - 2).F = k_Call then  --  Need to patch CallType later
              patch (0) := CD.LC - 2;
            else
              patch (0) := CD.LC - 3;
            end if;       -- LC-1 must be OP=3, update Display
            patch (1) := CD.LC;  --  Need to patch in JMPC address later
            Emit1 (CD, k_Conditional_Jump, dummy_address);  --  JMPC, address patched in after ELSE
                                      --or OR
            if CD.Sy = Semicolon then
              InSymbol;
            else
              Skip (CD, Semicolon, err_semicolon_missing);
            end if;
            if not (CD.Sy = OR_Symbol or else CD.Sy = ELSE_Symbol) then
              Multi_Statement (ELSE_OR);
            end if;
            if CD.Sy = OR_Symbol then  --  =====================> Timed Entry Call
              CD.ObjCode (patch (0)).X      := CallTMDE;  --  Timed Entry Call
              CD.ObjCode (patch (0) + 1).Y  := CallTMDE;  --  Exit type matches Entry type
              InSymbol;
              if CD.Sy = DELAY_Symbol then
                InSymbol;
                if CD.Sy = Semicolon then
                  Select_Error (err_missing_expression_for_delay);
                else          -- calculate delay value
                  patch (2) := CD.LC;
                  Expression (CD, Level, Semicolon_Set, Y);
                  patch (3) := CD.LC - 1;
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  else        --  end of timed Entry select ObjCode, do patching
                    CD.ObjCode (patch (1)).Y  := CD.LC;  --  if Entry not made, Skip rest
                    J                      := patch (3) - patch (2) + 1;
                    IStart                 := patch (0);
                    IEnd                   := CD.LC - 1;
                    while J > 0 loop           --  move delay time ObjCode To before
                      O := CD.ObjCode (IEnd);  --  opcodes kCall, k_Exit_Call
                      for I in reverse IEnd - 1 .. IStart loop
                        CD.ObjCode (I + 1) := CD.ObjCode (I);
                      end loop;
                      CD.ObjCode (IStart) := O;
                      J                   := J - 1;
                    end loop;
                    InSymbol;
                  end if;
                end if;
              else
                Select_Error (err_expecting_DELAY);
              end if;
            -- end Sy = OrSy
            else              -- Sy = ELSE_Symbol, ===============> Conditional Entry Call
              CD.ObjCode (patch (0)).X      := CallCNDE;  --  Conditional Entry Call
              CD.ObjCode (patch (0) + 1).Y  := CallCNDE;
              patch (2)                     := CD.LC;
              Emit1 (CD, k_Jump, dummy_address);  -- JMP, address patched in after END SELECT
              patch (3) := CD.LC;
              InSymbol;
              Multi_Statement (END_Set);
              CD.ObjCode (patch (1)).Y  := patch (3);
              CD.ObjCode (patch (2)).Y  := CD.LC;
            end if;
            if CD.Sy /= END_Symbol then
              Select_Error (err_END_missing);
            end if;
            InSymbol;
            if CD.Sy /= SELECT_Symbol then
              Select_Error (err_SELECT_missing);
            end if;
          else
            Select_Error (err_expecting_task_entry);
          end if;          -- Task.Entry Call expected
        end Qualified_Entry_Call;

        procedure Selective_Wait is         -- Kurtz <===================
          -- Jay, this Buds for you !!
          JSD, Alt_Patch      : Fixed_Size_Patch_Table;
          ISD, IAlt, StartSel : Integer;
          SelectDone          : Boolean;
          Y, X                : Exact_Typ;
          do_terminate        : Boolean;

          procedure Accept_Statement_2 is      -- Kurtz

            procedure Accept_Call_2 is
            begin
              --  Check to make sure parameters match with Entry Statement
              if CD.Sy = Semicolon then
                return;
              end if;
              if CD.Sy = LParent then      -- should be modified
                -- To check no. and
                while not (CD.Sy = DO_Symbol or CD.Sy = RParent) loop
                  InSymbol;
                end loop;
              end if;        -- of parameters.
              if CD.Sy = RParent then
                InSymbol;
              end if;
            end Accept_Call_2;

            I : Integer;
          begin         -- Accept_Statment_2
            InSymbol;
            I := Locate_Identifier (CD, CD.Id, Level);
            if CD.IdTab (I).Obj /= aEntry then
              Select_Error (err_use_Small_Sp);
            end if;
            InSymbol;
            Accept_Call_2;
            Emit2 (CD, k_Selective_Wait, 2, I);          --  Retain Entry Index
            Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
            Emit2 (CD, k_Selective_Wait, 3, CD.LC);  --  ACCEPT IF Ready ELSE Skip To LC
            -- CONDITIONAL ACCEPT MUST BE ATOMIC
            if CD.Sy = DO_Symbol then
              if Level = Nesting_Level_Max then
                Fatal (LEVELS);  --  Exception is raised there.
              end if;
              Level              := Level + 1;
              CD.Display (Level) := CD.IdTab (I).Block_Ref;
              InSymbol;
              Multi_Statement (END_Set);
              Test_END_Symbol (CD);
              if CD.Sy = IDent then
                if CD.Id /= CD.IdTab (I).Name then
                  Select_Error (err_incorrect_block_name);
                end if;
              end if;
              Level := Level - 1;
              InSymbol;
            end if;
            Emit1 (CD, k_End_Rendezvous, I);
          end Accept_Statement_2;

        begin  --  Selective_Wait ===============================> Kurtz
          ISD          := 0;
          IAlt         := 0;
          SelectDone   := False;
          do_terminate := False;
          StartSel     := CD.LC;
          Emit2 (CD, k_Selective_Wait, 1, 0);  --  START OF SELECT SELECTIVE Wait SEQUENCE
          loop
            case CD.Sy is
              when WHEN_Symbol =>
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
                InSymbol;  --  Consume WHEN symbol.
                Boolean_Expression (CD, Level, FSys_St + Finger, X);
                InSymbol;
                if CD.Sy = ACCEPT_Symbol then
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Conditional_Jump);
                  Accept_Statement_2;
                elsif CD.Sy = DELAY_Symbol then
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Conditional_Jump);
                  InSymbol;
                  Expression (CD, Level, FSys_St + Semicolon, Y);
                  Emit2 (CD, k_Selective_Wait, 4, CD.LC + 2);  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump);
                else
                  Select_Error (err_missing_a_procedure_declaration);
                end if;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                Feed_Patch_Table (JSD, ISD, CD.LC);
                Emit (CD, k_Jump);          --  patch JMP ADDRESS AT EndSy
              -- end WHEN_Symbol

              when ACCEPT_Symbol =>
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
                Accept_Statement_2;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                Feed_Patch_Table (JSD, ISD, CD.LC);
                Emit (CD, k_Jump);

              when OR_Symbol =>
                InSymbol;  --  Consume OR symbol.

              when ELSE_Symbol =>
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
                InSymbol;
                Multi_Statement (END_Set);
                Feed_Patch_Table (JSD, ISD, CD.LC);
                Emit (CD, k_Jump);

              when DELAY_Symbol =>
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
                --  Generate a Task delay, calculate return value if req'D
                InSymbol;
                if CD.Sy = Semicolon then
                  Skip (CD, Semicolon, err_missing_expression_for_delay);
                else          -- calculate return value
                  Expression (CD, Level, Semicolon_Set, Y);
                  Emit2 (CD, k_Selective_Wait, 4, CD.LC + 2);  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump);
                end if;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                Feed_Patch_Table (JSD, ISD, CD.LC);
                Emit (CD, k_Jump);

              when TERMINATE_Symbol =>
                InSymbol;
                if CD.Sy /= Semicolon then
                  Select_Error (err_semicolon_missing);
                end if;
                do_terminate := True;        -- Oguz
                InSymbol;

              when END_Symbol =>
                InSymbol;
                if CD.Sy /= SELECT_Symbol then
                  Select_Error (err_END_missing);
                end if;
                SelectDone := True;
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
                if do_terminate then
                  Emit2 (CD, k_Selective_Wait, 5, StartSel);
                else
                  Emit2 (CD, k_Selective_Wait, 6, StartSel);
                end if;   -- Suspend
                Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), JSD, ISD);

              when others =>
                SelectDone := True;
            end case;
            exit when SelectDone;
          end loop;
        end Selective_Wait;

      begin
        InSymbol;  --  Consume SELECT symbol.
        if CD.Sy = ACCEPT_Symbol or CD.Sy = WHEN_Symbol then
          Selective_Wait;
          InSymbol;
        elsif CD.Sy = IDent then  --  Task Entry objectName.
          Qualified_Entry_Call;
          InSymbol;
          -- Timed or Conditional Entry Call (?)
        else
          Select_Error (err_expecting_accept_when_or_entry_id);
        end if;
      end Select_Statement;

      procedure Block_Statement (block_name: Alfa) is  --  RM: 5.6
      begin
        Error (
          CD, err_not_yet_implemented,
          hint => "Block statements don't work yet",
          stop_on_error => True
        );
        --
        Block (CD, FSys_St, Is_a_function, True, Level + 1, CD.Id_Count, block_name, block_name);  --  !! up/low case
        --
        -- !! to check:
        -- !! * stack management of variables when entering / quitting the block
        -- !! * object code and nesting... works on some cases at least (test.adb) !...
        -- !! Perhaps keep same level but have local declarations as for the
        --    variable in a FOR_Statement.
        -- !! Either both FOR and Block statements forget their definitions, or there
        --    is perhaps a problem with the stack (DX).
        -- !! Local bodies of subprograms surely mess the object code.
      end Block_Statement;

      procedure Named_Statement is
        --  Block_Statement or loop, named by "name: loop"
        new_ident_for_statement           : constant Alfa := CD.Id;
        new_ident_for_statement_with_case : constant Alfa := CD.Id_with_case;
        --
        procedure Check_ID_after_END_LOOP is  --  RM 5.5 (5)
        begin
          if CD.Sy = IDent then
            if CD.Id /= new_ident_for_statement then
              Error (CD, err_END_LOOP_ident_wrong,
                     hint => To_String (new_ident_for_statement_with_case));
            end if;
            InSymbol;  --  Consume identifier.
          else
            Error (CD, err_END_LOOP_ident_missing,
                   hint => To_String (new_ident_for_statement_with_case));
          end if;
        end Check_ID_after_END_LOOP;
        --
      begin
        Enter (CD, Level, new_ident_for_statement, CD.Id_with_case, Label);
        Test (CD, Colon_Set, FSys_St,
          err_colon_missing_for_named_statement,
          stop_on_error => True
        );
        InSymbol;  --  Consume ':' symbol.
        case CD.Sy is
          when BEGIN_Symbol | DECLARE_Symbol => -- Named Block_Statement
            Block_Statement (new_ident_for_statement);
          when FOR_Symbol =>
            FOR_Statement;
            Check_ID_after_END_LOOP;
          when LOOP_Symbol =>
            LOOP_Statement (k_Jump, CD.LC);
            Check_ID_after_END_LOOP;
          when WHILE_Symbol =>
            WHILE_Statement;
            Check_ID_after_END_LOOP;
          when others =>
            Error (CD, err_syntax_error);
        end case;
      end Named_Statement;

      I_Statement : Integer;

    begin  --  Statement
      if CD.Err_Count > 0 then
        return;
      end if;
      if Statement_Begin_Symbol (CD.Sy) then
        case CD.Sy is
          when IDent =>
            I_Statement := Locate_Identifier (CD, CD.Id, Level, No_Id_Fail => False);
            InSymbol;
            if I_Statement = No_Id then
              --  New identifier: must be an identifier for a named Block_Statement or loop.
              Named_Statement;
            else
              case CD.IdTab (I_Statement).Obj is
                when Variable =>
                  Assignment (I_Statement, Check_read_only => True);
                when Declared_Number_or_Enum_Item =>
                  Error (CD, err_illegal_statement_start_symbol, "constant or an enumeration item",
                         stop_on_error => True);
                when TypeMark =>
                  Error (CD, err_illegal_statement_start_symbol, "type name", stop_on_error => True);
                when Funktion =>
                  Error (CD, err_illegal_statement_start_symbol, "function name",
                         stop_on_error => True);
                when aTask =>
                  Entry_Call (CD, Level, FSys_St, I_Statement, CallSTDE);
                when Prozedure =>
                  if CD.IdTab (I_Statement).LEV = 0 then
                    Standard_Procedures.Standard_Procedure
                      (CD, Level, FSys_St, SP_Code'Val (CD.IdTab (I_Statement).Adr_or_Sz));
                  else
                    Subprogram_or_Entry_Call (CD, Level, FSys_St, I_Statement, CallSTDP);
                  end if;
                when Label =>
                  Error (CD, err_duplicate_label, To_String (CD.Id));
                  Test (CD, Colon_Set, FSys_St, err_colon_missing);
                  InSymbol;
                when others =>
                  null;
              end case;
            end if;  --  end IDent
          when ACCEPT_Symbol =>
            Accept_Statement;
          when BEGIN_Symbol | DECLARE_Symbol =>
            --  Anonymous Block Statement
            Block_Statement (Empty_Alfa);
          when CASE_Symbol =>
            CASE_Statement;
          when DELAY_Symbol =>
            Delay_Statement;
          when EXIT_Symbol =>
            Exit_Statement;
          when FOR_Symbol =>
            FOR_Statement;
          when IF_Symbol =>
            IF_Statement;
          when LOOP_Symbol =>
            LOOP_Statement (k_Jump, CD.LC);
          when NULL_Symbol =>
            InSymbol;  --  Just consume the NULL symbol.
          when RETURN_Symbol =>
            RETURN_Statement;
          when SELECT_Symbol =>
            Select_Statement;
          when WHILE_Symbol =>
            WHILE_Statement;
          when others =>
            null;
        end case;
        --
        Need (CD, Semicolon, err_semicolon_missing);
      end if;  --  CD.Sy in Statement_Begin_Symbol
      --
      Test (CD, FSys_St - Semicolon, Semicolon_Set, err_incorrectly_used_symbol);
    end Statement;

    procedure Declarative_Part is
    begin
      loop
        Test (  --  Added 17-Apr-2018 to avoid infinite loop on erroneous code
          CD, Declaration_Symbol + BEGIN_Symbol,
          Empty_Symset,
          err_incorrectly_used_symbol,
          stop_on_error => True  --  Exception is raised there if there is an error.
        );
        if CD.Sy = IDent then
          Var_Declaration;
        end if;
        if CD.Sy = TYPE_Symbol then
          Type_Declaration;
        end if;
        if CD.Sy = TASK_Symbol then
          Tasking.Task_Declaration (CD, FSys, Level);
        end if;
        CD.Blocks_Table (PRB).VSize := Dx;
        --  ^ TBD: check if this is still correct for declarations that appear
        --    after subprograms !!
        while CD.Sy = PROCEDURE_Symbol or CD.Sy = FUNCTION_Symbol loop
          Proc_Func_Declaration;
        end loop;
        --
        exit when CD.Sy = BEGIN_Symbol;
      end loop;
    end Declarative_Part;

    procedure Statements_Part_Setup is
      Init_Code_Idx : Integer;
    begin
      MaxDX                    := Dx;
      CD.IdTab (Prt).Adr_or_Sz := CD.LC;
      --  Copy initialization (elaboration) ObjCode from end of ObjCode table
      Init_Code_Idx := CD.CMax + ICode;
      while Init_Code_Idx > CD.CMax loop
        CD.ObjCode (CD.LC) := CD.ObjCode (Init_Code_Idx);
        CD.LC              := CD.LC + 1;
        Init_Code_Idx      := Init_Code_Idx - 1;
      end loop;
      CD.CMax := CD.CMax + ICode;  --  Restore CMax to the initial max (=CDMax)
    end Statements_Part_Setup;

    procedure Statements_List is
    begin
      if CD.Sy = END_Symbol then  --  GdM 15-Aug-2014: there should be at least one statement.
        Error (CD, err_statement_expected);
      end if;
      loop
        Statement (FSys + END_Symbol);
        exit when CD.Sy = END_Symbol or CD.Err_Count > 0;
      end loop;
    end Statements_List;

    procedure Statements_Part_Closing is
    begin
      CD.Blocks_Table (PRB).SrcTo := CD.Line_Count;
    end Statements_Part_Closing;

    procedure Function_Result_Profile is
      I_Res_Type : Integer;
    begin
      if CD.Sy = RETURN_Symbol then
        InSymbol;  --  FUNCTION TYPE
        if CD.Sy = IDent then
          I_Res_Type := Locate_Identifier (CD, CD.Id, Level);
          InSymbol;
          if I_Res_Type /= 0 then
            if CD.IdTab (I_Res_Type).Obj /= TypeMark then
              Error (CD, err_missing_a_type_identifier, stop_on_error => True);
            elsif Standard_or_Enum_Typ (CD.IdTab (I_Res_Type).xTyp.TYP) then
              CD.IdTab (Prt).xTyp := CD.IdTab (I_Res_Type).xTyp;
            else
              Error (CD, err_bad_result_type_for_a_function, stop_on_error => True);
            end if;
          end if;
        else
          Error (CD, err_identifier_missing, stop_on_error => True);
        end if;
      else
        Error (CD, err_RETURN_missing, stop_on_error => True);
      end if;
    end Function_Result_Profile;

    Restore_Block_ID : Alfa := CD.Block_Id_with_casing;

  begin  --  Block
    Restore_Block_ID := CD.Block_Id_with_casing;
    CD.Block_Id_with_casing := Block_ID_with_case;
    if CD.Err_Count > 0 then
      return;
    end if;
    Dx    := 5;
    ICode := 0;
    if Is_a_block_statement then
      null;  --  We should be here with Sy = BEGIN_Symbol or Sy = DECLARE_Symbol.
    else
      Test (CD, Symbols_after_Subprogram_Identifier, FSys, err_incorrectly_used_symbol);
    end if;
    if CD.IdTab (Prt).Block_Ref > 0 then
      PRB := CD.IdTab (Prt).Block_Ref;
    else
      Enter_Block (CD, Prt);
      PRB                      := CD.Blocks_Count;
      CD.IdTab (Prt).Block_Ref := PRB;
    end if;
    CD.Display (Level) := PRB;
    CD.IdTab (Prt).xTyp := Type_Undefined;
    if CD.Sy = LParent and Level > 1 then
      Formal_Parameter_List;
    end if;
    --
    if CD.Err_Count > 0 then
      return;
    end if;
    --
    CD.Blocks_Table (PRB).Last_Param_Id_Idx := CD.Id_Count;
    CD.Blocks_Table (PRB).PSize := Dx;
    --
    if Is_a_function and not Is_a_block_statement then
      Function_Result_Profile;
    end if;
    --
    if CD.Sy = Semicolon then  --  end of specification part
      CD.Blocks_Table (PRB).VSize := Dx;
      CD.IdTab (Prt).Adr_or_Sz    := -1;    -- address of body TBD
      return;
    end if;
    --
    if Is_a_block_statement then
      case CD.Sy is
        when DECLARE_Symbol => InSymbol;
        when BEGIN_Symbol   => null;
        when others         => raise Internal_error with "Unexpected " & KeyWSymbol'Image(CD.Sy);
      end case;
    elsif CD.Sy = IS_Symbol then  --  The "IS" in "procedure ABC (param : T_Type) IS"
      InSymbol;
    else
      Error (CD, err_IS_missing);
      return;
    end if;
    --
    if CD.Sy = NULL_Symbol and not Is_a_block_statement then
      --  RM 6.7 Null Procedures (Ada 2005)
      --  E.g.: "procedure Not_Yet_Done (a : Integer) is null;"
      InSymbol;  --  Consume NULL symbol.
      Statements_Part_Setup;
      if Is_a_function then
        Error (CD, err_no_null_functions);  --  There are no null functions: what would be the result?
      else
        null;  --  No statement -> no instruction, like for the NULL statement.
      end if;
      Statements_Part_Closing;
    else
      Declarative_Part;
      InSymbol;  --  Consume BEGIN symbol.
      Statements_Part_Setup;
      Statements_List;
      Statements_Part_Closing;
      --
      if CD.Sy = END_Symbol then
        InSymbol;
      elsif CD.Err_Count > 0 then
        return;  --  At this point the program is already FUBAR.
      else
        Error (CD, err_END_missing);
        return;
      end if;
      --
      if CD.Sy = IDent then  --  Verify that the name after "end" matches the unit name.
        if CD.Id /= Block_ID then
          Error (CD, err_incorrect_block_name, hint => To_String (Block_ID_with_case));
        end if;
        InSymbol;
      elsif Is_a_block_statement and Block_ID /= Empty_Alfa then  --  "end [label]" is required
        Error (CD, err_incorrect_block_name, hint => To_String (Block_ID_with_case));
      end if;
    end if;
    --
    if CD.Sy /= Semicolon then
      Error (CD, err_semicolon_missing);
      return;
    end if;
    --
    if Block_ID /= CD.Main_Program_ID and not Is_a_block_statement then
      InSymbol;  --  Consume ';' symbol after END [Subprogram_Id].
      --
      --  Now we have either another declaration,
      --  or BEGIN or, if it's a package, END.
      Test (
        CD, FSys + Declaration_Symbol + BEGIN_Symbol + END_Symbol,
        Empty_Symset,
        err_incorrectly_used_symbol
      );
    end if;
    CD.Block_Id_with_casing := Restore_Block_ID;
  end Block;

end HAC.Parser;
