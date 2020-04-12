with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser is

  use HAC.Data;

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block (
    FSys                 : HAC.Data.Symset;
    Is_a_function        : Boolean;        --  RETURN [Value] statement expected
    Is_a_block_statement : Boolean;        --  5.6 Block Statements
    Level_A              : Integer;
    Prt                  : Integer;
    Block_ID             : HAC.Data.Alfa;  --  Name of this block (if any)
    Block_ID_with_case   : HAC.Data.Alfa
  )
  is
    Level : Integer := Level_A;

    type ConRec is record
      TP : Types;
      I  : Integer;
      R  : HAC_Float;
      --   CASE TP IS
      --     WHEN Ints | xChars | Bools => I: Integer;
      --     WHEN Floats => R: Float;
      --     WHEN others => null;
      --   END CASE;
    end record;

    Dx      : Integer;  -- data allocation Index
    MaxDX   : Integer;
    PRB     : Integer;  -- B-Index of this procedure
    I       : Integer;  -- Index into the identifier table IdTab
    ICode   : Integer;  -- Size of initialization ObjCode generated

    ------------------------------------------------------------------
    -------------------------------------------------------EnterArray-

    procedure Enter_Array (TP : Types; L, H : Integer) is
      Lz, Hz : Integer;
    begin
      if L > H then
        Error (err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)"); -- !!
      end if;
      Lz := L;
      Hz := H;
      if abs (L) > XMax or abs (H) > XMax then
        Error (err_illegal_array_bounds, "absolute value of a bound exceeds maximum value");
        Lz := 0;
        Hz := 0;
      end if;
      if A = AMax then
        Fatal (ARRAYS);  --  Exception is raised there.
      end if;
      A := A + 1;
      ArraysTab (A).Index_TYP := TP;
      ArraysTab (A).Low       := Lz;
      ArraysTab (A).High      := Hz;
    end Enter_Array;

    ------------------------------------------------------------------
    ------------------------------------------------------Enter_Block-
    procedure Enter_Block (Tptr : Integer) is
    begin
      if B = BMax then
        Fatal (PROCEDURES);  --  Exception is raised there.
      end if;
      B                    := B + 1;
      BlockTab (B).Id      := IdTab (Tptr).Name;
      BlockTab (B).Last    := 0;
      BlockTab (B).LastPar := 0;
      BlockTab (B).SrcFrom := Line_Count;
    end Enter_Block;

    ------------------------------------------------------------------
    ------------------------------------------------------Enter_Float-
    procedure Enter_Float (X : HAC_Float) is
    begin
      if C2 = C2Max - 1 then
        Fatal (FLOAT_CONSTANTS);  --  Exception is raised there.
      end if;
      FloatPtTab (C2 + 1) := X;  --  We add X's value as an extra item.
      C1                  := 1;
      while FloatPtTab (C1) /= X loop
        C1 := C1 + 1;
      end loop;
      if C1 > C2 then  --  X's value was not previously in the table.
        C2 := C1;
      end if;
    end Enter_Float;

    ------------------------------------------------------------------
    ------------------------------------------------------------Enter-
    procedure Enter (Id, Id_with_case : Alfa; K : aObject) is
      J, L : Integer;
    begin
      if T = TMax then
        Fatal (IDENTIFIERS);  --  Exception is raised there.
      end if;
      IdTab (No_Id).Name := Id;        --  Sentinel
      J                  := BlockTab (Display (Level)).Last;
      L                  := J;
      while IdTab (J).Name /= Id loop
        J := IdTab (J).Link;
      end loop;
      --  Follow the chain of identifiers for current Level.
      if J /= No_Id then
        Error (err_duplicate_identifier, Id);
      else      --  Enter identifier in table IdTab
        T         := T + 1;
        IdTab (T) :=
         (Name           => Id,
          Name_with_case => Id_with_case,
          Link           => L,
          Obj            => K,
          TYP            => NOTYP,
          Ref            => 0,
          Normal         => True,
          LEV            => Level,
          Adr            => 0
        );
        BlockTab (Display (Level)).Last := T;  --  Update start of identifier chain
      end if;
    end Enter;

    ------------------------------------------------------------------
    ------------------------------------------------Locate_Identifier-
    function Locate_Identifier (
      Id            : Alfa;
      No_Id_Fail    : Boolean := True;
      stop_on_error : Boolean := False) return Natural
    is
      L, J : Integer;
    begin
      L                  := Level;
      IdTab (No_Id).Name := Id;      --  Sentinel
      loop
        J := BlockTab (Display (L)).Last;
        while IdTab (J).Name /= Id loop  --  Scan all Id's on level L.
          J := IdTab (J).Link;
        end loop;
        L := L - 1;
        exit when L < 0 or J /= No_Id;
      end loop;
      if J = No_Id and No_Id_Fail then
        Error (err_undefined_identifier, stop_on_error => stop_on_error);
      end if;
      return J;
    end Locate_Identifier;

    ------------------------------------------------------------------
    -------------------------------------------------Get_File_Pointer-
    function Get_File_Pointer (Id : Alfa) return Integer is    -- Schoening
    begin   -- locate Id in FileIOTab
      for I in 1 .. FileIOTab.Kount loop
        if FileIOTab.Nam (I) (2) = ':' then
          if FileIOTab.Nam (I) (3 .. FileIOTab.LNam (I) - 2) =
             Id (1 .. FileIOTab.LNam (I) - 2)
          then
            return I;
          end if;
        elsif FileIOTab.Nam (I) = Id (1 .. FileIOTab.LNam (I)) then
          return I;
        end if;
      end loop;
      return No_File_Index;
    end Get_File_Pointer;

    ------------------------------------------------------------------
    ---------------------------------------------------Enter_Variable-
    procedure Enter_Variable is
    begin
      if Sy = IDent then
        Enter (Id, Id_with_case, Variable);
        InSymbol;
      else
        Error (err_identifier_missing);
      end if;
    end Enter_Variable;

    ------------------------------------------------------------------
    -----------------------------------------------Number_Declaration-
    procedure Number_Declaration (FSys : Symset; C : in out ConRec) is
      --  RM 3.3.2. Was: Constant in the Pascal compiler. It covers untyped constants.
      --  Additionally this compiler does on-the-fly declarations for bounds
      --  in ranges (FOR, ARRAY) and values in CASE statements.
      X, Sign : Integer;
    begin
      C.TP := NOTYP;
      C.I  := 0;
      Test (Constant_Definition_Begin_Symbol, FSys, err_illegal_symbol_for_a_number_declaration);
      if not Constant_Definition_Begin_Symbol (Sy) then
        return;
      end if;
      if Sy = CharCon then  --  Untyped character constant, occurs only in ranges.
        C.TP := xChars;
        C.I  := INum;
        InSymbol;
      else
        Sign := 1;
        if Plus_Minus (Sy) then
          if Sy = MinUS then
            Sign := -1;
          end if;
          InSymbol;
        end if;
        if Sy = IDent then
          X := Locate_Identifier (Id);
          if X /= 0 then
            if IdTab (X).Obj /= Declared_Number then
              Error (err_illegal_constant_or_constant_identifier);
            else
              C.TP := IdTab (X).TYP;
              if C.TP = Floats then
                C.R := HAC_Float (Sign) * FloatPtTab (IdTab (X).Adr);
              else
                C.I := Sign * IdTab (X).Adr;
              end if;
            end if;
          end if;  --  X /= 0
          InSymbol;
        elsif Sy = IntCon then
          C.TP := Ints;
          C.I  := Sign * INum;
          InSymbol;
        elsif Sy = FloatCon then
          C.TP := Floats;
          C.R  := HAC_Float (Sign) * RNum;
          InSymbol;
        else
          Skip (FSys, err_illegal_symbol_for_a_number_declaration);
        end if;
      end if;
      Test (FSys, Empty_Symset, err_incorrectly_used_symbol);
    end Number_Declaration;

    ------------------------------------------------------------------
    --------------------------------------------------------------TYP-

    procedure TYP (FSys : Symset; TP : out Types; RF, Sz : out Integer) is
      ELTP                 : Types;
      ELRF                 : Integer;
      ELSZ, Offset, T0, T1 : Integer;
      StrArray             : Boolean;

      procedure Array_Typ (ARef, Arsz : in out Integer; StrAr : Boolean) is
        ELTP       : Types;
        Low, High  : ConRec;
        ELRF, ELSZ : Integer;
      begin
        Number_Declaration (OF_RANGE_Double_Dot_RParent + FSys, Low);
        --
        if Low.TP = Floats then
          Error (err_illegal_array_bounds, "a float type is not expected for a bound");
          Low.TP := Ints;
          Low.I  := 0;
        end if;
        Need (Range_Double_Dot_Symbol, err_expecting_double_dot);
        --
        Number_Declaration (Comma_OF_RParent + FSys, High);
        --
        if High.TP /= Low.TP then
          Error (err_illegal_array_bounds, "bound types do not match");
          High.I := Low.I;
        end if;
        Enter_Array (Low.TP, Low.I, High.I);
        ARef := A;
        if StrAr then
          ELTP := xChars;
          ELRF := 0;
          ELSZ := 1;
          Need (RParent, err_closing_parenthesis_missing, Forgive => RBrack);
        elsif Sy = Comma then  --  Multidimensional array is array(range_1) of array(range_2,...)
          InSymbol;
          ELTP := Arrays;
          Array_Typ (ELRF, ELSZ, StrAr);  --  Recursion for next array dimension.
        else
          Need (RParent, err_closing_parenthesis_missing, Forgive => RBrack);
          Need (OF_Symbol, err_missing_OF);
          TYP (FSys, ELTP, ELRF, ELSZ);
        end if;
        Arsz := (High.I - Low.I + 1) * ELSZ;
        declare
          r : ATabEntry renames ArraysTab (ARef);
        begin
          r.Size        := Arsz;  --  NB: Index_TYP, Low, High already set
          r.Element_TYP := ELTP;
          r.ELREF       := ELRF;
          r.ELSize      := ELSZ;
        end;
      end Array_Typ;

      procedure Enumeration_Type is
        ECount : Natural := 0;
      begin
        TP := Enums;
        RF := T;
        loop
          InSymbol;  --  Consume '(' symbol.
          if Sy = IDent then
            ECount := ECount + 1;
            Enter (Id, Id_with_case, Declared_Number);
            IdTab (T).TYP := Enums;
            IdTab (T).Ref := RF;
            IdTab (T).Adr := ECount;
          else
            Error (err_identifier_missing);
          end if;
          InSymbol;
          exit when Sy /= Comma;
        end loop;
        Sz := ECount;
        Need (RParent, err_closing_parenthesis_missing);
      end Enumeration_Type;

      procedure Record_Type is
      begin
        InSymbol;  --  Consume RECORD symbol.
        Enter_Block (T);
        TP := Records;
        RF := B;
        if Level = LMax then
          Fatal (LEVELS);  --  Exception is raised there.
        end if;
        Level           := Level + 1;
        Display (Level) := B;
        Offset          := 0;
        --
        loop
          if Sy /= IDent then
            Error (err_identifier_missing, stop_on_error => True);
          else  --  RM 3.8 Component declaration
            T0 := T;
            Enter_Variable;
            while Sy = Comma loop  --  ','  in  "a, b, c : Integer;"
              InSymbol;
              Enter_Variable;
            end loop;
            Need (Colon, err_colon_missing);  --  ':'  in  "a, b, c : Integer;"
            T1 := T;
            TYP (FSys + Comma_END_IDent_Semicolon, ELTP, ELRF, ELSZ);
            while T0 < T1 loop
              T0             := T0 + 1;
              IdTab (T0).TYP := ELTP;
              IdTab (T0).Ref := ELRF;
              IdTab (T0).Adr := Offset;
              Offset         := Offset + ELSZ;
            end loop;
          end if;
          Need (Semicolon, err_semicolon_missing, Forgive => Comma);
          Ignore_Extra_Semicolons;
          exit when Sy = END_Symbol;
        end loop;
        --
        BlockTab (RF).VSize := Offset;
        Sz                  := Offset;
        BlockTab (RF).PSize := 0;
        InSymbol;
        Need (RECORD_Symbol, err_RECORD_missing);  --  (END) RECORD
        Level := Level - 1;
      end Record_Type;

      I : Integer;

    begin  --  Type
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      Test (Type_Begin_Symbol, FSys, err_missing_ARRAY_RECORD_or_ident);
      if Type_Begin_Symbol (Sy) then
        if Id(1..10) = "STRING    " then -- !! Need to implement constraints...
          Sy := String_Symbol;
        end if;
        case Sy is
          when IDent =>
            I := Locate_Identifier (Id);
            if I /= 0 then
              if IdTab (I).Obj = TypeMark then
                TP := IdTab (I).TYP;
                RF := IdTab (I).Ref;
                Sz := IdTab (I).Adr;
                if TP = NOTYP then
                  Error (err_undefined_type);
                end if;
              else
                Error (err_missing_a_type_identifier);
              end if;
            end if;
            InSymbol;

          when ARRAY_Symbol | String_Symbol =>
            StrArray := Sy = String_Symbol;
            InSymbol;
            Need (LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
            TP := Arrays;
            Array_Typ (RF, Sz, StrArray);

          when RECORD_Symbol =>
            Record_Type;
          when LParent =>
            Enumeration_Type;
          when others =>
            null;
        end case; -- Sy
        Test (FSys, Empty_Symset, err_incorrectly_used_symbol);
      end if;
    end TYP;

    ------------------------------------------------------------------
    --------------------------------------------Formal_Parameter_List-
    procedure Formal_Parameter_List is
      RF, Sz, X, T0 : Integer;
      TP            : Types := NOTYP;
      ValParam      : Boolean;
    begin
      InSymbol;
      RF := 0;
      Sz := 0;
      Test (IDent_Set, FSys + RParent, err_identifier_missing, stop_on_error => True);
      while Sy = IDent loop
        T0 := T;
        Enter_Variable;
        while Sy = Comma loop
          InSymbol;
          Enter_Variable;
        end loop;
        if Sy = Colon then
          InSymbol;
          if Sy = IN_Symbol then
            InSymbol;
          end if;
          if Is_a_function then -- if I am a function, no InOut parms allowed
            ValParam := True;
          elsif Sy /= OUT_Symbol then
            ValParam := True;
          else
            InSymbol;
            ValParam := False;
          end if;
          if Sy /= IDent then
            Error (err_identifier_missing);
          else
            X := Locate_Identifier (Id);
            InSymbol;
            if X /= No_Id then
              if IdTab (X).Obj = TypeMark then
                TP := IdTab (X).TYP;
                RF := IdTab (X).Ref;
                if ValParam then
                  Sz := IdTab (X).Adr;
                else
                  Sz := 1;
                end if;
              else
                Error (err_missing_a_type_identifier, stop_on_error => True);
              end if;
            end if;  --  X /= No_Id
          end if;
          Test (Comma_IDent_RParent_Semicolon, FSys, err_semicolon_missing, stop_on_error => True);
          while T0 < T loop
            T0 := T0 + 1;
            declare
              r : TabEntry renames IdTab (T0);
            begin
              r.TYP    := TP;
              r.Ref    := RF;
              r.Normal := ValParam;
              r.Adr    := Dx;
              r.LEV    := Level;
              Dx       := Dx + Sz;
            end;
          end loop;  --  while T0 < T
        else
          Error (err_colon_missing, stop_on_error => True);
        end if;
        if Sy /= RParent then
          Need (Semicolon, err_semicolon_missing, Forgive => Comma);
          Ignore_Extra_Semicolons;
          Test (IDent_Set, FSys + RParent, err_incorrectly_used_symbol);
        end if;
      end loop;  --  while Sy = IDent
      if Sy = RParent then
        InSymbol;
        Test (After_Subprogram_Parameters, FSys, err_incorrectly_used_symbol);
      else
        Error (err_closing_parenthesis_missing);
      end if;
    end Formal_Parameter_List;

    ------------------------------------------------------------------
    -------------------------------------------------Type_Declaration-
    procedure Type_Declaration is
      TP         : Types;
      RF, Sz, T1 : Integer;
    begin
      InSymbol;
      Test (IDent_Set, Semicolon_Set, err_identifier_missing);
      Enter (Id, Id_with_case, TypeMark);
      T1 := T;
      InSymbol;
      Need (IS_Symbol, err_IS_missing);
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      TYP (Comma_IDent_Semicolon + FSys, TP, RF, Sz);
      IdTab (T1).TYP := TP;
      IdTab (T1).Ref := RF;
      IdTab (T1).Adr := Sz;
      --
      Test_Semicolon (FSys);
    end Type_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer);

    ------------------------------------------------------------------
    --------------------------------------------------Var_Declaration-
    procedure Var_Declaration is               -- modified Hathorn
      --  This procedure processes both Variable and Constant declarations.
      T0, T1, RF, Sz, T0i, LC0, LC1 : Integer;
      TP                            : Types;
      is_constant, is_typed,
      is_untyped_constant              : Boolean;
      C                             : ConRec;
    begin
      while Sy = IDent loop
        T0 := T;
        Enter_Variable;
        while Sy = Comma loop
          InSymbol;
          Enter_Variable;
        end loop;
        Need (Colon, err_colon_missing);  --  ':'   in   "x, y : Integer;"
        T1 := T;
        --
        if Sy = IDent then  --MRC 6/91 from PC version
          --  NB (2018-04-02): this duplicates the same operation
          --  from the TYP call, but sets another I...
          I := Locate_Identifier (Id, stop_on_error => True);
        end if;
        Test (Type_Begin_Symbol + CONSTANT_Symbol, Semicolon_Set, err_incorrectly_used_symbol);
        --
        is_constant := False;
        if Sy = CONSTANT_Symbol then  --  Consume "constant" in "x : constant ...;"
          is_constant := True;
          InSymbol;
        end if;
        --
        is_typed := False;
        if Type_Begin_Symbol (Sy) then  --  Here, a type name or an anonymous type definition
          is_typed := True;
          TYP (Becomes_Comma_IDent_Semicolon + FSys, TP, RF, Sz);
        end if;
        Test (Becomes_EQL_Semicolon, Empty_Symset, err_incorrectly_used_symbol);
        --
        if Sy = EQL then
          Error (err_EQUALS_instead_of_BECOMES);
          Sy := Becomes;
        end if;
        --
        is_untyped_constant := is_constant and not is_typed;
        --
        if is_untyped_constant then
          --  Numeric constant: we parse the number here ("k : constant := 123.0").
          if Sy = Becomes then
            InSymbol;
            Number_Declaration (Comma_IDent_Semicolon + FSys, C);
          else
            Error (err_BECOMES_missing);
          end if;
        end if;
        --
        T0i := T0;
        if is_constant or is_typed then  --  All correct cases: untyped variables were caught.
          --  Update identifier table
          while T0 < T1 loop
            T0 := T0 + 1;
            declare
              r : TabEntry renames IdTab (T0);
            begin
              if is_untyped_constant then
                r.Obj := Declared_Number;
                r.TYP := C.TP;
                case C.TP is
                  when Floats =>
                    Enter_Float (C.R);
                    r.Adr := C1;
                  when Ints =>
                    r.Adr := C.I;
                  when others =>
                    Error (err_numeric_constant_expected);
                    --  "boo : constant := True;" or "x: constant := 'a';"
                    --  are wrong in Ada.
                    r.Adr := C.I;
                end case;
              else  --  a variable or a typed constant
                r.TYP := TP;
                r.Ref := RF;
                r.Adr := Dx;
                Dx    := Dx + Sz;
              end if;
            end;
          end loop;  --  While T0 < T1
        end if;
        --
        if is_constant and is_typed then
          --  For typed constants, the ":=" is required and consumed with the Assignment below.
          Test (Becomes_EQL, Empty_Symset, err_BECOMES_missing);
          if Sy = EQL then
            Error (err_EQUALS_instead_of_BECOMES);
            Sy := Becomes;
          end if;
        end if;
        --
        if Sy = Becomes and not is_untyped_constant then
          --  Create constant or variable initialization ObjCode
          LC0 := LC;
          Assignment (T1);
          T0 := T0i;
          while T0 < T1 - 1 loop
            T0 := T0 + 1;
            Emit2 (k_Load_Address, IdTab (T0).LEV, IdTab (T0).Adr);
            Emit2 (k_Push_Value, IdTab (T1).LEV, IdTab (T1).Adr);
            Emit (k_Store);
          end loop;
          --
          LC1 := LC;
          --  reset ObjCode pointer as if ObjCode had not been generated
          LC := LC0;
          --  copy ObjCode to end of ObjCode table
          ICode := ICode + (LC1 - LC0);      -- Size of ObjCode
          while LC0 < LC1 loop
            ObjCode (CMax) := ObjCode (LC0);
            CMax           := CMax - 1;
            LC0            := LC0 + 1;
          end loop;
        end if;
        Test_Semicolon (FSys);
      end loop;  --  While Sy = IDent
    end Var_Declaration;

    ------------------------------------------------------------------
    --------------------------------------------Proc_Func_Declaration-
    procedure Proc_Func_Declaration is
      IsFun: constant Boolean := Sy = FUNCTION_Symbol;
    begin
      InSymbol;
      if Sy /= IDent then
        Error (err_identifier_missing);
        Id := Empty_Alfa;
      end if;
      declare
        Id_subprog_with_case : constant Alfa := Id_with_case;
      begin
        if IsFun then
          Enter (Id, Id_with_case, Funktion);
        else
          Enter (Id, Id_with_case, Prozedure);
        end if;
        InSymbol;
        Block (FSys, IsFun, False, Level + 1, T, IdTab(T).Name, Id_subprog_with_case);
      end;
      if IsFun then
        Emit1 (k_Exit_Function, 1);
      else
        Emit1 (k_Exit_Call, CallSTDP);
      end if;
    end Proc_Func_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------Task_Declaration-
    procedure Task_Declaration is          -- Hathorn
      I, T0             : Integer;
      TaskID            : Alfa;
      saveLineCount     : constant Integer := Line_Count;  --  Source line where Task appeared
    begin
      InSymbol;
      if Sy = BODY_Symbol then     -- Task Body
        InSymbol;
        I      := Locate_Identifier (Id);
        TaskID := IdTab (I).Name;
        BlockTab (IdTab (I).Ref).SrcFrom := saveLineCount;  --(* Manuel *)
        InSymbol;
        Block (FSys, False, False, Level + 1, I, TaskID, TaskID);  --  !! up/low case
        Emit1 (k_Exit_Call, CallSTDP);
      else                         -- Task Specification
        if Sy = IDent then
          TaskID := Id;
        else
          Error (err_identifier_missing);
          Id := Empty_Alfa;
        end if;
        TCount := TCount + 1;
        if TCount > TaskMax then
          Fatal (TASKS);  --  Exception is raised there.
        end if;
        Enter (TaskID, TaskID, aTask);  --  !! casing
        TaskDefTab (TCount) := T;
        Enter_Block (T);
        IdTab (T).Ref := B;
        InSymbol;
        if Sy = Semicolon then
          InSymbol;  --  Task with no entries
        else  --  Parsing the Entry specs
          Need (IS_Symbol, err_IS_missing);
          if Level = LMax then
            Fatal (LEVELS);  --  Exception is raised there.
          end if;
          Level           := Level + 1;
          Display (Level) := B;
          while Sy = ENTRY_Symbol loop
            InSymbol;
            if Sy /= IDent then
              Error (err_identifier_missing);
              Id := Empty_Alfa;
            end if;
            ECount := ECount + 1;
            if ECount > EntryMax then
              Fatal (ENTRIES);  --  Exception is raised there.
            end if;
            Enter (Id, Id_with_case, aEntry);
            EntryTab (ECount) := T;  --  point to identifier table location
            T0                := T;  --  of TaskID
            InSymbol;
            Block (FSys, False, False, Level + 1, T, IdTab(T).Name, IdTab(T).Name_with_case);
            IdTab (T0).Adr := TCount;
            if Sy = Semicolon then
              InSymbol;
            else
              Error (err_semicolon_missing);
            end if;
          end loop; -- Sy = ENTRY_Symbol

          Level := Level - 1;
          Test_END_Symbol;
          if Sy = IDent and Id = TaskID then
            InSymbol;
          else
            Skip (Semicolon, err_incorrect_block_name);
          end if;
          Test_Semicolon (FSys);
        end if;
      end if;
    end Task_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Expression-
    procedure Expression (FSys : Symset; X : in out Item);

    ------------------------------------------------------------------
    ---------------------------------------------------------Selector-
    procedure Selector (FSys : Symset; V : in out Item) is
      X    : Item;
      a, J : Integer;
      err  : Compile_Error;
    begin
      pragma Assert (Selector_Symbol_Loose (Sy));  --  '.' or '(' or (wrongly) '['
      loop
        if Sy = Period then
          InSymbol;                --  Field selector
          if Sy /= IDent then
            Error (err_identifier_missing);
          else
            if V.TYP /= Records then
              Error (err_var_with_field_selector_must_be_record);
            else  --  Search field identifier
              J              := BlockTab (V.Ref).Last;
              IdTab (0).Name := Id;
              while IdTab (J).Name /= Id loop
                J := IdTab (J).Link;
              end loop;
              if J = 0 then
                Error (err_undefined_identifier);
              end if;
              V.TYP := IdTab (J).TYP;
              V.Ref := IdTab (J).Ref;
              a     := IdTab (J).Adr;
              if a /= 0 then
                Emit1 (k_Offset, a);
              end if;
            end if;
            InSymbol;
          end if;
        else    --  Array selector
          if Sy = LBrack then  --  '['
            --  Common mistake by Pascal, Python or R programmers.
            Error (err_left_bracket_instead_of_parenthesis);
          end if;
          loop
            InSymbol;
            Expression (FSys + Comma_RParent + RBrack, X);
            if V.TYP = Arrays then
              a := V.Ref;
              if ArraysTab (a).Index_TYP /= X.TYP then
                Error (err_illegal_array_subscript);
              elsif ArraysTab (a).ELSize = 1 then
                Emit1 (kIndex1, a);
              else
                Emit1 (kIndex, a);
              end if;
              V.TYP := ArraysTab (a).Element_TYP;
              V.Ref := ArraysTab (a).ELREF;
            else
              Error (err_indexed_variable_must_be_an_array);
            end if;
            exit when Sy /= Comma;
          end loop;
          if Sy = RBrack then  --  ']' : same kind of mistake as for '[' ...
            Error (err_right_bracket_instead_of_parenthesis);
            InSymbol;
          else
            Need (RParent, err_closing_parenthesis_missing);
          end if;
        end if;
        exit when not Selector_Symbol_Loose (Sy);
      end loop;
      --
      if FSys = Semicolon_Set then
        err := err_semicolon_missing;
      else
        err := err_incorrectly_used_symbol;
      end if;
      Test (FSys, Empty_Symset, err);
    end Selector;

    ------------------------------------------------------------------
    -------------------------------------------------------------Call-
    procedure Call (FSys : Symset; I, CallType : Integer) is
      --****************************************************************
      -- Generate ObjCode for procedure or Task Entry Call
      -- CallType specifies type of Call
      --  = 0 then standard procedure Call,    CallSTDP
      --  = 1 then standard Task Entry Call,    CallSTDE
      --  = 2 then timed Task Entry Call,      CallTMDE
      --  = 3 then conditional Task Entry Call,   CallCNDE
      --****************************************************************
      X            : Item;
      LastP, CP, K : Integer;
    begin
      Emit1 (kMarkStack, I);
      LastP := BlockTab (IdTab (I).Ref).LastPar;
      CP    := I;
      if Sy = LParent then          --  Actual parameter list
        loop
          InSymbol;
          if CP >= LastP then
            Error (err_number_of_parameters_do_not_match);
          else
            CP := CP + 1;
            if IdTab (CP).Normal then       -- value parameter
              Expression (FSys + Colon_Comma_RParent, X);
              if X.TYP = IdTab (CP).TYP then
                if X.Ref /= IdTab (CP).Ref then
                  Error (err_parameter_types_do_not_match);
                elsif X.TYP = Arrays then
                  Emit1 (k_Load_Block, ArraysTab (X.Ref).Size);
                elsif X.TYP = Records then
                  Emit1 (k_Load_Block, BlockTab (X.Ref).VSize);
                end if;
              elsif X.TYP = Ints and IdTab (CP).TYP = Floats then
                Forbid_Type_Coercion ("value is integer, parameter is floating-point");
                Emit1 (k_Integer_to_Float, 0);
              elsif X.TYP /= NOTYP then
                Error (err_parameter_types_do_not_match);
              end if;
            else              -- Variable (Name) parameter
              if Sy /= IDent then
                Error (err_identifier_missing);
              else
                K := Locate_Identifier (Id);
                InSymbol;
                if K /= 0 then
                  if IdTab (K).Obj /= Variable then
                    Error (err_variable_missing);
                  end if;
                  X.TYP := IdTab (K).TYP;
                  X.Ref := IdTab (K).Ref;
                  if IdTab (K).Normal then
                    Emit2 (k_Load_Address, IdTab (K).LEV, IdTab (K).Adr);
                  else
                    Emit2 (k_Push_Value, IdTab (K).LEV, IdTab (K).Adr);
                  end if;
                   if Selector_Symbol_Loose (Sy) then  --  '.' or '(' or (wrongly) '['
                    Selector (FSys + Colon_Comma_RParent, X);
                  end if;
                  if (X.TYP /= IdTab (CP).TYP) or
                     (X.Ref /= IdTab (CP).Ref)
                  then
                    Error (err_parameter_types_do_not_match);
                  end if;
                end if;
              end if;
            end if;
          end if;
          Test (Comma_RParent, FSys, err_incorrectly_used_symbol);
          exit when Sy /= Comma;
        end loop;
        Need (RParent, err_closing_parenthesis_missing);
      end if;
      if CP < LastP then  --  Too few actual parameters
        Error (err_number_of_parameters_do_not_match);
      end if;

      if CallType = CallSTDP then
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
      else
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
        Emit1 (k_Exit_Call, CallType);  --  Return from Entry Call
      end if;

      if IdTab (I).LEV < Level then
        Emit2 (k_Update_Display_Vector, IdTab (I).LEV, Level);
      end if;
    end Call;

    ------------------------------------------------------------------
    -------------------------------------------------------Entry_Call-
    procedure Entry_Call (FSys : Symset; I, CallType : Integer) is -- Hathorn
      Addr, J : Integer;
    begin
      if Sy /= Period then
        Skip (Semicolon, err_incorrectly_used_symbol);
      else
        InSymbol;                  --  Task Entry Selector
        if Sy /= IDent then
          Skip (Semicolon, err_identifier_missing);
        else
          J              := BlockTab (IdTab (I).Ref).Last;
          IdTab (0).Name := Id;
          while IdTab (J).Name /= Id loop
            J := IdTab (J).Link;
          end loop;

          if J = 0 then
            Error (err_undefined_identifier);
          end if;

          Addr := J;
          InSymbol;
          Call (FSys, Addr, CallType);
        end if;
      end if;
    end Entry_Call;

    ------------------------------------------------------------------
    -------------------------------------------------------Expression-
    procedure Expression (FSys : Symset; X : in out Item) is
      Y  : Item;
      OP : KeyWSymbol;
      F  : Integer;

      procedure Simple_Expression (FSys : Symset; X : in out Item) is
        Y  : Item;
        OP : KeyWSymbol;

        procedure Term (FSys : Symset; X : in out Item) is
          Y  : Item;
          OP : KeyWSymbol;

          procedure Factor (FSys : Symset; X : in out Item) is
            I, F : Integer;
            err  : Compile_Error;

            procedure Standard_Function (SF_Code : Integer) is
              T_Argument : Typ_Set;  --  Expected type of the function's argument
              N  : Integer := SF_Code;
            begin  --  STANDARD FUNCTION NO. N , N >= SF_Clock INDICATES
                   --  a NILADIC FUNCTION.
              if N < SF_Clock then
                Need (LParent, err_missing_an_opening_parenthesis);
                if N < SF_EOF or N > SF_EOLN then
                  Expression (FSys + RParent, X);
                  case N is
                    when SF_Abs =>  --  Abs (NB: in Ada it's an operator, not a function)
                      T_Argument    := Numeric_Typ;
                      IdTab (I).TYP := X.TYP;
                      if X.TYP = Floats then
                        N := N + 1;
                      end if;
                    when SF_T_Val =>  --  S'Val : RM 3.5.5 (5)
                      T_Argument := Ints_Typ;
                    when SF_T_Pos =>  --  S'Pos : RM 3.5.5 (2)
                      T_Argument := Discrete_Typ;
                    when SF_T_Succ | SF_T_Pred =>  -- S'Succ, S'Pred : RM 3.5 (22, 25)
                      T_Argument := Discrete_Typ;
                      IdTab (I).TYP := X.TYP;
                    when SF_Round_Float_to_Int | SF_Trunc_Float_to_Int |
                         SF_Sin | SF_Cos | SF_Exp | SF_Log | SF_Sqrt | SF_Arctan
                      =>
                      T_Argument := Numeric_Typ;
                      if Ints_Typ (X.TYP) then
                        Forbid_Type_Coercion ("value is of integer type; floating-point is expected here");
                        Emit1 (k_Integer_to_Float, 0);
                      end if;
                    --  Random
                    when SF_Random =>
                      T_Argument := Ints_Typ;
                      IdTab (I).TYP := X.TYP;
                    when others =>
                      null;
                  end case;  --  N
                  --
                  if T_Argument (X.TYP) then
                    Emit1 (k_Standard_Functions, N);
                  elsif X.TYP /= NOTYP then
                    Error (err_argument_to_std_function_of_wrong_type);
                  end if;
                else           --  N in [EOF, EOLN]
                  if Sy /= IDent then
                    Error (err_identifier_missing);
                  elsif Id(1..10) = "INPUT     " then  --  Standard_Input
                    Emit2 (k_Standard_Functions, 0, N);
                  else
                    I := Get_File_Pointer (Id);
                    if I = No_File_Index then -- NB: bug: was 0 instead of -1...
                      Error (err_undefined_identifier);
                    else
                      Emit2 (k_Standard_Functions, I, N);
                    end if;
                  end if;
                  InSymbol;
                end if;        --  N in [EOF, EOLN]
                X.TYP := IdTab (I).TYP;
                Need (RParent, err_closing_parenthesis_missing);
              else             --  NILADIC FUNCTION
                case N is
                  when SF_Clock =>
                    Emit1 (k_Standard_Functions, N);
                  when others =>
                    null;
                end case;
              end if;    -- NILADIC FUNCTIONS, N >= SF_Clock
            end Standard_Function;

            procedure Type_Conversion is  --  Ada RM 4.6
              kind : Type_Conversion_Kind := Unknown;
              Type_Id : constant String := Alfa_to_String (Id);
            begin
              Need (LParent, err_missing_an_opening_parenthesis);
              if Type_Id = HAC_Float_Name then
                kind := To_Float;
              elsif Type_Id = HAC_Integer_Name then
                kind := To_Integer;
              end if;
              Expression (FSys + RParent, X);
              case kind is
                when To_Float =>
                  case X.TYP is
                    when Floats =>
                      null;  --  !!  Emit warning "already float"
                    when Ints =>
                      Emit1 (k_Integer_to_Float, 0);
                    when others =>
                      Argument_Type_Not_Supported;
                  end case;
                  X.TYP := Floats;
                when To_Integer =>
                  case X.TYP is
                    when Floats =>  --  Rounding to closest integer (Ada RM 4.6 (33)).
                      Emit1 (k_Standard_Functions, SF_Round_Float_to_Int);
                    when Ints =>
                      null;  --  !!  Emit warning "already integer"
                    when others =>
                      Argument_Type_Not_Supported;
                  end case;
                  X.TYP := Ints;
                when Unknown =>
                  Error (err_type_conversion_not_supported, "no support for target type " & Type_Id);
              end case;
              Need (RParent, err_closing_parenthesis_missing);
            end Type_Conversion;

          begin  --  Factor
            X.TYP := NOTYP;
            X.Ref := 0;
            Test (Factor_Begin_Symbol + StrCon, FSys,
              err_factor_unexpected_symbol
            );
            if Sy = StrCon then
              X.TYP := Strings;
              Emit1 (k_Literal, SLeng);       -- String Length
              Emit1 (k_Literal, INum);        -- pointer To String IdTab
              InSymbol;
            end if;
            while Factor_Begin_Symbol (Sy) loop
              case Sy is
                when IDent =>
                  I := Locate_Identifier (Id, stop_on_error => True);
                  InSymbol;
                  exit when I = No_Id;  --  Id not found, error already issued by Locate_Identifier
                  declare
                    r : TabEntry renames IdTab (I);
                  begin
                    case r.Obj is
                      when Declared_Number =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                        if X.TYP = Floats then
                          Emit1 (k_Load_Float, r.Adr);
                        else
                          Emit1 (k_Literal, r.Adr);
                        end if;
                        --
                      when Variable =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                         if Selector_Symbol_Loose (Sy) then  --  '.' or '(' or (wrongly) '['
                          if r.Normal then
                            F := k_Load_Address;
                          else
                            F := k_Push_Value;
                          end if;
                          Emit2 (F, r.LEV, r.Adr);
                          Selector (FSys, X);
                          if Standard_or_Enum_Typ (X.TYP) then
                            Emit (kCase34);
                          end if;
                        else
                          if Standard_or_Enum_Typ (X.TYP) then
                            if r.Normal then
                              F := k_Push_Value;
                            else
                              F := k_Push_Indirect_Value;
                            end if;
                          elsif r.Normal then
                            F := k_Load_Address;
                          else
                            F := k_Push_Value;
                          end if;
                          Emit2 (F, r.LEV, r.Adr);
                        end if;
                        --
                      when TypeMark =>
                        Type_Conversion;
                        --
                      when Prozedure =>
                        Error (err_expected_constant_function_variable_or_subtype);
                        --
                      when Funktion =>
                        X.TYP := r.TYP;
                        if r.LEV = 0 then
                          Standard_Function (r.Adr);
                        else
                          Call (FSys, I, CallSTDP);
                        end if;
                        --
                      when others =>
                        null;
                    end case;
                  end;
                  --
                when CharCon | IntCon | FloatCon =>
                  if Sy = FloatCon then
                    X.TYP := Floats;
                    Enter_Float (RNum);
                    Emit1 (k_Load_Float, C1);
                  else
                    if Sy = CharCon then
                      X.TYP := xChars;
                    else
                      X.TYP := Ints;
                    end if;
                    Emit1 (k_Literal, INum);
                  end if;
                  X.Ref := 0;
                  InSymbol;
                  --
                when LParent =>    --  (
                  InSymbol;
                  Expression (FSys + RParent, X);
                  Need (RParent, err_closing_parenthesis_missing);
                  --
                when NOT_Symbol =>
                  InSymbol;
                  Factor (FSys, X);
                  if X.TYP = Bools then
                    Emit (k_NOT_Boolean);
                  elsif X.TYP /= NOTYP then
                    Error (err_resulting_type_should_be_Boolean);
                  end if;
                  --
                when others =>
                  null;
              end case;
              --
              if FSys = Semicolon_Set then
                err := err_semicolon_missing;
              else
                err := err_incorrectly_used_symbol;
              end if;
              Test (FSys, Factor_Begin_Symbol, err);
            end loop;
          end Factor;

        begin  --  Term
          Factor (FSys + FactorZ, X);
          while FactorZ (Sy) loop
            OP := Sy;
            InSymbol;
            Factor (FSys + FactorZ, Y);
            if OP = xTimes then     --  *
              if X.TYP /= Y.TYP then
                Forbid_Type_Coercion ("for this standard operator, types must be the same");
              end if;
              case X.TYP is
                when NOTYP =>
                  null;
                when Ints =>
                  Emit (k_MULT_Integer);
                when Floats =>
                  Emit (k_MULT_Float);
                when others =>
                  Error (err_operator_not_defined_for_types);
              end case;
            elsif OP = Divide then    --  /
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_DIV_Integer);
              else
                if X.TYP = Ints then
                  Forbid_Type_Coercion ("left operand's type is integer, right operand's isn't");
                  Emit1 (k_Integer_to_Float, 1);  --  NB: this assumed Y.TYP was Floats!
                  X.TYP := Floats;
                end if;
                if Y.TYP = Ints then
                  Forbid_Type_Coercion ("right operand's type is integer, left operand's isn't");
                  Emit1 (k_Integer_to_Float, 0);  --  NB: this assumed Y.TYP was Floats!
                  Y.TYP := Floats;
                end if;
                if X.TYP = Floats and Y.TYP = Floats then
                  Emit (k_DIV_Float);
                else
                  if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                    Error (err_illegal_type_for_arithmetic_expression);
                  end if;
                  X.TYP := NOTYP;
                end if;
              end if;
            elsif OP = AND_Symbol then
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (k_AND_Boolean);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (err_resulting_type_should_be_Boolean);
                end if;
                X.TYP := NOTYP;
              end if;
            elsif OP = MOD_Symbol then
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_MOD_Integer);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (err_mod_requires_integer_arguments);
                end if;
                X.TYP := NOTYP;
              end if;
            elsif OP = xx_Power then
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_Power_Integer);
              elsif X.TYP = Floats and Y.TYP = Ints then
                Emit (k_Power_Float_Integer);
              elsif X.TYP = Floats and Y.TYP = Floats then
                Emit (k_Power_Float);
              else
                Error (err_invalid_power_operands);
              end if;
            else
              raise Internal_error with "Unknown operator in Term";
            end if;
          end loop;
        end Term;

      begin  --  Simple_Expression
        --  + , -
        if Plus_Minus (Sy) then
          OP := Sy;
          InSymbol;
          Term (FSys + Plus_Minus, X);
          if X.TYP > Floats then
            Error (err_illegal_type_for_arithmetic_expression);
          elsif OP = MinUS then
            if X.TYP = Floats then
              Emit (k_Unary_MINUS_Float);
            else
              Emit (k_Unary_MINUS_Integer);
            end if;
          end if;
        else
          Term (FSys + TermZ, X);
        end if;
        while TermZ (Sy) loop
          OP := Sy;
          InSymbol;
          Term (FSys + TermZ, Y);
          case OP is
            when OR_Symbol =>
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (k_OR_Boolean);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (err_resulting_type_should_be_Boolean);
                end if;
                X.TYP := NOTYP;
              end if;
            when XOR_Symbol =>
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (k_XOR_Boolean);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (err_resulting_type_should_be_Boolean);
                end if;
                X.TYP := NOTYP;
              end if;
            when Plus | MinUS =>
              if X.TYP /= Y.TYP then
                Forbid_Type_Coercion ("for this standard operator, types must be the same");
              end if;
              case X.TYP is
                when NOTYP =>
                  null;  --  Already in error.
                when Ints =>
                  if OP = Plus then
                    Emit (k_ADD_Integer);
                  else
                    Emit (k_SUBTRACT_Integer);
                  end if;
                when Floats =>
                  if OP = Plus then
                    Emit (k_ADD_Float);
                  else
                    Emit (k_SUBTRACT_Float);
                  end if;
                when others =>
                  Error (err_operator_not_defined_for_types);
              end case;
            when others =>  --  Doesn't happen: TermZ(OP) is True.
              null;
          end case;
        end loop;
      end Simple_Expression;

    begin  --  Expression
      Simple_Expression (FSys + Comparison_Operator, X);
      if Comparison_Operator (Sy) then
        OP := Sy;
        InSymbol;
        Simple_Expression (FSys, Y);
        if X.TYP = Ints and Y.TYP = Floats then
          Forbid_Type_Coercion ("left operand's type is integer, right operand's is floating-point");
          X.TYP := Floats;
          Emit1 (k_Integer_to_Float, 1);
        end if;
        if Y.TYP = Ints and X.TYP = Floats then
          Forbid_Type_Coercion ("left operand's type is floating-point, right operand's is integer");
          Y.TYP := Floats;
          Emit1 (k_Integer_to_Float, 0);
        end if;
        if X.TYP = Enums and Y.TYP = Enums and X.Ref /= Y.Ref then
          Error (err_incompatible_types_for_comparison);
        end if;
        if X.TYP = Y.TYP then
          if X.TYP = Floats then
            F := 0;
          else
            F := 6;
          end if;
          case OP is
            when EQL =>
              Emit (k_EQL_Float + F);
            when NEQ =>
              Emit (k_NEQ_Float + F);
            when LSS =>
              Emit (k_LSS_Float + F);
            when LEQ =>
              Emit (k_LEQ_Float + F);
            when GTR =>
              Emit (k_GTR_Float + F);
            when GEQ =>
              Emit (k_GEQ_Float + F);
            when others =>
              null;
          end case;
        else
          Error (err_incompatible_types_for_comparison);
        end if;
        X.TYP := Bools;
      end if;
    end Expression;

    procedure Boolean_Expression (FSys : Symset; X : in out Item) is
    begin
      Expression (FSys, X);
      Check_Boolean (X.TYP);
    end Boolean_Expression;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer) is
      X, Y : Item;
      F    : Integer;
    -- IdTab[I].Obj = Variable
    begin
      X.TYP := IdTab (I).TYP;
      X.Ref := IdTab (I).Ref;
      if IdTab (I).Normal then
        F := k_Load_Address;
      else
        F := k_Push_Value;
      end if;
      Emit2 (F, IdTab (I).LEV, IdTab (I).Adr);
      if Selector_Symbol_Loose (Sy) then  --  '.' or '(' or (wrongly) '['
        Selector (Becomes_EQL + FSys, X);
      end if;
      if Sy = Becomes then
        InSymbol;
      elsif Sy = EQL then
        --  Common mistake by BASIC or C programmers.
        Error (err_EQUALS_instead_of_BECOMES);
        InSymbol;
      else
        Error (err_BECOMES_missing);
      end if;
      Expression (Semicolon_Set, Y);
      if X.TYP = Y.TYP then
        if Standard_Typ (X.TYP) then
          Emit (k_Store);
        elsif X.Ref /= Y.Ref then
          Error (err_types_of_assignment_must_match);
        else
          case X.TYP is
            when Arrays =>
              Emit1 (k_Copy_Block, ArraysTab (X.Ref).Size);
            when Records =>
              Emit1 (k_Copy_Block, BlockTab (X.Ref).VSize);
            when Enums =>  --  Behaves like a standard type
              Emit (k_Store);
            when others =>
              null;
          end case;
        end if;
      elsif X.TYP = Floats and Y.TYP = Ints then
        Forbid_Type_Coercion ("integer type value assigned to floating-point variable");
        Emit1 (k_Integer_to_Float, 0);
        Emit (k_Store);
      elsif X.TYP = Arrays and Y.TYP = Strings then
        if ArraysTab (X.Ref).Element_TYP /= xChars then
          Error (err_types_of_assignment_must_match);
        else
          Emit1 (k_String_assignment, ArraysTab (X.Ref).Size);  --  array size
        end if;
      elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
        Error (err_types_of_assignment_must_match);
      end if;
    end Assignment;

    ------------------------------------------------------------------
    --------------------------------------------------------Statement--
    procedure Statement (FSys : Symset) is
      I : Integer;

      procedure Multi_Statement (Sentinal : Symset) is   -- Hathorn
        nxtSym : Symset;
      begin
        if Sentinal (Sy) then -- GdM 15-Aug-2014: there should be at least one statement.
          Error (err_statement_expected);
        end if;
        nxtSym := Sentinal + Statement_Begin_Symbol;
        loop
          Statement (nxtSym); --MRC,was: UNTIL (Sy IN Sentinal);
          exit when Sentinal (Sy) or Err_Count > 0;
        end loop;
      end Multi_Statement;

      procedure Accept_Statement is            -- Hathorn
        I : Integer;

        procedure Accept_Call (FSys : Symset; I : Integer) is
          pragma Unreferenced (I, FSys);
        begin   --  !!  Check to make sure parameters match with Entry Statement
          if Sy = Semicolon then
            return;
          end if;
          if Sy = LParent then          -- <--- temporary
            while not (Sy = DO_Symbol or Sy = RParent) loop
              InSymbol;
            end loop; -- !! should check no. and
          end if;    -- Types of parms.
          if Sy = RParent then
            InSymbol;
          end if;
        end Accept_Call;

      begin  --  Accept_Statement
        InSymbol;
        I := Locate_Identifier (Id);
        if IdTab (I).Obj /= aEntry then
          Error (err_use_Small_Sp);
        end if;
        InSymbol;
        Accept_Call (FSys, I);
        Emit1 (k_Accept_Rendezvous, I);
        if Sy = DO_Symbol then
          if Level = LMax then
            Fatal (LEVELS);  --  Exception is raised there.
          end if;
          Level           := Level + 1;
          Display (Level) := IdTab (I).Ref;
          InSymbol;
          Multi_Statement (END_Set);
          Test_END_Symbol;
          if Sy = IDent then
            if Id /= IdTab (I).Name then
              Error (err_incorrect_block_name);
            end if;
            InSymbol;
          end if;
          Level := Level - 1;
        end if;
        Emit1 (k_End_Rendezvous, I);
      end Accept_Statement;

      procedure Exit_Statement is
        --  Generate an absolute branch statement with a dummy end loop address
        X : Item;
      begin
        pragma Assert (Sy = EXIT_Symbol);
        InSymbol;  --  Consume EXIT symbol.
        if Sy = WHEN_Symbol then  --  Conditional Exit
          InSymbol;
          Boolean_Expression (Semicolon_Set, X);
          Emit1 (k_Conditional_Jump, LC + 2);  --  Conditional jump around Exit
        end if;
        Emit1 (k_Jump, dummy_address);  --  Unconditional jump with dummy address to be patched
      end Exit_Statement;

      procedure IF_Statement is
        X        : Item;
        LC0, LC1 : Integer;
      begin
        InSymbol;
        Boolean_Expression (FSys + DO_THEN, X);
        LC1 := LC;
        Emit (k_Conditional_Jump);                  -- JMPC
        Need (THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
        Multi_Statement (ELSE_ELSIF_END);
        LC0 := LC;
        --
        while Sy = ELSIF_Symbol loop  --  Added Hathorn
          InSymbol;
          Emit1 (k_Jump, dummy_address);  --  Unconditional jump with dummy address to be patched
          ObjCode (LC1).Y := LC;          --  Patch the previous conditional jump
          Boolean_Expression (FSys + DO_THEN, X);
          LC1 := LC;
          Emit (k_Conditional_Jump);      -- JMPC
          Need (THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
          Multi_Statement (ELSE_ELSIF_END);
        end loop;
        --
        if Sy = ELSE_Symbol then
          InSymbol;
          Emit1 (k_Jump, dummy_address);
          ObjCode (LC1).Y := LC;
          Multi_Statement (END_Set);
        else
          ObjCode (LC1).Y := LC;
        end if;
        Need (END_Symbol, err_END_missing);         --  END (IF)
        Need (IF_Symbol,  err_missing_closing_IF);  --  (END) IF
        --  Go back and patch the dummy addresses in unconditional jumps
        while LC0 < LC loop
          if ObjCode (LC0).Y = dummy_address then
            ObjCode (LC0).Y := LC;
          end if;
          LC0 := LC0 + 1;
        end loop;
      end IF_Statement;

      procedure LOOP_Statement (FCT, B : Integer) is    -- Hathorn
        LC0 : Integer := LC;
      begin
        if Sy = LOOP_Symbol then
          InSymbol;
        else
          Skip (Statement_Begin_Symbol, err_missing_closing_IF);
        end if;
        Multi_Statement (END_Set);
        Emit1 (FCT, B);
        Need (END_Symbol,  err_END_missing);           --  END (LOOP)
        Need (LOOP_Symbol, err_closing_LOOP_missing);  --  (END) LOOP
        --  Go back and patch the dummy addresses generated by Exit statements.
        while LC0 < LC loop
          if ObjCode (LC0).Y = dummy_address then
            ObjCode (LC0).Y := LC;
          end if;
          LC0 := LC0 + 1;
        end loop;
      end LOOP_Statement;

      procedure RETURN_Statement is           -- Hathorn
        -- Generate a procedure return Statement, calculate
        -- return value if req'D
        X, Y : Item;
        F    : Integer;
      begin
        if Block_ID = Main_Program_ID then
          Error (err_illegal_return_statement_from_main); -- !! but... this is legal in Ada !!
        end if;
        I := Locate_Identifier (Block_ID);
        InSymbol;
        if Sy = Semicolon then
          if Is_a_function then
            Error (err_functions_must_return_a_value);
          end if;
        else
          if not Is_a_function then
            Error (err_procedures_cannot_return_a_value, stop_on_error => True);
          end if;
          --  Calculate return value (destination: X; expression: Y).
          if IdTab (I).Ref = Display (Level) then
            X.TYP := IdTab (I).TYP;
            X.Ref := IdTab (I).Ref;
            if IdTab (I).Normal then
              F := k_Load_Address;
            else
              F := k_Push_Value;
            end if;
            Emit2 (F, IdTab (I).LEV + 1, 0);
            --
            Expression (Semicolon_Set, Y);
            if X.TYP = Y.TYP then
              if Standard_Typ (X.TYP) then
                Emit (k_Store);
              elsif X.Ref /= Y.Ref then
                Error (err_types_of_assignment_must_match);
              end if;
            elsif X.TYP = Floats and Y.TYP = Ints then
              Forbid_Type_Coercion ("integer type value returned as floating-point");
              Emit1 (k_Integer_to_Float, 0);
              Emit (k_Store);
            elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
              Error (err_types_of_assignment_must_match);
            end if;
          else
            Error (err_illegal_return_statement_from_main);
          end if;       -- !! but... this is legal in Ada !!
        end if;
        if Is_a_function then
          Emit1 (k_Exit_Function, CallSTDP);
        else
          Emit1 (k_Exit_Call, CallSTDP);
        end if;
      end RETURN_Statement;

      procedure Delay_Statement is            -- Cramer
        -- Generate a Task delay
        Y : Item;
      begin
        InSymbol;
        if Sy = Semicolon then
          Skip (Semicolon, err_missing_expression_for_delay);
        else                  -- calculate delay value
          Expression (Semicolon_Set, Y);
          if Y.TYP /= Floats then
            Error (err_wrong_type_in_DELAY);
          end if;
        end if;
        Emit (k_Delay);
      end Delay_Statement;

      procedure CASE_Statement is
        X         : Item;
        I, J, LC1 : Integer;
        type GrounfZ is record
          Val, LC : Index;
        end record;
        CaseTab : array (1 .. CSMax) of GrounfZ;
        ExitTab : array (1 .. CSMax) of Integer;

        procedure CASE_Label is
          Lab : ConRec;
          K   : Integer;
        begin
          Number_Declaration (FSys + Alt_Finger, Lab);
          if Lab.TP /= X.TYP then
            Error (err_case_label_not_same_type_as_case_clause);
          elsif I = CSMax then
            Fatal (OBJECTS);  --  Exception is raised there.
          else
            I               := I + 1;
            CaseTab (I).Val := Lab.I;
            CaseTab (I).LC  := LC;
            K               := 0;
            loop
              K := K + 1;
              exit when CaseTab (K).Val = Lab.I;
            end loop;
            if K < I then
              Error (err_duplicate_case_choice_value);
            end if;
          end if;
        end CASE_Label;

        procedure One_CASE is
        begin
          if Sy = WHEN_Symbol then
            InSymbol;
            if Constant_Definition_Begin_Symbol (Sy) then
              CASE_Label;
              while Sy = Alt loop
                InSymbol;
                CASE_Label;
              end loop;
            end if;
            if Sy = OTHERS_Symbol then        -- Hathorn
              if I = CSMax then
                Fatal (OBJECTS);  --  Exception is raised there.
              end if;
              I               := I + 1;
              CaseTab (I).Val := 0;
              CaseTab (I).LC  := -LC;
              InSymbol;
            end if;
            Need (Finger, err_FINGER_missing);
            Multi_Statement (END_WHEN);
            J           := J + 1;
            ExitTab (J) := LC;
            Emit (k_Jump);
          else
            Error (err_WHEN_missing);
          end if;
        end One_CASE;

      begin  --  CASE_Statement
        InSymbol;
        I := 0;
        J := 0;
        Expression (FSys + Colon_Comma_IS_OF, X);
        if not (X.TYP = Ints or
                X.TYP = Bools or
                X.TYP = xChars or
                X.TYP = NOTYP)
        then
          Error (err_bad_type_for_a_case_statement); --- !! mmmh: enums ?...
        end if;
        LC1 := LC;
        Emit (kSwitch); -- JMPX
        if Sy = IS_Symbol then -- Was OF_Symbol in SmallAda ! I.e. case x OF when 1 => ...
          InSymbol;
        elsif Sy = OF_Symbol then
          Error (err_OF_instead_of_IS);  --  Common mistake by Pascal programmers
          InSymbol;
        else
          Error (err_IS_missing);
        end if;

        while Sy = WHEN_Symbol loop  --  Each case is parsed here.
          One_CASE;
        end loop;

        ObjCode (LC1).Y := LC;
        for K in 1 .. I loop
          if CaseTab (K).LC > 0 then
            Emit2 (k_Switch_2, 1, CaseTab (K).Val);
            Emit1 (k_Switch_2, CaseTab (K).LC);
          else
            Emit2 (k_Switch_2, -1, CaseTab (K).Val);
            Emit1 (k_Switch_2, -CaseTab (K).LC);
          end if;
        end loop;

        Emit1 (k_Jump, 0);
        for K in 1 .. J loop
          ObjCode (ExitTab (K)).Y  := LC;
        end loop;
        Need (END_Symbol,  err_END_missing);           --  END (CASE)
        Need (CASE_Symbol, err_missing_closing_CASE);  --  (END) CASE
      end CASE_Statement;

      procedure WHILE_Statement is
        X        : Item;
        LC1, LC2 : Integer;
      begin
        InSymbol;  --  Consume WHILE symbol.
        LC1 := LC;
        Boolean_Expression (FSys + DO_LOOP, X);
        LC2 := LC;
        Emit (k_Conditional_Jump);
        LOOP_Statement (k_Jump, LC1);
        ObjCode (LC2).Y := LC;
      end WHILE_Statement;

      procedure FOR_Statement is
        X            : Item;
        F, LC1, last : Integer;
      begin
        InSymbol;  --  Consume FOR symbol.
        if Sy = IDent then
          if T = TMax then
            Fatal (IDENTIFIERS);  --  Exception is raised there.
          end if;
          --  Declare local loop control Variable  --  added Hathorn
          last := BlockTab (Display (Level)).Last;
          T    := T + 1;
          declare
            r : TabEntry renames IdTab (T);
          begin
            r.Name   := Id;
            r.Link   := last;
            r.Obj    := Variable;
            r.TYP    := NOTYP;
            r.Ref    := 0;
            r.Normal := True;
            r.LEV    := Level;
            r.Adr    := Dx;
          end;
          BlockTab (Display (Level)).Last  := T;
          Dx                               := Dx + 1;
          if Dx > MaxDX then
            MaxDX := Dx;
          end if;
          BlockTab (Display (Level)).VSize  := MaxDX;
        else
          Skip (Fail_after_FOR + FSys, err_identifier_missing);
        end if;
        --
        Emit2 (k_Load_Address, IdTab (T).LEV, IdTab (T).Adr);
        InSymbol;
        F := kFor1;
        if Sy = IN_Symbol then
          InSymbol;
          if Sy = REVERSE_Symbol then
            F := kFor1Rev;
            InSymbol;
          end if;
          Expression (END_LOOP_RANGE_Double_Dot + FSys, X);
          IdTab (T).TYP := X.TYP;
          if not (X.TYP = Ints
                 or X.TYP = Bools
                 or X.TYP = xChars)
          then
            Error (err_control_variable_of_the_wrong_type);
          end if;
          if Sy = Range_Double_Dot_Symbol then
            InSymbol;
            Expression (FSys + LOOP_Symbol, X);
            if IdTab (T).TYP /= X.TYP then
              Error (err_first_and_last_must_have_matching_types);
            end if;
          else
            Skip (END_LOOP_Semicolon + FSys, err_expecting_double_dot);
          end if;
        else
          Skip (FSys + LOOP_Symbol, err_IN_missing);
        end if;
        LC1 := LC;
        Emit (F);
        LOOP_Statement (F + 1, LC);
        ObjCode (LC1).Y                  := LC;
        T                                := T - 1;
        BlockTab (Display (Level)).Last  := last;
        Dx                               := Dx - 1;
      end FOR_Statement;

      procedure Select_Statement is
        procedure Select_Error (N : Compile_Error) is
        begin
          Skip (Semicolon, N);
        end Select_Error;

        -- Either a Timed or Conditional Entry Call.

        procedure Qualified_Entry_Call is
          I, J, IStart, IEnd : Integer;
          patch              : array (0 .. 4) of Integer;
          O                  : Order;
          Y                  : Item;
        begin
          I := Locate_Identifier (Id);
          if IdTab (I).Obj = aTask then
            InSymbol;
            Entry_Call (FSys, I, -1);
            if ObjCode (LC - 2).F = kCall then  --  Need to patch CallType later
              patch (0) := LC - 2;
            else
              patch (0) := LC - 3;
            end if;       -- LC-1 must be OP=3, update Display
            patch (1) := LC;  --  Need to patch in JMPC address later
            Emit1 (k_Conditional_Jump, dummy_address);    -- JMPC, address patched in after ELSE
                                      --or OR
            if Sy = Semicolon then
              InSymbol;
            else
              Skip (Semicolon, err_semicolon_missing);
            end if;
            if not (Sy = OR_Symbol or else Sy = ELSE_Symbol) then
              Multi_Statement (ELSE_OR);
            end if;
            if Sy = OR_Symbol then -- =====================> Timed Entry Call
              ObjCode (patch (0)).X      := CallTMDE; -- Timed Entry Call
              ObjCode (patch (0) + 1).Y  := CallTMDE; -- Exit type matches
                                                      --Entry type
              InSymbol;
              if Sy = DELAY_Symbol then
                InSymbol;
                if Sy = Semicolon then
                  Select_Error (err_missing_expression_for_delay);
                else          -- calculate delay value
                  patch (2) := LC;
                  Expression (Semicolon_Set, Y);
                  patch (3) := LC - 1;
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  else        -- end of timed Entry select ObjCode, do patching
                    ObjCode (patch (1)).Y  := LC; -- if Entry not made, Skip rest
                    J                      := patch (3) - patch (2) + 1;
                    IStart                 := patch (0);
                    IEnd                   := LC - 1;
                    while J > 0 loop     -- move delay time ObjCode To before
                      O := ObjCode (IEnd);  -- opcodes kCall, k_Exit_Call
                      for I in reverse IEnd - 1 .. IStart loop
                        ObjCode (I + 1) := ObjCode (I);
                      end loop;
                      ObjCode (IStart) := O;
                      J                := J - 1;
                    end loop;
                    InSymbol;
                  end if;
                end if;
              else
                Select_Error (err_expecting_DELAY);
              end if;
            -- end Sy = OrSy
            else              -- Sy = ELSE_Symbol, ===============> Conditional
                              --                             Entry Call
              ObjCode (patch (0)).X      := CallCNDE; -- Conditional Entry Call
              ObjCode (patch (0) + 1).Y  := CallCNDE;
              patch (2)                  := LC;
              Emit1 (k_Jump, dummy_address);          -- JMP, address patched in after END
                                          --SELECT
              patch (3) := LC;
              InSymbol;
              Multi_Statement (END_Set);
              ObjCode (patch (1)).Y  := patch (3);
              ObjCode (patch (2)).Y  := LC;
            end if;
            if Sy /= END_Symbol then
              Select_Error (err_END_missing);
            end if;
            InSymbol;
            if Sy /= SELECT_Symbol then
              Select_Error (err_SELECT_missing);
            end if;
          else
            Select_Error (err_expecting_task_entry);
          end if;          -- Task.Entry Call expected
        end Qualified_Entry_Call;

        procedure Selective_Wait is         -- Kurtz <===================
          -- Jay, this Buds for you !!

          type patch_ptr is array (1 .. 10) of Integer;

          JSD, Alt            : patch_ptr;
          ISD, IAlt, StartSel : Integer;
          SelectDone          : Boolean;
          Y, X                : Item;
          do_terminate        : Boolean;

          procedure Accept_Statement_2 is      -- Kurtz
            I : Integer;

            procedure Accept_Call_2 (FSys : Symset; I : Integer) is
            pragma Unreferenced (FSys, I);
            begin
              --  Check to make sure parameters match with Entry Statement
              if Sy = Semicolon then
                return;
              end if;
              if Sy = LParent then      -- should be modified
                -- To check no. and
                while not (Sy = DO_Symbol or Sy = RParent) loop
                  InSymbol;
                end loop;
              end if;        -- of parameters.
              if Sy = RParent then
                InSymbol;
              end if;
            end Accept_Call_2;

          begin         -- Accept_Statment_2
            InSymbol;
            I := Locate_Identifier (Id);
            if IdTab (I).Obj /= aEntry then
              Select_Error (err_use_Small_Sp);
            end if;
            InSymbol;
            Accept_Call_2 (FSys, I);
            Emit2 (k_Selective_Wait, 2, I);          -- Retain Entry Index
            if IAlt < 10 then
              IAlt := IAlt + 1;
            else
              Fatal (PATCHING);
            end if;
            Alt (IAlt) := LC;              -- SAVE LOCATION FOR PATCHING
            Emit2 (k_Selective_Wait, 3, LC); -- ACCEPT IF Ready ELSE Skip To LC
            -- CONDITIONAL ACCEPT MUST BE ATOMIC
            if Sy = DO_Symbol then
              if Level = LMax then
                Fatal (LEVELS);  --  Exception is raised there.
              end if;
              Level           := Level + 1;
              Display (Level) := IdTab (I).Ref;
              InSymbol;
              Multi_Statement (END_Set);
              Test_END_Symbol;
              if Sy = IDent then
                if Id /= IdTab (I).Name then
                  Select_Error (err_incorrect_block_name);
                end if;
              end if;
              Level := Level - 1;
              InSymbol;
            end if;
            Emit1 (k_End_Rendezvous, I);
          end Accept_Statement_2;

        begin  --  Selective_Wait ===============================> Kurtz
          ISD          := 0;
          IAlt         := 0;
          SelectDone   := False;
          do_terminate := False;
          StartSel     := LC;
          Emit2 (k_Selective_Wait, 1, 0); -- START OF SELECT SELECTIVE Wait
                                        -- SEQUENCE
          loop
            case Sy is
              when WHEN_Symbol =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                InSymbol;          -- WHENSTATEMENT
                Boolean_Expression (FSys + Finger, X);
                InSymbol;
                if Sy = ACCEPT_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Conditional_Jump);
                    Accept_Statement_2;
                  end if;
                elsif Sy = DELAY_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Conditional_Jump);
                    InSymbol;
                    Expression (FSys + Semicolon, Y);
                    Emit2 (k_Selective_Wait, 4, LC + 2);  --  Update delay time
                    if Y.TYP /= Floats then
                      Select_Error (err_wrong_type_in_DELAY);
                    end if;
                    if IAlt > 10 then
                      Fatal (PATCHING);
                    end if;
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Jump);
                  end if;
                else
                  Select_Error (err_missing_a_procedure_declaration);
                end if;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);          --  patch JMP ADDRESS AT EndSy
              -- end WHEN_Symbol

              when ACCEPT_Symbol =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                --  patch
                IAlt := 0;
                Accept_Statement_2;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);

              when OR_Symbol =>  --  OR STATEMENT
                InSymbol;

              when ELSE_Symbol =>
                for I in 1 .. IAlt loop
                  --  patch ObjCode
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                IAlt := 0;
                InSymbol;
                Multi_Statement (END_Set);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end ELSE_Symbol

              when DELAY_Symbol =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                --  patch
                IAlt := 0;
                --  Generate a Task delay, calculate return value if req'D
                InSymbol;
                if Sy = Semicolon then
                  Skip (Semicolon, err_missing_expression_for_delay);
                else          -- calculate return value
                  Expression (Semicolon_Set, Y);
                  Emit2 (k_Selective_Wait, 4, LC + 2);  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  end if;
                  IAlt       := IAlt + 1;
                  Alt (IAlt) := LC;
                  Emit (k_Jump);
                end if;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end DELAY_Symbol

              when TERMINATE_Symbol =>
                InSymbol;
                if Sy /= Semicolon then
                  Select_Error (err_semicolon_missing);
                end if;
                do_terminate := True;        -- Oguz
                InSymbol;

              when END_Symbol =>
                InSymbol;
                if Sy /= SELECT_Symbol then
                  Select_Error (err_END_missing);
                end if;
                SelectDone := True;
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                if do_terminate then
                  Emit2 (k_Selective_Wait, 5, StartSel);
                else
                  Emit2 (k_Selective_Wait, 6, StartSel);
                end if;   -- Suspend
                for I in 1 .. ISD loop
                  ObjCode (JSD (I)).Y  := LC;
                end loop;
                -- patch
                ISD := 0;
              -- end EndSy

              when others =>
                SelectDone := True;
            end case;
            exit when SelectDone;
          end loop;
        end Selective_Wait;

      begin
        InSymbol;  --  Consume SELECT_Symbol.
        if Sy = ACCEPT_Symbol or Sy = WHEN_Symbol then
          Selective_Wait;
          InSymbol;
        elsif Sy = IDent then  --  Task Entry objectName.
          Qualified_Entry_Call;
          InSymbol;
          -- Timed or Conditional Entry Call (?)
        else
          Select_Error (err_expecting_accept_when_or_entry_id);
        end if;
      end Select_Statement;

      procedure Standard_Procedure (N : Integer) is
        -- NB: Most of this part will disappear when Ada.Text_IO etc.
        -- are implemented, as well as overloading.
        I, F              : Integer;
        X, Y              : Item;
        do_first_InSymbol : Boolean := True;
      begin
        case N is -- Numbers: see EnterStdFcns in HAC.Compiler
          when 1 | 2 =>  -- GET, GET_LINE
            if Sy = LParent then
              InSymbol;
              I := Get_File_Pointer (Id);  -- Schoening
              if I = No_File_Index then
                Emit1 (k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              else -- First parameter is a file variable
                Emit1 (k_Set_current_file_pointer, I);
                InSymbol;
                if Sy /= Comma then
                  if Sy = RParent then
                    goto SKIP1b;
                  else
                    Error (err_identifier_missing);
                  end if;
                end if;
              end if;
              loop
                if do_first_InSymbol then
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if Sy /= IDent then
                  Error (err_identifier_missing);
                else
                  I := Locate_Identifier (Id);
                  InSymbol;
                  if I /= 0 then
                    if IdTab (I).Obj /= Variable then
                      Error (err_variable_missing);
                    else
                      X.TYP := IdTab (I).TYP;
                      X.Ref := IdTab (I).Ref;
                      if IdTab (I).Normal then
                        F := 0;
                      else
                        F := 1;
                      end if;
                      Emit2 (F, IdTab (I).LEV, IdTab (I).Adr);
                      if Selector_Symbol_Loose (Sy) then  --  '.' or '(' or (wrongly) '['
                        Selector (FSys + Comma_RParent, X);
                      end if;
                      if X.TYP = Ints or
                         X.TYP = Floats or
                         X.TYP = xChars or
                         X.TYP = NOTYP
                      then
                        Emit1 (k_Read, Types'Pos (X.TYP));
                      else
                        Error (err_illegal_parameters_to_Put);
                      end if;
                    end if;
                  end if;
                end if;
                Test (Comma_RParent, FSys, err_incorrectly_used_symbol);
                exit when Sy /= Comma;
              end loop;
              <<SKIP1b>>
              Need (RParent, err_closing_parenthesis_missing);
            end if;
            if N = 2 then
              Emit (kGetNewline);
            end if;

          when 3 | 4 =>          -- PUT, PUT_LINE
            if Sy = LParent then
              InSymbol;
              I := Get_File_Pointer (Id);   -- Schoening
              if I = No_File_Index then
                Emit1 (k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              else -- First parameter is a file variable
                Emit1 (k_Set_current_file_pointer, I);
                InSymbol;
                if Sy /= Comma then
                  if Sy = RParent then
                    goto Label_21; -- skip the loop
                  else
                    Error (err_identifier_missing);
                  end if;
                end if;
              end if;
              loop
                if do_first_InSymbol then
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if Sy = StrCon then
                  Emit1 (k_Literal, SLeng);
                  Emit1 (k_Write_String, INum);
                  InSymbol;
                else
                  Expression (FSys + Colon_Comma_RParent, X);
                  if X.TYP = Enums then
                    X.TYP := Ints;
                  end if;
                  if (not Standard_Typ (X.TYP)) and X.TYP /= Strings then
                    Error (err_illegal_parameters_to_Put);
                  end if;
                  if Sy = Colon then
                    InSymbol;
                    Expression (FSys + Colon_Comma_RParent, Y);
                    if Y.TYP /= Ints then
                      Error (err_parameter_must_be_Integer);
                    end if;
                    if Sy = Colon then  --  ':' Pascal-ism (Write/WriteLn) !!
                      if X.TYP /= Floats then
                        Error (err_parameter_must_be_of_type_Float);
                      end if;
                      InSymbol;
                      Expression (FSys + Comma_RParent, Y);
                      if Y.TYP /= Ints then
                        Error (err_parameter_must_be_Integer);
                      end if;
                      Emit (k_Write_Float);
                    else
                      Emit1 (kWrite2, Types'Pos (X.TYP));
                    end if;
                  elsif X.TYP = Strings then
                    Emit1 (k_Write_String, X.Ref);
                  else
                    Emit1 (kWrite1, Types'Pos (X.TYP));
                  end if;
                end if;
                exit when Sy /= Comma;
              end loop;

              <<Label_21>>
              Need (RParent, err_closing_parenthesis_missing);
            end if;
            if N = 4 then
              Emit (kPutNewline);
            end if;

          when 5 | 6 =>                  -- Wait,SIGNAL
            if Sy /= LParent then
              Error (err_missing_an_opening_parenthesis);
            else
              InSymbol;
              if Sy /= IDent then
                Error (err_undefined_identifier);
              else
                I := Locate_Identifier (Id);
                InSymbol;
                if I /= 0 then
                  if IdTab (I).Obj /= Variable then
                    Error (err_variable_missing);
                  else
                    X.TYP := IdTab (I).TYP;
                    X.Ref := IdTab (I).Ref;
                    if IdTab (I).Normal then
                      F := k_Load_Address;
                    else
                      F := k_Push_Value;
                    end if;
                    Emit2 (F, IdTab (I).LEV, IdTab (I).Adr);
                    if Selector_Symbol_Loose (Sy) then  --  '.' or '(' or (wrongly) '['
                      Selector (FSys + RParent, X);
                    end if;
                    if X.TYP = Ints then
                      Emit (N + 1);    -- N is 5, or 6. Opcode is 6 or 7
                    else
                      Error (err_parameter_must_be_Integer);
                    end if;
                  end if;
                end if;
              end if;
              Need (RParent, err_closing_parenthesis_missing);
            end if;

          when 7 | 8 | 9 =>    -- reset, Rewrite, Close
            -- Schoening
            if Sy /= LParent then
              Error (err_missing_an_opening_parenthesis);
            else
              InSymbol;
              I := Get_File_Pointer (Id);
              if I = No_File_Index then
                Error (err_identifier_missing);
              else
                Emit2 (kFile_I_O, I, N);
              end if;
              InSymbol;
              Need (RParent, err_closing_parenthesis_missing);
            end if;  -- reset

          when 10 =>        -- CursorAt
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              begin
                InSymbol;
                Expression (Colon_Comma_LParent_RParent_Semicolon, X);
                if X.TYP /= Ints then
                  Skip (Semicolon, err_parameter_must_be_Integer);
                end if;
                if Sy /= Comma then
                  Skip (Semicolon, err_COMMA_missing);
                else
                  InSymbol;
                  Expression (Colon_Comma_LParent_RParent_Semicolon, X);
                  if X.TYP /= Ints then
                    Skip (Semicolon, err_parameter_must_be_Integer);
                  end if;
                  if Sy = Comma then
                    Skip (Semicolon, err_number_of_parameters_do_not_match);
                  elsif Sy /= RParent then
                    Skip (Semicolon, err_closing_parenthesis_missing);
                  else
                    Emit (k_Cursor_At);
                    InSymbol;
                  end if;
                end if;
              end;
            end if;                -- CursorAt

          when 11 =>                   -- Quantum
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (RParent_Set, X);
              if X.TYP /= Floats then
                Skip (Semicolon, err_parameter_must_be_of_type_Float);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (k_Set_Quantum_Task);
                InSymbol;
              end if;
            end if;                -- Quantum

          when 12 =>                   -- Set Priority
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (RParent_Set, X);
              if X.TYP /= Ints then
                Skip (Semicolon, err_parameter_must_be_Integer);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (k_Set_Task_Priority);
                InSymbol;
              end if;
            end if;                -- Priority
            --
          when 13 =>                   -- Set Priority Inheritance,INHERITP
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Boolean_Expression (RParent_Set, X);
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (k_Set_Task_Priority_Inheritance);
                InSymbol;
              end if;
            end if;                -- Inheritp
            --
          when others =>
            null;
        end case;
      end Standard_Procedure;

      procedure Block_Statement (block_name: Alfa) is  -- RM: 5.6
      begin
        Error (
          err_not_yet_implemented,
          hint => "Block statements don't work yet",
          stop_on_error => True
        );
        --
        Block (FSys, Is_a_function, True, Level + 1, T, block_name, block_name);  --  !! up/low case
        --
        -- !! to check:
        -- !! * stack management of variables when entering / quitting the block
        -- !! * object code and nesting... works on some cases at least (test.adb) !...
        -- !! Perhaps keep same level but have local declarations as for the
        --    variable in a FOR_Statement.
        -- !! Local bodies of subprograms surely mess the object code.
      end Block_Statement;

      procedure Named_Statement is
        --  Block_Statement or loop, named by "name: loop"
        new_label: constant Alfa := Id;
      begin
        Enter (new_label, Id_with_case, Label);
        Test (Colon_Set, FSys,
          err_colon_missing_for_named_statement,
          stop_on_error => True
        );
        InSymbol;
        case Sy is
          when BEGIN_Symbol | DECLARE_Symbol => -- Named Block_Statement
            Block_Statement (new_label);
          when LOOP_Symbol | FOR_Symbol | WHILE_Symbol =>
            null; -- !! should check label after end loop
          when others =>
            null;
        end case;
      end Named_Statement;

    begin  --  Statement
      if Err_Count > 0 then
        return;
      end if;

      --{ Mark the following opcodes as belonging to LineCount # }
      Emit1 (kHighlightSource, Line_Count);  --{MRC: this line is not in PC version}
      --{ This did not work because the LineCount was off by one. Why? }
      --{ MRC: This line is needed in order to highlight lines in task windows
      --}

      if Statement_Begin_Symbol (Sy) then
        case Sy is
          when IDent =>
            I := Locate_Identifier (Id, No_Id_Fail => False);
            InSymbol;
            if I = No_Id then
              --  New identifier: must be a label for named Block_Statement or loop.
              Named_Statement;
            else
              case IdTab (I).Obj is
                when Declared_Number =>
                  if Sy = Becomes then
                    Error (err_illegal_statement_start_symbol, ": cannot modify a constant");
                  else
                    Error (err_illegal_statement_start_symbol);
                  end if;
                  Assignment (I);
                when TypeMark | Funktion =>
                  Error (err_illegal_statement_start_symbol, ": statement cannot start with function or type name");
                  Assignment (I);
                when Variable =>
                  Assignment (I);
                when aTask =>
                  Entry_Call (FSys, I, CallSTDE);
                when Prozedure =>
                  if IdTab (I).LEV = 0 then
                    Standard_Procedure (IdTab (I).Adr);
                  else
                    Call (FSys, I, CallSTDP);
                  end if;
                when Label =>
                  Error (err_duplicate_label, Alfa_to_String(Id));
                  Test (Colon_Set, FSys, err_colon_missing);
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
            LOOP_Statement (k_Jump, LC);
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
        if not EofInput then      --{MRC: added IF NOT... from PC version}
          Need (Semicolon, err_semicolon_missing);
        end if;
      end if;  --  Sy in Statement_Begin_Symbol
      --
      if not EofInput then
        Test (FSys - Semicolon, Semicolon_Set, err_incorrectly_used_symbol);
      end if;
    end Statement;

    procedure Declarative_Part is
    begin
      loop
        Test (  --  Added 17-Apr-2018 to avoid infinite loop on erroneous code
          Declaration_Symbol + BEGIN_Symbol,
          Empty_Symset,
          err_incorrectly_used_symbol,
          stop_on_error => True  --  Exception is raised there if there is an error.
        );
        if Sy = IDent then
          Var_Declaration;
        end if;
        if Sy = TYPE_Symbol then
          Type_Declaration;
        end if;
        if Sy = TASK_Symbol then
          Task_Declaration;
        end if;
        BlockTab (PRB).VSize := Dx;
        --  ^ TBD: check if this is still correct for declarations that appear
        --    after subprograms !!
        while Sy = PROCEDURE_Symbol or Sy = FUNCTION_Symbol loop
          Proc_Func_Declaration;
        end loop;
        --
        exit when Sy = BEGIN_Symbol;
      end loop;
    end Declarative_Part;

    procedure Statements_Part_Setup is
    begin
      MaxDX           := Dx;
      IdTab (Prt).Adr := LC;
      --  Copy initialization (elaboration) ObjCode from end of ObjCode table
      I := CMax + ICode;
      while I > CMax loop
        ObjCode (LC) := ObjCode (I);
        LC           := LC + 1;
        I            := I - 1;
      end loop;
      CMax := CMax + ICode;  -- Restore CMax to the initial max (=CDMax)
    end Statements_Part_Setup;

    procedure Statements_List is
    begin
      if Sy = END_Symbol then  --  GdM 15-Aug-2014: there should be at least one statement.
        Error (err_statement_expected);
      end if;
      loop
        Statement (FSys + END_Symbol);
        exit when Sy = END_Symbol or Err_Count > 0;
      end loop;
    end Statements_List;

    procedure Statements_Part_Closing is
    begin
      BlockTab (PRB).SrcTo := Line_Count;
    end Statements_Part_Closing;

  begin  --  Block
    if Err_Count > 0 then --{MRC, from PC source}
      return;
    end if;
    Dx    := 5;
    ICode := 0;
    if Level > LMax then
      Fatal (LEVELS);  --  Exception is raised there.
    end if;
    if Is_a_block_statement then
      null;  --  We should be here with Sy = BEGIN_Symbol or Sy = DECLARE_Symbol.
    else
      Test (Symbols_after_Subprogram_Identifier, FSys, err_incorrectly_used_symbol);
    end if;
    if IdTab (Prt).Ref > 0 then
      PRB := IdTab (Prt).Ref;
    else
      Enter_Block (Prt);
      PRB             := B;
      IdTab (Prt).Ref := PRB;
    end if;
    Display (Level) := PRB;
    IdTab (Prt).TYP := NOTYP;
    if Sy = LParent and Level > 1 then
      Formal_Parameter_List;
    end if;
    --
    if Err_Count > 0 then
      return;
    end if;
    --
    BlockTab (PRB).LastPar := T;
    BlockTab (PRB).PSize   := Dx;
    if Is_a_function and not Is_a_block_statement then
      if Sy = RETURN_Symbol then
        InSymbol;  --  FUNCTION TYPE
        if Sy = IDent then
          I := Locate_Identifier (Id);
          InSymbol;
          if I /= 0 then
            if IdTab (I).Obj /= TypeMark then
              Error (err_missing_a_type_identifier);
              return;  --{MRC, from PC source}
            elsif Standard_Typ (IdTab (I).TYP) then
              IdTab (Prt).TYP := IdTab (I).TYP;
            else
              Error (err_bad_result_type_for_a_function);
              return;    --{MRC, from PC source}
            end if;
          end if;
        else
          Error (err_identifier_missing, stop_on_error => True);
        end if;
      else
        Error (err_RETURN_missing, stop_on_error => True);
      end if;
    end if;
    --
    if Sy = Semicolon then  -- end of specification part
      BlockTab (PRB).VSize := Dx;
      IdTab (Prt).Adr      := -1;    -- address of body TBD
      return;
    end if;
    --
    if Is_a_block_statement then
      case Sy is
        when DECLARE_Symbol => InSymbol;
        when BEGIN_Symbol   => null;
        when others         => raise Internal_error with "Unexpected " & KeyWSymbol'Image(Sy);
      end case;
    elsif Sy = IS_Symbol then  --  The "IS" in "procedure ABC (param : T_Type) IS"
      InSymbol;
    else
      Error (err_IS_missing);
      return;
    end if;
    --
    if Sy = NULL_Symbol and not Is_a_block_statement then
      --  RM 6.7 Null Procedures (Ada 2005)
      --  E.g.: "procedure Not_Yet_Done (a : Integer) is null;"
      InSymbol;  --  Consume NULL symbol.
      Statements_Part_Setup;
      if Is_a_function then
        Error (err_no_null_functions);  --  There are no null functions: what would be the result?
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
      if Sy = END_Symbol then
        InSymbol;
      elsif Err_Count > 0 then
        return;  --  At this point the program is already FUBAR.
      else
        Error (err_END_missing);
        return;
      end if;
      --
      if Sy = IDent then  --  Verify that the name after "end" matches the unit name.
        if Id /= Block_ID then
          Error (err_incorrect_block_name, hint => Alfa_to_String(Block_ID_with_case));
        end if;
        InSymbol;
      elsif Is_a_block_statement and Block_ID /= Empty_Alfa then  --  "end [label]" is required
        Error (err_incorrect_block_name, hint => Alfa_to_String(Block_ID_with_case));
      end if;
    end if;
    --
    if Sy /= Semicolon then
      Error (err_semicolon_missing);
      return;
    end if;
    --
    if Block_ID /= Main_Program_ID and not Is_a_block_statement then
      InSymbol;  --  Consume ';' symbol after END [Subprogram_Id].
      --
      --  Now we have either another declaration,
      --  or BEGIN or, if it's a package, END.
      Test (
        FSys + Declaration_Symbol + BEGIN_Symbol + END_Symbol,
        Empty_Symset,
        err_incorrectly_used_symbol
      );
    end if;
  end Block;

end HAC.Parser;
