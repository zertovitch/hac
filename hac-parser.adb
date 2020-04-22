with HAC.Parser.Helpers;     use HAC.Parser.Helpers;
with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

package body HAC.Parser is

  use HAC.Compiler, HAC.Data;

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block (
    CD                   : in out HAC.Compiler.Compiler_Data;
    FSys                 :        HAC.Data.Symset;
    Is_a_function        :        Boolean;        --  RETURN [Value] statement expected
    Is_a_block_statement :        Boolean;        --  5.6 Block Statements
    Level_A              :        Integer;
    Prt                  :        Integer;
    Block_ID             :        HAC.Data.Alfa;  --  Name of this block (if any)
    Block_ID_with_case   :        HAC.Data.Alfa
  )
  is
    Level : Integer := Level_A;
    procedure InSymbol is begin InSymbol (CD); end;

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
    ICode   : Integer;  -- Size of initialization ObjCode generated

    ------------------------------------------------------------------
    -------------------------------------------------------EnterArray-

    procedure Enter_Array (TP : Types; L, H : Integer) is
      Lz, Hz : Integer;
    begin
      if L > H then
        Error (CD, err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)"); -- !!
      end if;
      Lz := L;
      Hz := H;
      if abs (L) > XMax or abs (H) > XMax then
        Error (CD, err_illegal_array_bounds, "absolute value of a bound exceeds maximum value");
        Lz := 0;
        Hz := 0;
      end if;
      if CD.Arrays_Count = AMax then
        Fatal (ARRAYS);  --  Exception is raised there.
      end if;
      CD.Arrays_Count := CD.Arrays_Count + 1;
      declare
        New_A : ATabEntry renames CD.Arrays_Table (CD.Arrays_Count);
      begin
        New_A.Index_TYP := TP;
        New_A.Low       := Lz;
        New_A.High      := Hz;
      end;
    end Enter_Array;

    ------------------------------------------------------------------
    ------------------------------------------------------Enter_Block-
    procedure Enter_Block (Tptr : Integer) is
    begin
      if CD.Blocks_Count = BMax then
        Fatal (PROCEDURES);  --  Exception is raised there.
      end if;
      CD.Blocks_Count := CD.Blocks_Count + 1;
      declare
        New_B : BTabEntry renames CD.Blocks_Table (CD.Blocks_Count);
      begin
        New_B.Id      := CD.IdTab (Tptr).Name;
        New_B.Last    := 0;
        New_B.LastPar := 0;
        New_B.SrcFrom := CD.Line_Count;
      end;
    end Enter_Block;

    ------------------------------------------------------------------
    ----------------------------------------------Enter_or_find_Float-
    procedure Enter_or_find_Float (X : HAC_Float) is
    begin
      if CD.Float_Constants_Count = C2Max - 1 then
        Fatal (FLOAT_CONSTANTS);  --  Exception is raised there.
      end if;
      CD.Float_Constants_Table (CD.Float_Constants_Count + 1) := X;  --  We add X's value as an extra item.
      CD.RNum_Index  := 1;
      while CD.Float_Constants_Table (CD.RNum_Index) /= X loop
        CD.RNum_Index := CD.RNum_Index + 1;
      end loop;
      if CD.RNum_Index > CD.Float_Constants_Count then  --  X's value was not previously in the table.
        CD.Float_Constants_Count := CD.RNum_Index;
      end if;
    end Enter_or_find_Float;

    ------------------------------------------------------------------
    ------------------------------------------------------------Enter-
    procedure Enter (Id, Id_with_case : Alfa; K : aObject) is
      J, L : Integer;
    begin
      if CD.Id_Count = Id_Table_Max then
        Fatal (IDENTIFIERS);  --  Exception is raised there.
      end if;
      CD.IdTab (No_Id).Name := Id;  --  Sentinel
      J                     := CD.Blocks_Table (CD.Display (Level)).Last;
      L                     := J;
      while CD.IdTab (J).Name /= Id loop
        J := CD.IdTab (J).Link;
      end loop;
      --  Follow the chain of identifiers for current Level.
      if J /= No_Id then
        Error (CD, err_duplicate_identifier, Id);
      else      --  Enter identifier in table IdTab
        CD.Id_Count            := CD.Id_Count + 1;
        CD.IdTab (CD.Id_Count) :=
         (Name           => Id,
          Name_with_case => Id_with_case,
          Link           => L,
          Obj            => K,
          Read_only      => False,
          TYP            => NOTYP,
          Ref            => 0,
          Normal         => True,
          LEV            => Level,
          Adr            => 0
        );
        CD.Blocks_Table (CD.Display (Level)).Last := CD.Id_Count;  --  Update start of identifier chain
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
      L                    := Level;
      CD.IdTab (No_Id).Name := Id;  --  Sentinel
      loop
        J := CD.Blocks_Table (CD.Display (L)).Last;
        while CD.IdTab (J).Name /= Id loop  --  Scan all Id's on level L.
          J := CD.IdTab (J).Link;
        end loop;
        L := L - 1;
        exit when L < 0 or J /= No_Id;
      end loop;
      if J = No_Id and No_Id_Fail then
        Error (CD, err_undefined_identifier, stop_on_error => stop_on_error);
      end if;
      return J;
    end Locate_Identifier;

    ------------------------------------------------------------------
    -------------------------------------------------Get_File_Pointer-
    function Get_File_Pointer (Id : Alfa) return Integer is    -- Schoening
    begin   -- locate Id in FileIOTab
      for I in 1 .. CD.File_IO_Table.Kount loop
        if CD.File_IO_Table.Nam (I) (2) = ':' then
          if CD.File_IO_Table.Nam (I) (3 .. CD.File_IO_Table.LNam (I) - 2) =
             Id (1 .. CD.File_IO_Table.LNam (I) - 2)
          then
            return I;
          end if;
        elsif CD.File_IO_Table.Nam (I) = Id (1 .. CD.File_IO_Table.LNam (I)) then
          return I;
        end if;
      end loop;
      return No_File_Index;
    end Get_File_Pointer;

    ------------------------------------------------------------------
    ---------------------------------------------------Enter_Variable-
    procedure Enter_Variable is
    begin
      if CD.Sy = IDent then
        Enter (CD.Id, CD.Id_with_case, Variable);
        InSymbol;
      else
        Error (CD, err_identifier_missing);
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
      Test (CD, Constant_Definition_Begin_Symbol, FSys, err_illegal_symbol_for_a_number_declaration);
      if not Constant_Definition_Begin_Symbol (CD.Sy) then
        return;
      end if;
      if CD.Sy = CharCon then  --  Untyped character constant, occurs only in ranges.
        C.TP := xChars;
        C.I  := CD.INum;
        InSymbol;
      else
        Sign := 1;
        if Plus_Minus (CD.Sy) then
          if CD.Sy = Minus then
            Sign := -1;
          end if;
          InSymbol;
        end if;
        if CD.Sy = IDent then  --  Number defined using another one: "minus_pi : constant := -pi;"
          X := Locate_Identifier (CD.Id);
          if X /= 0 then
            if CD.IdTab (X).Obj /= Declared_Number_or_Enum_Item then
              Error (CD, err_illegal_constant_or_constant_identifier);
            else
              C.TP := CD.IdTab (X).TYP;
              if C.TP = Floats then
                C.R := HAC_Float (Sign) * CD.Float_Constants_Table (CD.IdTab (X).Adr);
              else
                C.I := Sign * CD.IdTab (X).Adr;
              end if;
            end if;
          end if;  --  X /= 0
          InSymbol;
        elsif CD.Sy = IntCon then
          C.TP := Ints;
          C.I  := Sign * CD.INum;
          InSymbol;
        elsif CD.Sy = FloatCon then
          C.TP := Floats;
          C.R  := HAC_Float (Sign) * CD.RNum;
          InSymbol;
        else
          Skip (CD, FSys, err_illegal_symbol_for_a_number_declaration);
        end if;
      end if;
      Test (CD, FSys, Empty_Symset, err_incorrectly_used_symbol);
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
          Error (CD, err_illegal_array_bounds, "a float type is not expected for a bound");
          Low.TP := Ints;
          Low.I  := 0;
        end if;
        Need (CD, Range_Double_Dot_Symbol, err_expecting_double_dot);
        --
        Number_Declaration (Comma_OF_RParent + FSys, High);
        --
        if High.TP /= Low.TP then
          Error (CD, err_illegal_array_bounds, "bound types do not match");
          High.I := Low.I;
        end if;
        Enter_Array (Low.TP, Low.I, High.I);
        ARef := CD.Arrays_Count;
        if StrAr then
          ELTP := xChars;
          ELRF := 0;
          ELSZ := 1;
          Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
        elsif CD.Sy = Comma then  --  Multidimensional array is array(range_1) of array(range_2,...)
          InSymbol;
          ELTP := Arrays;
          Array_Typ (ELRF, ELSZ, StrAr);  --  Recursion for next array dimension.
        else
          Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
          Need (CD, OF_Symbol, err_missing_OF);
          TYP (FSys, ELTP, ELRF, ELSZ);
        end if;
        Arsz := (High.I - Low.I + 1) * ELSZ;
        declare
          r : ATabEntry renames CD.Arrays_Table (ARef);
        begin
          r.Size        := Arsz;  --  NB: Index_TYP, Low, High already set
          r.Element_TYP := ELTP;
          r.ELREF       := ELRF;
          r.ELSize      := ELSZ;
        end;
      end Array_Typ;

      procedure Enumeration_Type is  --  RM 3.5.1 Enumeration Types
        enum_count : Natural := 0;
      begin
        TP := Enums;
        RF := CD.Id_Count;
        loop
          InSymbol;  --  Consume '(' symbol.
          if CD.Sy = IDent then
            enum_count := enum_count + 1;
            Enter (CD.Id, CD.Id_with_case, Declared_Number_or_Enum_Item);
            declare
              New_Enum : IdTabEntry renames CD.IdTab (CD.Id_Count);
            begin
              New_Enum.Read_only := True;
              New_Enum.TYP       := Enums;
              New_Enum.Ref       := RF;
              New_Enum.Adr       := enum_count - 1;  --  RM 3.5.1 (7): position begins with 0.
            end;
          else
            Error (CD, err_identifier_missing);
          end if;
          InSymbol;
          exit when CD.Sy /= Comma;
        end loop;
        Sz := enum_count;
        --  Huh ?? size should be 1 (to be checked !!) but Sz is perhaps misused as a count
        Need (CD, RParent, err_closing_parenthesis_missing);
      end Enumeration_Type;

      procedure Record_Type is
      begin
        InSymbol;  --  Consume RECORD symbol.
        Enter_Block (CD.Id_Count);
        TP := Records;
        RF := CD.Blocks_Count;
        if Level = LMax then
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
            Enter_Variable;
            while CD.Sy = Comma loop  --  ','  in  "a, b, c : Integer;"
              InSymbol;
              Enter_Variable;
            end loop;
            Need (CD, Colon, err_colon_missing);  --  ':'  in  "a, b, c : Integer;"
            T1 := CD.Id_Count;
            TYP (FSys + Comma_END_IDent_Semicolon, ELTP, ELRF, ELSZ);
            while T0 < T1 loop
              T0                := T0 + 1;
              CD.IdTab (T0).TYP := ELTP;
              CD.IdTab (T0).Ref := ELRF;
              CD.IdTab (T0).Adr := Offset;
              Offset            := Offset + ELSZ;
            end loop;
          end if;
          Need (CD, Semicolon, err_semicolon_missing, Forgive => Comma);
          Ignore_Extra_Semicolons (CD);
          exit when CD.Sy = END_Symbol;
        end loop;
        --
        CD.Blocks_Table (RF).VSize := Offset;
        Sz                  := Offset;
        CD.Blocks_Table (RF).PSize := 0;
        InSymbol;
        Need (CD, RECORD_Symbol, err_RECORD_missing);  --  (END) RECORD
        Level := Level - 1;
      end Record_Type;

      I : Integer;

    begin  --  Type
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      Test (CD, Type_Begin_Symbol, FSys, err_missing_ARRAY_RECORD_or_ident);
      if Type_Begin_Symbol (CD.Sy) then
        if CD.Id(1..10) = "STRING    " then -- !! Need to implement constraints...
          CD.Sy := String_Symbol;
        end if;
        case CD.Sy is
          when IDent =>
            I := Locate_Identifier (CD.Id);
            if I /= 0 then
              if CD.IdTab (I).Obj = TypeMark then
                TP := CD.IdTab (I).TYP;
                RF := CD.IdTab (I).Ref;
                Sz := CD.IdTab (I).Adr;
                if TP = NOTYP then
                  Error (CD, err_undefined_type);
                end if;
              else
                Error (CD, err_missing_a_type_identifier);
              end if;
            end if;
            InSymbol;

          when ARRAY_Symbol | String_Symbol =>
            StrArray := CD.Sy = String_Symbol;
            InSymbol;
            Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
            TP := Arrays;
            Array_Typ (RF, Sz, StrArray);

          when RECORD_Symbol =>
            Record_Type;
          when LParent =>
            Enumeration_Type;
          when others =>
            null;
        end case;  --  CD.Sy
        Test (CD, FSys, Empty_Symset, err_incorrectly_used_symbol);
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
      Test (CD, IDent_Set, FSys + RParent, err_identifier_missing, stop_on_error => True);
      while CD.Sy = IDent loop
        T0 := CD.Id_Count;
        Enter_Variable;
        while CD.Sy = Comma loop
          InSymbol;
          Enter_Variable;
        end loop;
        if CD.Sy = Colon then  --  ':'  in  "function F (x,y : in Real) return Real;"
          InSymbol;
          if CD.Sy = IN_Symbol then
            InSymbol;
          end if;
          if Is_a_function then -- if I am a function, no InOut parms allowed
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
            X := Locate_Identifier (CD.Id);
            InSymbol;
            if X /= No_Id then
              if CD.IdTab (X).Obj = TypeMark then
                TP := CD.IdTab (X).TYP;
                RF := CD.IdTab (X).Ref;
                if ValParam then
                  Sz := CD.IdTab (X).Adr;
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
              r.TYP       := TP;
              r.Ref       := RF;
              r.Normal    := ValParam;
              r.Read_only := ValParam;
              r.Adr       := Dx;
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
      TP         : Types;
      RF, Sz, T1 : Integer;
    begin
      InSymbol;
      Test (CD, IDent_Set, Semicolon_Set, err_identifier_missing);
      Enter (CD.Id, CD.Id_with_case, TypeMark);
      T1 := CD.Id_Count;
      InSymbol;
      Need (CD, IS_Symbol, err_IS_missing);
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      TYP (Comma_IDent_Semicolon + FSys, TP, RF, Sz);
      CD.IdTab (T1).TYP := TP;
      CD.IdTab (T1).Ref := RF;
      CD.IdTab (T1).Adr := Sz;
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
        T0, T1, RF, Sz, T0i, LC0, LC1 : Integer;
        TP                            : Types;
        is_constant, is_typed,
        is_untyped_constant           : Boolean;
        C                             : ConRec;
        I_dummy                       : Integer;
      begin
        T0 := CD.Id_Count;
        Enter_Variable;
        while CD.Sy = Comma loop
          InSymbol;
          Enter_Variable;
        end loop;
        Need (CD, Colon, err_colon_missing);  --  ':'   in   "x, y : Integer;"
        T1 := CD.Id_Count;
        --
        if CD.Sy = IDent then
          I_dummy := Locate_Identifier (CD.Id, stop_on_error => True);
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
          TYP (Becomes_Comma_IDent_Semicolon + FSys, TP, RF, Sz);
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
            Number_Declaration (Comma_IDent_Semicolon + FSys, C);
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
                r.TYP := C.TP;
                case C.TP is
                  when Floats =>
                    Enter_or_find_Float (C.R);
                    r.Adr := CD.RNum_Index;
                  when Ints =>
                    r.Adr := C.I;
                  when others =>
                    Error (CD, err_numeric_constant_expected);
                    --  "boo : constant := True;" or "x: constant := 'a';"
                    --  are wrong in Ada.
                    r.Adr := C.I;
                end case;
              else  --  A variable or a typed constant
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
            Emit2 (CD, k_Load_Address, CD.IdTab (T0).LEV, CD.IdTab (T0).Adr);
            Emit2 (CD, k_Push_Value,   CD.IdTab (T1).LEV, CD.IdTab (T1).Adr);
            Emit (CD, k_Store);
          end loop;
          --
          LC1 := CD.LC;
          --  reset ObjCode pointer as if ObjCode had not been generated
          CD.LC := LC0;
          --  copy ObjCode to end of ObjCode table in reverse order
          ICode := ICode + (LC1 - LC0);  --  Size of initialization ObjCode
          while LC0 < LC1 loop
            CD.ObjCode (CMax) := CD.ObjCode (LC0);
            CMax              := CMax - 1;
            LC0               := LC0 + 1;
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
          Enter (CD.Id, CD.Id_with_case, Funktion);
        else
          Enter (CD.Id, CD.Id_with_case, Prozedure);
        end if;
        InSymbol;
        Block (CD, FSys, IsFun, False, Level + 1, CD.Id_Count,
               CD.IdTab (CD.Id_Count).Name, Id_subprog_with_case);
      end;
      if IsFun then
        Emit1 (CD, k_Exit_Function, 1);
      else
        Emit1 (CD, k_Exit_Call, CallSTDP);
      end if;
    end Proc_Func_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------Task_Declaration-
    procedure Task_Declaration is          -- Hathorn
      I, T0         : Integer;
      TaskID        : Alfa;
      saveLineCount : constant Integer := CD.Line_Count;  --  Source line where Task appeared
    begin
      InSymbol;
      if CD.Sy = BODY_Symbol then  --  Task Body
        InSymbol;
        I      := Locate_Identifier (CD.Id);
        TaskID := CD.IdTab (I).Name;
        CD.Blocks_Table (CD.IdTab (I).Ref).SrcFrom := saveLineCount;  --(* Manuel *)
        InSymbol;
        Block (CD, FSys, False, False, Level + 1, I, TaskID, TaskID);  --  !! up/low case
        Emit1 (CD, k_Exit_Call, CallSTDP);
      else                         --  Task Specification
        if CD.Sy = IDent then
          TaskID := CD.Id;
        else
          Error (CD, err_identifier_missing);
          CD.Id := Empty_Alfa;
        end if;
        CD.Tasks_Definitions_Count := CD.Tasks_Definitions_Count + 1;
        if CD.Tasks_Definitions_Count > TaskMax then
          Fatal (TASKS);  --  Exception is raised there.
        end if;
        Enter (TaskID, TaskID, aTask);  --  !! casing
        CD.Tasks_Definitions_Table (CD.Tasks_Definitions_Count) := CD.Id_Count;
        Enter_Block (CD.Id_Count);
        CD.IdTab (CD.Id_Count).Ref := CD.Blocks_Count;
        InSymbol;
        if CD.Sy = Semicolon then
          InSymbol;  --  Task with no entries
        else  --  Parsing the Entry specs
          Need (CD, IS_Symbol, err_IS_missing);
          if Level = LMax then
            Fatal (LEVELS);  --  Exception is raised there.
          end if;
          Level              := Level + 1;
          CD.Display (Level) := CD.Blocks_Count;
          while CD.Sy = ENTRY_Symbol loop
            InSymbol;
            if CD.Sy /= IDent then
              Error (CD, err_identifier_missing);
              CD.Id := Empty_Alfa;
            end if;
            CD.Entries_Count := CD.Entries_Count + 1;
            if CD.Entries_Count > EntryMax then
              Fatal (ENTRIES);  --  Exception is raised there.
            end if;
            Enter (CD.Id, CD.Id_with_case, aEntry);
            CD.Entries_Table (CD.Entries_Count) := CD.Id_Count;  --  point to identifier table location
            T0                                  := CD.Id_Count;  --  of TaskID
            InSymbol;
            Block (CD, FSys, False, False, Level + 1, CD.Id_Count,
                   CD.IdTab (CD.Id_Count).Name, CD.IdTab (CD.Id_Count).Name_with_case);
            CD.IdTab (T0).Adr := CD.Tasks_Definitions_Count;
            if CD.Sy = Semicolon then
              InSymbol;
            else
              Error (CD, err_semicolon_missing);
            end if;
          end loop;  --  while CD.Sy = ENTRY_Symbol

          Level := Level - 1;
          Test_END_Symbol (CD);
          if CD.Sy = IDent and CD.Id = TaskID then
            InSymbol;
          else
            Skip (CD, Semicolon, err_incorrect_block_name);
          end if;
          Test_Semicolon (CD, FSys);
        end if;
      end if;
    end Task_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Expression-
    procedure Expression (FSys : Symset; X : in out Item);

    ------------------------------------------------------------------
    ---------------------------------------------------------Selector-
    procedure Selector (FSys : Symset; V : in out Item) is
      --
      procedure Record_Field_Selector is
        Field_Offset, Field_Id : Integer;
      begin
        if V.TYP = Records then
          Field_Id       := CD.Blocks_Table (V.Ref).Last;
          CD.IdTab (0).Name := CD.Id;
          while CD.IdTab (Field_Id).Name /= CD.Id loop  --  Search field identifier
            Field_Id := CD.IdTab (Field_Id).Link;
          end loop;
          if Field_Id = No_Id then
            Error (CD, err_undefined_identifier, stop_on_error => True);
          end if;
          V.TYP        := CD.IdTab (Field_Id).TYP;
          V.Ref        := CD.IdTab (Field_Id).Ref;
          Field_Offset := CD.IdTab (Field_Id).Adr;
          if Field_Offset /= 0 then
            Emit1 (CD, k_Record_Field_Offset, Field_Offset);
          end if;
        else
          Error (CD, err_var_with_field_selector_must_be_record);
        end if;
        InSymbol;
      end Record_Field_Selector;
      --
      procedure Array_Coordinates_Selector is
        Array_Index_Expr : Item;  --  Evaluation of "i", "j+7", "k*2" in "a (i, j+7, k*2)".
        AT_Index : Integer;       --  Index in the table of all arrays definitions.
      begin
        loop
          InSymbol;  --  Consume '(' or ',' symbol.
          Expression (FSys + Comma_RParent + RBrack, Array_Index_Expr);
          if V.TYP = Arrays then
            AT_Index := V.Ref;
            if CD.Arrays_Table (AT_Index).Index_TYP /= Array_Index_Expr.TYP then
              Error (CD, err_illegal_array_subscript);
            elsif CD.Arrays_Table (AT_Index).ELSize = 1 then
              Emit1 (CD, k_Array_Index_Element_Size_1, AT_Index);
            else
              Emit1 (CD, k_Array_Index, AT_Index);
            end if;
            V.TYP := CD.Arrays_Table (AT_Index).Element_TYP;
            V.Ref := CD.Arrays_Table (AT_Index).ELREF;
          else
            Error (CD, err_indexed_variable_must_be_an_array);
          end if;
          exit when CD.Sy /= Comma;
        end loop;
      end Array_Coordinates_Selector;
      --
      err : Compile_Error;
    begin
      pragma Assert (Selector_Symbol_Loose (CD.Sy));  --  '.' or '(' or (wrongly) '['
      loop
        if CD.Sy = Period then
          InSymbol;  --  Consume '.' symbol.
          if CD.Sy = IDent then
            Record_Field_Selector;
          else
            Error (CD, err_identifier_missing);
          end if;
        else
          if CD.Sy = LBrack then  --  '['
            --  Common mistake by Pascal, Python or R programmers.
            Error (CD, err_left_bracket_instead_of_parenthesis);
          end if;
          Array_Coordinates_Selector;
          if CD.Sy = RBrack then  --  ']' : same kind of mistake as for '[' ...
            Error (CD, err_right_bracket_instead_of_parenthesis);
            InSymbol;
          else
            Need (CD, RParent, err_closing_parenthesis_missing);
          end if;
        end if;
        exit when not Selector_Symbol_Loose (CD.Sy);
      end loop;
      --
      if FSys = Semicolon_Set then
        err := err_semicolon_missing;
      else
        err := err_incorrectly_used_symbol;
      end if;
      Test (CD, FSys, Empty_Symset, err);
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
      Emit1 (CD, k_Mark_Stack, I);
      LastP := CD.Blocks_Table (CD.IdTab (I).Ref).LastPar;
      CP    := I;
      if CD.Sy = LParent then  --  Actual parameter list
        loop
          InSymbol;
          if CP >= LastP then
            Error (CD, err_number_of_parameters_do_not_match);
          else
            CP := CP + 1;
            if CD.IdTab (CP).Normal then           --  Value parameter (IN)
              Expression (FSys + Colon_Comma_RParent, X);
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
                K := Locate_Identifier (CD.Id);
                InSymbol;
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
                    Selector (FSys + Colon_Comma_RParent, X);
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
      if CP < LastP then  --  Too few actual parameters
        Error (CD, err_number_of_parameters_do_not_match);
      end if;

      if CallType = CallSTDP then
        Emit2 (CD, k_Call, CallType, CD.Blocks_Table (CD.IdTab (I).Ref).PSize - 1);
      else
        Emit2 (CD, k_Call, CallType, CD.Blocks_Table (CD.IdTab (I).Ref).PSize - 1);
        Emit1 (CD, k_Exit_Call, CallType);  --  Return from Entry Call
      end if;

      if CD.IdTab (I).LEV < Level then
        Emit2 (CD, k_Update_Display_Vector, CD.IdTab (I).LEV, Level);
      end if;
    end Call;

    ------------------------------------------------------------------
    -------------------------------------------------------Entry_Call-
    procedure Entry_Call (FSys : Symset; I, CallType : Integer) is -- Hathorn
      Addr, J : Integer;
    begin
      if CD.Sy /= Period then
        Skip (CD, Semicolon, err_incorrectly_used_symbol);
      else
        InSymbol;                  --  Task Entry Selector
        if CD.Sy /= IDent then
          Skip (CD, Semicolon, err_identifier_missing);
        else
          J                 := CD.Blocks_Table (CD.IdTab (I).Ref).Last;
          CD.IdTab (0).Name := CD.Id;
          while CD.IdTab (J).Name /= CD.Id loop
            J := CD.IdTab (J).Link;
          end loop;

          if J = 0 then
            Error (CD, err_undefined_identifier);
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

      procedure Simple_Expression (FSys : Symset; X : in out Item) is
        Y  : Item;
        OP : KeyWSymbol;

        procedure Term (FSys : Symset; X : in out Item) is
          Y  : Item;
          OP : KeyWSymbol;

          procedure Factor (FSys : Symset; X : in out Item) is
            I : Integer;
            F : Opcode;
            err  : Compile_Error;

            procedure Standard_Function (SF_Code : Integer) is
              T_Argument : Typ_Set;  --  Expected type of the function's argument
              N  : Integer := SF_Code;
            begin  --  STANDARD FUNCTION NO. N , N >= SF_Clock INDICATES
                   --  a NILADIC FUNCTION.
              if N < SF_Clock then
                Need (CD, LParent, err_missing_an_opening_parenthesis);
                if N < SF_EOF or N > SF_EOLN then
                  Expression (FSys + RParent, X);
                  case N is
                    when SF_Abs =>  --  Abs (NB: in Ada it's an operator, not a function)
                      T_Argument       := Numeric_Typ_Set;
                      CD.IdTab (I).TYP := X.TYP;
                      if X.TYP = Floats then
                        N := N + 1;
                      end if;
                    when SF_T_Val =>  --  S'Val : RM 3.5.5 (5)
                      T_Argument := Ints_Typ;
                    when SF_T_Pos =>  --  S'Pos : RM 3.5.5 (2)
                      T_Argument := Discrete_Typ;
                    when SF_T_Succ | SF_T_Pred =>  -- S'Succ, S'Pred : RM 3.5 (22, 25)
                      T_Argument := Discrete_Typ;
                      CD.IdTab (I).TYP := X.TYP;
                    when SF_Round_Float_to_Int | SF_Trunc_Float_to_Int |
                         SF_Sin | SF_Cos | SF_Exp | SF_Log | SF_Sqrt | SF_Arctan
                      =>
                      T_Argument := Numeric_Typ_Set;
                      if Ints_Typ (X.TYP) then
                        Forbid_Type_Coercion (CD, "value is of integer type; floating-point is expected here");
                        Emit1 (CD, k_Integer_to_Float, 0);
                      end if;
                    --  Random
                    when SF_Random =>
                      T_Argument := Ints_Typ;
                      CD.IdTab (I).TYP := X.TYP;
                    when others =>
                      null;
                  end case;  --  N
                  --
                  if T_Argument (X.TYP) then
                    Emit1 (CD, k_Standard_Functions, N);
                  elsif X.TYP /= NOTYP then
                    Error (CD, err_argument_to_std_function_of_wrong_type);
                  end if;
                else           --  N in [EOF, EOLN]
                  if CD.Sy /= IDent then
                    Error (CD, err_identifier_missing);
                  elsif CD.Id(1..10) = "INPUT     " then  --  Standard_Input
                    Emit2 (CD, k_Standard_Functions, 0, N);
                  else
                    I := Get_File_Pointer (CD.Id);
                    if I = No_File_Index then -- NB: bug: was 0 instead of -1...
                      Error (CD, err_undefined_identifier);
                    else
                      Emit2 (CD, k_Standard_Functions, I, N);
                    end if;
                  end if;
                  InSymbol;
                end if;        --  N in [EOF, EOLN]
                X.TYP := CD.IdTab (I).TYP;
                Need (CD, RParent, err_closing_parenthesis_missing);
              else             --  NILADIC FUNCTION
                case N is
                  when SF_Clock =>
                    Emit1 (CD, k_Standard_Functions, N);
                  when others =>
                    null;
                end case;
              end if;    -- NILADIC FUNCTIONS, N >= SF_Clock
            end Standard_Function;

            procedure Type_Conversion is  --  Ada RM 4.6
              kind : Type_Conversion_Kind := Unknown;
              Type_Id : constant String := To_String (CD.Id);
            begin
              Need (CD, LParent, err_missing_an_opening_parenthesis);
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
                      Emit1 (CD, k_Integer_to_Float, 0);
                    when others =>
                      Argument_Type_Not_Supported (CD);
                  end case;
                  X.TYP := Floats;
                when To_Integer =>
                  case X.TYP is
                    when Floats =>  --  Rounding to closest integer (Ada RM 4.6 (33)).
                      Emit1 (CD, k_Standard_Functions, SF_Round_Float_to_Int);
                    when Ints =>
                      null;  --  !!  Emit warning "already integer"
                    when others =>
                      Argument_Type_Not_Supported (CD);
                  end case;
                  X.TYP := Ints;
                when Unknown =>
                  Error (CD, err_type_conversion_not_supported, "no support for target type " & Type_Id);
              end case;
              Need (CD, RParent, err_closing_parenthesis_missing);
            end Type_Conversion;

          begin  --  Factor
            X.TYP := NOTYP;
            X.Ref := 0;
            Test (CD, Factor_Begin_Symbol + StrCon, FSys,
              err_factor_unexpected_symbol
            );
            if CD.Sy = StrCon then
              X.TYP := Strings;
              Emit1 (CD, k_Literal, CD.SLeng);  --  String Literal Length
              Emit1 (CD, k_Literal, CD.INum);   --  Index To String IdTab
              InSymbol;
            end if;
            while Factor_Begin_Symbol (CD.Sy) loop
              case CD.Sy is
                when IDent =>
                  I := Locate_Identifier (CD.Id, stop_on_error => True);
                  InSymbol;
                  exit when I = No_Id;  --  Id not found, error already issued by Locate_Identifier
                  declare
                    r : IdTabEntry renames CD.IdTab (I);
                  begin
                    case r.Obj is
                      when Declared_Number_or_Enum_Item =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                        if X.TYP = Floats then
                          --  Address is an index in the float constants table.
                          Emit1 (CD, k_Load_Float, r.Adr);
                        else
                          --  Here the address is actually the immediate (discrete) value.
                          Emit1 (CD, k_Literal, r.Adr);
                        end if;
                        --
                      when Variable =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                        if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                          if r.Normal then
                            F := k_Load_Address;
                          else
                            F := k_Push_Value;
                          end if;
                          Emit2 (CD, F, r.LEV, r.Adr);
                          Selector (FSys, X);
                          if Standard_or_Enum_Typ (X.TYP) then
                            Emit (CD, k_Case_34);
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
                          Emit2 (CD, F, r.LEV, r.Adr);
                        end if;
                        --
                      when TypeMark =>
                        Type_Conversion;
                        --
                      when Prozedure =>
                        Error (CD, err_expected_constant_function_variable_or_subtype);
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
                  if CD.Sy = FloatCon then
                    X.TYP := Floats;
                    Enter_or_find_Float (CD.RNum);
                    Emit1 (CD, k_Load_Float, CD.RNum_Index);
                  else
                    if CD.Sy = CharCon then
                      X.TYP := xChars;
                    else
                      X.TYP := Ints;
                    end if;
                    Emit1 (CD, k_Literal, CD.INum);
                  end if;
                  X.Ref := 0;
                  InSymbol;
                  --
                when LParent =>    --  (
                  InSymbol;
                  Expression (FSys + RParent, X);
                  Need (CD, RParent, err_closing_parenthesis_missing);
                  --
                when NOT_Symbol =>
                  InSymbol;
                  Factor (FSys, X);
                  if X.TYP = Bools then
                    Emit (CD, k_NOT_Boolean);
                  elsif X.TYP /= NOTYP then
                    Error (CD, err_resulting_type_should_be_Boolean);
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
              Test (CD, FSys, Factor_Begin_Symbol, err);
            end loop;
          end Factor;

        begin  --  Term
          Factor (FSys + FactorZ, X);
          while FactorZ (CD.Sy) loop
            OP := CD.Sy;
            InSymbol;
            Factor (FSys + FactorZ, Y);
            if X.TYP = NOTYP or X.TYP = NOTYP then
              null;  --  Something is already wrong at this point; nothing to check or emit.
            else
              case OP is
                when Times =>     --  *
                  if X.TYP in Numeric_Typ then
                    if X.TYP = Y.TYP then
                      Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                    else
                      Forbid_Type_Coercion (CD, "for this standard operator, types must be the same");
                    end if;
                  else
                    Error (CD, err_operator_not_defined_for_types);
                  end if;
                when Divide =>    --  /
                  if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                  else
                    if X.TYP = Ints then
                      Forbid_Type_Coercion (CD, "left operand's type is integer, right operand's isn't");
                      Emit1 (CD, k_Integer_to_Float, 1);  --  NB: this assumed Y.TYP was Floats!
                      X.TYP := Floats;
                    end if;
                    if Y.TYP = Ints then
                      Forbid_Type_Coercion (CD, "right operand's type is integer, left operand's isn't");
                      Emit1 (CD, k_Integer_to_Float, 0);  --  NB: this assumed Y.TYP was Floats!
                      Y.TYP := Floats;
                    end if;
                    Error (CD, err_illegal_type_for_arithmetic_expression);
                    X.TYP := NOTYP;
                  end if;
                when AND_Symbol =>
                  if X.TYP = Bools and Y.TYP = Bools then
                    Emit (CD, k_AND_Boolean);
                  else
                    Error (CD, err_resulting_type_should_be_Boolean);
                    X.TYP := NOTYP;
                  end if;
                when MOD_Symbol =>
                  if X.TYP = Ints and Y.TYP = Ints then
                    Emit (CD, k_MOD_Integer);
                  else
                    Error (CD, err_mod_requires_integer_arguments);
                    X.TYP := NOTYP;
                  end if;
                when Power =>
                  if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                  elsif X.TYP = Floats and Y.TYP = Ints then
                    Emit (CD, k_Power_Float_Integer);
                  else
                    Error (CD, err_invalid_power_operands);
                  end if;
                when others =>
                  raise Internal_error with "Unknown operator in Term";
              end case;
            end if;
          end loop;
        end Term;

      begin  --  Simple_Expression
        --  + , -
        if Plus_Minus (CD.Sy) then
          OP := CD.Sy;
          InSymbol;
          Term (FSys + Plus_Minus, X);
          if X.TYP not in Numeric_Typ then
            Error (CD, err_illegal_type_for_arithmetic_expression);
          elsif OP = Minus then
            Emit_Unary_Minus (CD, X.TYP);
          end if;
        else
          Term (FSys + TermZ, X);
        end if;
        while TermZ (CD.Sy) loop
          OP := CD.Sy;
          InSymbol;
          Term (FSys + TermZ, Y);
          if X.TYP = NOTYP or X.TYP = NOTYP then
            null;  --  Something is already wrong at this point; nothing to check or emit.
          else
            case OP is
              when OR_Symbol =>
                if X.TYP = Bools and Y.TYP = Bools then
                  Emit (CD, k_OR_Boolean);
                else
                  Error (CD, err_resulting_type_should_be_Boolean);
                  X.TYP := NOTYP;
                end if;
              when XOR_Symbol =>
                if X.TYP = Bools and Y.TYP = Bools then
                  Emit (CD, k_XOR_Boolean);
                else
                  Error (CD, err_resulting_type_should_be_Boolean);
                  X.TYP := NOTYP;
                end if;
              when Plus | Minus =>
                if X.TYP in Numeric_Typ then
                  if X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                  else
                    Forbid_Type_Coercion (CD, "for this standard operator, types must be the same");
                  end if;
                else
                  Error (CD, err_operator_not_defined_for_types);
                end if;
              when others =>  --  Doesn't happen: TermZ(OP) is True.
                null;
            end case;
          end if;
        end loop;
      end Simple_Expression;

    begin  --  Expression
      Simple_Expression (FSys + Comparison_Operator_Set, X);
      if CD.Sy in Comparison_Operator then
        OP := CD.Sy;
        InSymbol;
        Simple_Expression (FSys, Y);
        if X.TYP = Ints and Y.TYP = Floats then
          Forbid_Type_Coercion (CD, "left operand's type is integer, right operand's is floating-point");
          X.TYP := Floats;
          Emit1 (CD, k_Integer_to_Float, 1);
        elsif Y.TYP = Ints and X.TYP = Floats then
          Forbid_Type_Coercion (CD, "left operand's type is floating-point, right operand's is integer");
          Y.TYP := Floats;
          Emit1 (CD, k_Integer_to_Float, 0);
        elsif X.TYP = Enums and Y.TYP = Enums and X.Ref /= Y.Ref then
          Error (CD, err_incompatible_types_for_comparison);
        elsif X.TYP = Y.TYP then
          if Atomic_Typ (X.TYP) then
            Emit_Comparison_Instruction (CD, OP, X.TYP);
          else
            Error (CD, err_operator_not_defined_for_types);
          end if;
        else
          Error (CD, err_incompatible_types_for_comparison);
        end if;
        X.TYP := Bools;
      end if;
    end Expression;

    procedure Boolean_Expression (FSys : Symset; X : in out Item) is
    begin
      Expression (FSys, X);
      Check_Boolean (CD, X.TYP);
    end Boolean_Expression;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer; Check_read_only : Boolean) is
      X, Y : Item;
      F    : Opcode;
    begin
      pragma Assert (CD.IdTab (I).Obj = Variable);
      X.TYP := CD.IdTab (I).TYP;
      X.Ref := CD.IdTab (I).Ref;
      if CD.IdTab (I).Normal then
        F := k_Load_Address;
      else
        F := k_Push_Value;
      end if;
      Emit2 (CD, F, CD.IdTab (I).LEV, CD.IdTab (I).Adr);
      if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
        Selector (Becomes_EQL + FSys, X);
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
      Expression (Semicolon_Set, Y);
      if X.TYP = Y.TYP then
        if Standard_Typ (X.TYP) then
          Emit (CD, k_Store);
        elsif X.Ref /= Y.Ref then
          Error (CD, err_types_of_assignment_must_match);
        else
          case X.TYP is
            when Arrays =>
              Emit1 (CD, k_Copy_Block, CD.Arrays_Table (X.Ref).Size);
            when Records =>
              Emit1 (CD, k_Copy_Block, CD.Blocks_Table (X.Ref).VSize);
            when Enums =>  --  Behaves like a standard type
              Emit (CD, k_Store);
            when others =>
              null;
          end case;
        end if;
      elsif X.TYP = Floats and Y.TYP = Ints then
        Forbid_Type_Coercion (CD, "integer type value assigned to floating-point variable");
        Emit1 (CD, k_Integer_to_Float, 0);
        Emit (CD, k_Store);
      elsif X.TYP = Arrays and Y.TYP = Strings then
        if CD.Arrays_Table (X.Ref).Element_TYP /= xChars then
          Error (CD, err_types_of_assignment_must_match);
        else
          Emit1 (CD, k_String_assignment, CD.Arrays_Table (X.Ref).Size);  --  array size
        end if;
      elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
        Error (CD, err_types_of_assignment_must_match);
      end if;
    end Assignment;

    ------------------------------------------------------------------
    --------------------------------------------------------Statement--
    procedure Statement (FSys : Symset) is

      procedure Multi_Statement (Sentinal : Symset) is   -- Hathorn
        nxtSym : Symset;
      begin
        if Sentinal (CD.Sy) then -- GdM 15-Aug-2014: there should be at least one statement.
          Error (CD, err_statement_expected);
        end if;
        nxtSym := Sentinal + Statement_Begin_Symbol;
        loop
          Statement (nxtSym); --MRC,was: UNTIL (Sy IN Sentinal);
          exit when Sentinal (CD.Sy) or Err_Count > 0;
        end loop;
      end Multi_Statement;

      procedure Accept_Statement is            -- Hathorn

        procedure Accept_Call (FSys : Symset; I : Integer) is
          pragma Unreferenced (I, FSys);
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
        I_Entry := Locate_Identifier (CD.Id);
        if CD.IdTab (I_Entry).Obj /= aEntry then
          Error (CD, err_use_Small_Sp);
        end if;
        InSymbol;
        Accept_Call (FSys, I_Entry);
        Emit1 (CD, k_Accept_Rendezvous, I_Entry);
        if CD.Sy = DO_Symbol then
          if Level = LMax then
            Fatal (LEVELS);  --  Exception is raised there.
          end if;
          Level              := Level + 1;
          CD.Display (Level) := CD.IdTab (I_Entry).Ref;
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
        X : Item;
      begin
        pragma Assert (CD.Sy = EXIT_Symbol);
        InSymbol;  --  Consume EXIT symbol.
        if CD.Sy = WHEN_Symbol then  --  Conditional Exit
          InSymbol;
          Boolean_Expression (Semicolon_Set, X);
          Emit1 (CD, k_Conditional_Jump, CD.LC + 2);  --  Conditional jump around Exit
        end if;
        Emit1 (CD, k_Jump, dummy_address);  --  Unconditional jump with dummy address to be patched
      end Exit_Statement;

      procedure IF_Statement is
        X        : Item;
        LC0, LC1 : Integer;
      begin
        InSymbol;
        Boolean_Expression (FSys + DO_THEN, X);
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
          Boolean_Expression (FSys + DO_THEN, X);
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
        X, Y : Item;
        F : Opcode;
        Block_Idx : Integer;
      begin
        if Block_ID = Main_Program_ID then
          Error (CD, err_illegal_return_statement_from_main); -- !! but... this is legal in Ada !!
        end if;
        Block_Idx := Locate_Identifier (Block_ID);
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
          if CD.IdTab (Block_Idx).Ref = CD.Display (Level) then
            X.TYP := CD.IdTab (Block_Idx).TYP;
            X.Ref := CD.IdTab (Block_Idx).Ref;
            if CD.IdTab (Block_Idx).Normal then
              F := k_Load_Address;
            else
              F := k_Push_Value;
            end if;
            Emit2 (CD, F, CD.IdTab (Block_Idx).LEV + 1, 0);
            --
            Expression (Semicolon_Set, Y);
            if X.TYP = Y.TYP then
              if Standard_Typ (X.TYP) then
                Emit (CD, k_Store);
              elsif X.Ref /= Y.Ref then
                Error (CD, err_types_of_assignment_must_match);
              end if;
            elsif X.TYP = Floats and Y.TYP = Ints then
              Forbid_Type_Coercion (CD, "integer type value returned as floating-point");
              Emit1 (CD, k_Integer_to_Float, 0);
              Emit (CD, k_Store);
            elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
              Error (CD, err_types_of_assignment_must_match);
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
        Y : Item;
      begin
        InSymbol;
        if CD.Sy = Semicolon then
          Skip (CD, Semicolon, err_missing_expression_for_delay);
        else                  -- calculate delay value
          Expression (Semicolon_Set, Y);
          if Y.TYP /= Floats then
            Error (CD, err_wrong_type_in_DELAY);
          end if;
        end if;
        Emit (CD, k_Delay);
      end Delay_Statement;

      procedure CASE_Statement is
        X         : Item;
        I, J, LC1 : Integer;
        type CASE_Label_Value is record
          Val : Integer;  --  value of a choice in a CASE statement
          LC  : Integer;  --  instruction address
        end record;
        CaseTab : array (1 .. Cases_Max) of CASE_Label_Value;
        ExitTab : array (1 .. Cases_Max) of Integer;
        others_flag : Boolean := False;

        procedure CASE_Label is
          Lab : ConRec;
          K   : Integer;
        begin
          Number_Declaration (FSys + Alt_Finger, Lab);
          if Lab.TP /= X.TYP then
            Error (CD, err_case_label_not_same_type_as_case_clause);
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
          if CD.Sy = WHEN_Symbol then
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
            Need (CD, Finger, err_FINGER_missing);
            Multi_Statement (END_WHEN);
            J           := J + 1;
            ExitTab (J) := CD.LC;
            Emit (CD, k_Jump);
          else
            Error (CD, err_WHEN_missing);
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
          Error (CD, err_bad_type_for_a_case_statement); --- !! mmmh: enums ?...
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

        while CD.Sy = WHEN_Symbol loop  --  All cases are parsed here.
          One_CASE;
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
        X : Item;
        LC_Cond_Eval, LC_Cond_Jump : Integer;
      begin
        InSymbol;  --  Consume WHILE symbol.
        LC_Cond_Eval := CD.LC;
        Boolean_Expression (FSys + DO_LOOP, X);
        LC_Cond_Jump := CD.LC;
        Emit (CD, k_Conditional_Jump);
        LOOP_Statement (k_Jump, LC_Cond_Eval);
        CD.ObjCode (LC_Cond_Jump).Y := CD.LC;
      end WHILE_Statement;

      procedure FOR_Statement is  --  RM 5.5 (9)
        X         : Item;
        F         : Opcode;
        LC1, last : Integer;
      begin
        InSymbol;  --  Consume FOR symbol.
        if CD.Sy = IDent then
          if CD.Id_Count = Id_Table_Max then
            Fatal (IDENTIFIERS);  --  Exception is raised there.
          end if;
          --  Declare local loop control Variable  --  added Hathorn
          last := CD.Blocks_Table (CD.Display (Level)).Last;
          CD.Id_Count := CD.Id_Count + 1;
          declare
            r : IdTabEntry renames CD.IdTab (CD.Id_Count);
          begin
            r.Name      := CD.Id;
            r.Link      := last;
            r.Obj       := Variable;
            r.Read_only := True;
            r.TYP       := NOTYP;
            r.Ref       := 0;
            r.Normal    := True;
            r.LEV       := Level;
            r.Adr       := Dx;
          end;
          CD.Blocks_Table (CD.Display (Level)).Last  := CD.Id_Count;
          Dx                                      := Dx + 1;
          if Dx > MaxDX then
            MaxDX := Dx;
          end if;
          CD.Blocks_Table (CD.Display (Level)).VSize := MaxDX;
        else
          Skip (CD, Fail_after_FOR + FSys, err_identifier_missing);
        end if;
        --
        Emit2 (CD, k_Load_Address, CD.IdTab (CD.Id_Count).LEV, CD.IdTab (CD.Id_Count).Adr);
        InSymbol;
        F := k_FOR_Forward_Begin;
        if CD.Sy = IN_Symbol then         --       "IN"  in  "for i in reverse 1 .. 10 loop"
          InSymbol;
          if CD.Sy = REVERSE_Symbol then  --  "REVERSE"  in  "for i in reverse 1 .. 10 loop"
            F := k_FOR_Reverse_Begin;
            InSymbol;
          end if;
          --  !!  Here we should have a more general syntax:
          --      discrete_subtype_definition RM 3.6 (6) which is either
          --      a subtype_indication 3.2.2 (3) : name [constraint] like "Color [range red .. blue]"
          --      or a range 3.5 (3): "low .. high" or range_attribute_reference: A'Range
          Expression (END_LOOP_RANGE_Double_Dot + FSys, X);  --  Lower bound
          CD.IdTab (CD.Id_Count).TYP := X.TYP;
          if not Discrete_Typ (X.TYP) then
            Error (CD, err_control_variable_of_the_wrong_type);
          end if;
          if CD.Sy = Range_Double_Dot_Symbol then               --  ..
            InSymbol;
            Expression (FSys + LOOP_Symbol, X);              --  Upper bound
            if CD.IdTab (CD.Id_Count).TYP /= X.TYP then
              Error (CD, err_first_and_last_must_have_matching_types);
            end if;
          else
            Skip (CD, END_LOOP_Semicolon + FSys, err_expecting_double_dot);
          end if;
        else
          Skip (CD, FSys + LOOP_Symbol, err_IN_missing);
        end if;
        LC1 := CD.LC;
        Emit (CD, F);
        LOOP_Statement (For_END (F), CD.LC);
        --  Forget the iterator variable.
        CD.ObjCode (LC1).Y                        := CD.LC;
        CD.Id_Count                               := CD.Id_Count - 1;
        CD.Blocks_Table (CD.Display (Level)).Last := last;
        Dx                                        := Dx - 1;
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
          Y                  : Item;
        begin
          I := Locate_Identifier (CD.Id);
          if CD.IdTab (I).Obj = aTask then
            InSymbol;
            Entry_Call (FSys, I, -1);
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
                  Expression (Semicolon_Set, Y);
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

          begin         -- Accept_Statment_2
            InSymbol;
            I := Locate_Identifier (CD.Id);
            if CD.IdTab (I).Obj /= aEntry then
              Select_Error (err_use_Small_Sp);
            end if;
            InSymbol;
            Accept_Call_2 (FSys, I);
            Emit2 (CD, k_Selective_Wait, 2, I);          --  Retain Entry Index
            if IAlt < 10 then
              IAlt := IAlt + 1;
            else
              Fatal (PATCHING);
            end if;
            Alt (IAlt) := CD.LC;              --  SAVE LOCATION FOR PATCHING
            Emit2 (CD, k_Selective_Wait, 3, CD.LC);  --  ACCEPT IF Ready ELSE Skip To LC
            -- CONDITIONAL ACCEPT MUST BE ATOMIC
            if CD.Sy = DO_Symbol then
              if Level = LMax then
                Fatal (LEVELS);  --  Exception is raised there.
              end if;
              Level              := Level + 1;
              CD.Display (Level) := CD.IdTab (I).Ref;
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
                for I in 1 .. IAlt loop
                  CD.ObjCode (Alt (I)).Y  := CD.LC;
                end loop;
                -- patch
                IAlt := 0;
                InSymbol;          -- WHENSTATEMENT
                Boolean_Expression (FSys + Finger, X);
                InSymbol;
                if CD.Sy = ACCEPT_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := CD.LC;
                    Emit (CD, k_Conditional_Jump);
                    Accept_Statement_2;
                  end if;
                elsif CD.Sy = DELAY_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := CD.LC;
                    Emit (CD, k_Conditional_Jump);
                    InSymbol;
                    Expression (FSys + Semicolon, Y);
                    Emit2 (CD, k_Selective_Wait, 4, CD.LC + 2);  --  Update delay time
                    if Y.TYP /= Floats then
                      Select_Error (err_wrong_type_in_DELAY);
                    end if;
                    if IAlt > 10 then
                      Fatal (PATCHING);
                    end if;
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := CD.LC;
                    Emit (CD, k_Jump);
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
                JSD (ISD) := CD.LC;
                Emit (CD, k_Jump);          --  patch JMP ADDRESS AT EndSy
              -- end WHEN_Symbol

              when ACCEPT_Symbol =>
                for I in 1 .. IAlt loop
                  CD.ObjCode (Alt (I)).Y  := CD.LC;
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
                JSD (ISD) := CD.LC;
                Emit (CD, k_Jump);

              when OR_Symbol =>  --  OR STATEMENT
                InSymbol;

              when ELSE_Symbol =>
                for I in 1 .. IAlt loop
                  --  patch ObjCode
                  CD.ObjCode (Alt (I)).Y  := CD.LC;
                end loop;
                IAlt := 0;
                InSymbol;
                Multi_Statement (END_Set);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := CD.LC;
                Emit (CD, k_Jump);
              -- end ELSE_Symbol

              when DELAY_Symbol =>
                for I in 1 .. IAlt loop
                  CD.ObjCode (Alt (I)).Y  := CD.LC;
                end loop;
                --  patch
                IAlt := 0;
                --  Generate a Task delay, calculate return value if req'D
                InSymbol;
                if CD.Sy = Semicolon then
                  Skip (CD, Semicolon, err_missing_expression_for_delay);
                else          -- calculate return value
                  Expression (Semicolon_Set, Y);
                  Emit2 (CD, k_Selective_Wait, 4, CD.LC + 2);  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  if IAlt > 10 then
                    Fatal (PATCHING);
                  end if;
                  IAlt       := IAlt + 1;
                  Alt (IAlt) := CD.LC;
                  Emit (CD, k_Jump);
                end if;
                InSymbol;
                Multi_Statement (ELSE_END_OR);
                if ISD > 10 then
                  Fatal (PATCHING);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := CD.LC;
                Emit (CD, k_Jump);
              -- end DELAY_Symbol

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
                for I in 1 .. IAlt loop
                  CD.ObjCode (Alt (I)).Y  := CD.LC;
                end loop;
                -- patch
                IAlt := 0;
                if do_terminate then
                  Emit2 (CD, k_Selective_Wait, 5, StartSel);
                else
                  Emit2 (CD, k_Selective_Wait, 6, StartSel);
                end if;   -- Suspend
                for I in 1 .. ISD loop
                  CD.ObjCode (JSD (I)).Y  := CD.LC;
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

      procedure Standard_Procedure (N : Integer) is
        -- NB: Most of this part will disappear when Ada.Text_IO etc.
        -- are implemented, as well as overloading.
        I                 : Integer;
        F                 : Opcode;
        X, Y              : Item;
        do_first_InSymbol : Boolean := True;
      begin
        case N is -- Numbers: see EnterStdFcns in HAC.Compiler
          when 1 | 2 =>  -- GET, GET_LINE
            if CD.Sy = LParent then
              InSymbol;
              I := Get_File_Pointer (CD.Id);  -- Schoening
              if I = No_File_Index then
                Emit1 (CD, k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              else -- First parameter is a file variable
                Emit1 (CD, k_Set_current_file_pointer, I);
                InSymbol;
                if CD.Sy /= Comma then
                  if CD.Sy = RParent then
                    goto SKIP1b;
                  else
                    Error (CD, err_identifier_missing);
                  end if;
                end if;
              end if;
              loop
                if do_first_InSymbol then
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if CD.Sy /= IDent then
                  Error (CD, err_identifier_missing);
                else
                  I := Locate_Identifier (CD.Id);
                  InSymbol;
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
                        Selector (FSys + Comma_RParent, X);
                      end if;
                      if X.TYP = Ints or
                         X.TYP = Floats or
                         X.TYP = xChars or
                         X.TYP = NOTYP
                      then
                        Emit1 (CD, k_Read, Types'Pos (X.TYP));
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
              InSymbol;
              I := Get_File_Pointer (CD.Id);   -- Schoening
              if I = No_File_Index then
                Emit1 (CD, k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              else -- First parameter is a file variable
                Emit1 (CD, k_Set_current_file_pointer, I);
                InSymbol;
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
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if CD.Sy = StrCon then
                  Emit1 (CD, k_Literal, CD.SLeng);
                  Emit1 (CD, k_Write_String, CD.INum);
                  InSymbol;
                else
                  Expression (FSys + Colon_Comma_RParent, X);
                  if X.TYP = Enums then
                    X.TYP := Ints;
                  end if;
                  if (not Standard_Typ (X.TYP)) and X.TYP /= Strings then
                    Error (CD, err_illegal_parameters_to_Put);
                  end if;
                  if CD.Sy = Colon then
                    InSymbol;
                    Expression (FSys + Colon_Comma_RParent, Y);
                    if Y.TYP /= Ints then
                      Error (CD, err_parameter_must_be_Integer);
                    end if;
                    if CD.Sy = Colon then  --  ':' Pascal-ism (Write/WriteLn) !!
                      if X.TYP /= Floats then
                        Error (CD, err_parameter_must_be_of_type_Float);
                      end if;
                      InSymbol;
                      Expression (FSys + Comma_RParent, Y);
                      if Y.TYP /= Ints then
                        Error (CD, err_parameter_must_be_Integer);
                      end if;
                      Emit (CD, k_Write_Float);
                    else
                      Emit1 (CD, k_Write_2, Types'Pos (X.TYP));
                    end if;
                  elsif X.TYP = Strings then
                    Emit1 (CD, k_Write_String, X.Ref);
                  else
                    Emit1 (CD, k_Write_1, Types'Pos (X.TYP));
                  end if;
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
              InSymbol;
              if CD.Sy /= IDent then
                Error (CD, err_undefined_identifier);
              else
                I := Locate_Identifier (CD.Id);
                InSymbol;
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
                      Selector (FSys + RParent, X);
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
              InSymbol;
              I := Get_File_Pointer (CD.Id);
              if I = No_File_Index then
                Error (CD, err_identifier_missing);
              else
                Emit2 (CD, k_File_I_O, I, N);
              end if;
              InSymbol;
              Need (CD, RParent, err_closing_parenthesis_missing);
            end if;  -- reset

          when 10 =>        -- CursorAt
            -- Cramer
            if CD.Sy /= LParent then
              Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
            else
              begin
                InSymbol;
                Expression (Colon_Comma_LParent_RParent_Semicolon, X);
                if X.TYP /= Ints then
                  Skip (CD, Semicolon, err_parameter_must_be_Integer);
                end if;
                if CD.Sy /= Comma then
                  Skip (CD, Semicolon, err_COMMA_missing);
                else
                  InSymbol;
                  Expression (Colon_Comma_LParent_RParent_Semicolon, X);
                  if X.TYP /= Ints then
                    Skip (CD, Semicolon, err_parameter_must_be_Integer);
                  end if;
                  if CD.Sy = Comma then
                    Skip (CD, Semicolon, err_number_of_parameters_do_not_match);
                  elsif CD.Sy /= RParent then
                    Skip (CD, Semicolon, err_closing_parenthesis_missing);
                  else
                    Emit (CD, k_Cursor_At);
                    InSymbol;
                  end if;
                end if;
              end;
            end if;                -- CursorAt

          when 11 =>                   -- Quantum
            -- Cramer
            if CD.Sy /= LParent then
              Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (RParent_Set, X);
              if X.TYP /= Floats then
                Skip (CD, Semicolon, err_parameter_must_be_of_type_Float);
              end if;
              if CD.Sy /= RParent then
                Skip (CD, Semicolon, err_closing_parenthesis_missing);
              else
                Emit (CD, k_Set_Quantum_Task);
                InSymbol;
              end if;
            end if;                -- Quantum

          when 12 =>                   -- Set Priority
            -- Cramer
            if CD.Sy /= LParent then
              Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (RParent_Set, X);
              if X.TYP /= Ints then
                Skip (CD, Semicolon, err_parameter_must_be_Integer);
              end if;
              if CD.Sy /= RParent then
                Skip (CD, Semicolon, err_closing_parenthesis_missing);
              else
                Emit (CD, k_Set_Task_Priority);
                InSymbol;
              end if;
            end if;                -- Priority
            --
          when 13 =>                   -- Set Priority Inheritance,INHERITP
            -- Cramer
            if CD.Sy /= LParent then
              Skip (CD, Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Boolean_Expression (RParent_Set, X);
              if CD.Sy /= RParent then
                Skip (CD, Semicolon, err_closing_parenthesis_missing);
              else
                Emit (CD, k_Set_Task_Priority_Inheritance);
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
          CD, err_not_yet_implemented,
          hint => "Block statements don't work yet",
          stop_on_error => True
        );
        --
        Block (CD, FSys, Is_a_function, True, Level + 1, CD.Id_Count, block_name, block_name);  --  !! up/low case
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
        Enter (new_ident_for_statement, CD.Id_with_case, Label);
        Test (CD, Colon_Set, FSys,
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
      if Err_Count > 0 then
        return;
      end if;
      if Statement_Begin_Symbol (CD.Sy) then
        case CD.Sy is
          when IDent =>
            I_Statement := Locate_Identifier (CD.Id, No_Id_Fail => False);
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
                  Entry_Call (FSys, I_Statement, CallSTDE);
                when Prozedure =>
                  if CD.IdTab (I_Statement).LEV = 0 then
                    Standard_Procedure (CD.IdTab (I_Statement).Adr);
                  else
                    Call (FSys, I_Statement, CallSTDP);
                  end if;
                when Label =>
                  Error (CD, err_duplicate_label, To_String (CD.Id));
                  Test (CD, Colon_Set, FSys, err_colon_missing);
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
      Test (CD, FSys - Semicolon, Semicolon_Set, err_incorrectly_used_symbol);
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
          Task_Declaration;
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
      MaxDX              := Dx;
      CD.IdTab (Prt).Adr := CD.LC;
      --  Copy initialization (elaboration) ObjCode from end of ObjCode table
      Init_Code_Idx := CMax + ICode;
      while Init_Code_Idx > CMax loop
        CD.ObjCode (CD.LC) := CD.ObjCode (Init_Code_Idx);
        CD.LC              := CD.LC + 1;
        Init_Code_Idx      := Init_Code_Idx - 1;
      end loop;
      CMax := CMax + ICode;  -- Restore CMax to the initial max (=CDMax)
    end Statements_Part_Setup;

    procedure Statements_List is
    begin
      if CD.Sy = END_Symbol then  --  GdM 15-Aug-2014: there should be at least one statement.
        Error (CD, err_statement_expected);
      end if;
      loop
        Statement (FSys + END_Symbol);
        exit when CD.Sy = END_Symbol or Err_Count > 0;
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
          I_Res_Type := Locate_Identifier (CD.Id);
          InSymbol;
          if I_Res_Type /= 0 then
            if CD.IdTab (I_Res_Type).Obj /= TypeMark then
              Error (CD, err_missing_a_type_identifier, stop_on_error => True);
            elsif Standard_Typ (CD.IdTab (I_Res_Type).TYP) then
              CD.IdTab (Prt).TYP := CD.IdTab (I_Res_Type).TYP;
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
    if Err_Count > 0 then
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
      Test (CD, Symbols_after_Subprogram_Identifier, FSys, err_incorrectly_used_symbol);
    end if;
    if CD.IdTab (Prt).Ref > 0 then
      PRB := CD.IdTab (Prt).Ref;
    else
      Enter_Block (Prt);
      PRB                := CD.Blocks_Count;
      CD.IdTab (Prt).Ref := PRB;
    end if;
    CD.Display (Level) := PRB;
    CD.IdTab (Prt).TYP := NOTYP;
    if CD.Sy = LParent and Level > 1 then
      Formal_Parameter_List;
    end if;
    --
    if Err_Count > 0 then
      return;
    end if;
    --
    CD.Blocks_Table (PRB).LastPar := CD.Id_Count;
    CD.Blocks_Table (PRB).PSize   := Dx;
    --
    if Is_a_function and not Is_a_block_statement then
      Function_Result_Profile;
    end if;
    --
    if CD.Sy = Semicolon then  --  end of specification part
      CD.Blocks_Table (PRB).VSize := Dx;
      CD.IdTab (Prt).Adr          := -1;    -- address of body TBD
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
      elsif Err_Count > 0 then
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
    if Block_ID /= Main_Program_ID and not Is_a_block_statement then
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
