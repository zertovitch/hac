with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;

with Ada.Unchecked_Deallocation;

package body HAC.Parser is

  use HAC.Data;

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block(
    FSys                 : HAC.Data.Symset;
    Is_a_function        : Boolean;       --  RETURN [Value] statement expected
    Is_a_block_statement : Boolean;       --  5.6 Block Statements
    Level_A              : Integer;
    Prt                  : Integer;
    BlockID              : HAC.Data.Alfa  --  Name of this block (if any)
  )
  is

    Level : Integer := Level_A;

    type ConRec is record
      TP : Types;
      I  : Integer;
      R  : Float;
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

    procedure EnterArray (TP : Types; L, H : Integer) is
      Lz, Hz : Integer;
    begin
      if L > H then
        Error (err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)"); -- !!
      end if;
      Lz := L;
      Hz := H;
      if abs (L) > XMax or abs (H) > XMax then
        Error (err_illegal_array_bounds);
        Lz := 0;
        Hz := 0;
      end if;
      if A = AMax then
        Fatal (ARRAYS_table_overflow);
      else
        A := A + 1;
        declare
          r : ATabEntry renames ArraysTab (A);
        begin
          r.Index_TYP := TP;
          r.Low    := Lz;
          r.High   := Hz;
        end;
      end if;
    end EnterArray;

    ------------------------------------------------------------------
    -------------------------------------------------------EnterBlock-
    procedure EnterBlock (Tptr : Integer) is
    begin
      if B = BMax then
        Fatal (PROCEDURES_table_overflow);
      else
        B                    := B + 1;
        BlockTab (B).Id      := IdTab (Tptr).Name;
        BlockTab (B).Last    := 0;
        BlockTab (B).LastPar := 0;
        BlockTab (B).SrcFrom := Line_Count;
      end if;
    end EnterBlock;

    ------------------------------------------------------------------
    -------------------------------------------------------EnterFloat-
    procedure EnterFloat (X : Float) is
    begin
      if C2 = C2Max - 1 then
        Fatal (FLOAT_constants_table_overflow);
      else
        FloatPtTab (C2 + 1) := X;
        C1                  := 1;
        while FloatPtTab (C1) /= X loop
          C1 := C1 + 1;
        end loop;
        if C1 > C2 then
          C2 := C1;
        end if;
      end if;
    end EnterFloat;

    ------------------------------------------------------------------
    -------------------------------------------------------------Skip-
    procedure Skip (FSys : Symset; N : Error_code) is

      function StopMe return Boolean is
      begin
        return False;
      end StopMe;

    begin
      Error (N);
      SkipFlag := True;
      while not FSys (Sy) loop
        InSymbol;
        if StopMe then
          raise Failure_1_0;
        end if;
      end loop;

      InSymbol;    -- Manuel:  If this InSymbol call is
      -- omitted, the system will get in an
      -- infinite loop on the statement:
      --  put_lin("Typo is on purpose");

      if StopMe then
        raise Failure_1_0;
      end if;
      if SkipFlag then
        EndSkip;
      end if;
    end Skip;

    procedure Skip (S : KeyWSymbol; N : Error_code) is
      to_skip : Symset := Empty_Symset;
    begin
      to_skip (S) := True;
      Skip (to_skip, N);
    end Skip;

    ------------------------------------------------------------------
    -------------------------------------------------------------Test-
    procedure Test (S1, S2 : Symset; N : Error_code) is
    begin
      if not S1 (Sy) then
        Skip (S1 + S2, N);
      end if;
    end Test;

    ------------------------------------------------------------------
    ---------------------------------------------------Test_Semicolon-
    procedure Test_Semicolon is
      comma_or_colon : constant Symset :=
       Symset'(Comma | Colon => True, others => False);
    begin
      if Sy = Semicolon then
        InSymbol;
      else
        Error (err_SEMICOLON_missing);
        if comma_or_colon (Sy) then
          InSymbol;
        end if;
      end if;
      Test
       (Symset'((IDent | TYPE_Symbol | TASK_Symbol => True, others => False)) +
        Block_Begin_Symbol,
        FSys,
        err_incorrectly_used_symbol);
    end Test_Semicolon;

    ------------------------------------------------------------------
    ----------------------------------------------------------TestEnd-
    procedure TestEnd is
    begin
      if Sy = END_Symbol then
        InSymbol;
      else
        Skip (Semicolon, err_END_missing);
      end if;
    end TestEnd;

    ------------------------------------------------------------------
    ------------------------------------------------------------Enter-
    procedure Enter (Id : Alfa; K : aObject) is
      J, L : Integer;
    begin
      if T = TMax then
        Fatal (IDENTIFIERS_table_overflow);
      else
        IdTab (0).Name := Id;        -- sentinel
        J              := BlockTab (Display (Level)).Last;
        L              := J;
        while IdTab (J).Name /= Id loop
          J := IdTab (J).Link;
        end loop;
        -- Follow the chain of identifiers for current Level.
        if J /= 0 then
          Error (err_duplicate_identifier, Id);
        else      -- Enter identifier if there is room in table IdTab
          T                                := T + 1;
          IdTab (T)                        :=
           (Name   => Id,
            Link   => L,
            Obj    => K,
            TYP    => NOTYP,
            Ref    => 0,
            Normal => True,
            LEV    => Level,
            Adr    => 0);
          BlockTab (Display (Level)).Last  := T;  -- update start of identifer chain
        end if;
      end if;
    end Enter;

    ------------------------------------------------------------------
    ------------------------------------------------Locate_identifier-
    function Locate_identifier (Id : Alfa; No_Id_Fail: Boolean:= True) return Natural is
      L, J : Integer;
    begin
      L              := Level;
      IdTab (0).Name := Id;      -- sentinel
      loop
        J := BlockTab (Display (L)).Last;
        while IdTab (J).Name /= Id loop
          J := IdTab (J).Link;
        end loop;
        L := L - 1;
        exit when L < 0 or J /= 0;
      end loop;
      if J = No_Id and No_Id_Fail then
        Error (err_undefined_identifier);
      end if;
      return J;
    end Locate_identifier;

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
    ----------------------------------------------------EnterVariable-
    procedure EnterVariable is
    begin
      if Sy = IDent then
        Enter (Id, Variable);
        InSymbol;
      else
        Error (err_identifier_missing);
      end if;
    end EnterVariable;

    ------------------------------------------------------------------
    ---------------------------------------------------------Constant-
    procedure KKonstant (FSys : Symset; C : in out ConRec) is
      X, Sign : Integer;
    begin
      C.TP := NOTYP;
      C.I  := 0;
      Test (ConstBegSys, FSys, err_illegal_symbol_for_a_constant);
      if ConstBegSys (Sy) then
        if Sy = CharCon then
          C.TP := xChars;
          C.I  := INum;
          InSymbol;
        else
          Sign := 1;
          if Sy = Plus or Sy = MinUS then
            if Sy = MinUS then
              Sign := -1;
            end if;
            InSymbol;
          end if;

          if Sy = IDent then
            X := Locate_identifier (Id);
            if X /= 0 then
              if IdTab (X).Obj /= Konstant then
                Error (err_illegal_constant_or_constant_identifier);
              else
                C.TP := IdTab (X).TYP;
                if C.TP = Floats then
                  C.R := Float (Sign) * FloatPtTab (IdTab (X).Adr);
                else
                  C.I := Sign * IdTab (X).Adr;
                end if;
              end if;
            end if; -- X /= 0
            InSymbol;
          elsif Sy = IntCon then
            C.TP := Ints;
            C.I  := Sign * INum;
            InSymbol;
          elsif Sy = FloatCon then
            C.TP := Floats;
            C.R  := Float (Sign) * RNum;
            InSymbol;
          else
            Skip (FSys, err_illegal_symbol_for_a_constant);
          end if;
        end if;
        Test (FSys, Empty_Symset, err_incorrectly_used_symbol);
      end if;

    end KKonstant;

    ------------------------------------------------------------------
    --------------------------------------------------------------TYP-

    procedure TYP (FSys : Symset; TP : out Types; RF, Sz : out Integer) is
      I, ECount            : Integer;
      ELTP                 : Types;
      ELRF                 : Integer;
      ELSZ, Offset, T0, T1 : Integer;
      StrArray             : Boolean;

      FSys_gnagna : constant Symset := FSys - Semicolon_Comma_IDent + END_set;

      procedure ArrayTyp (ARef, Arsz : in out Integer; StrAr : Boolean) is
        ELTP       : Types;
        Low, High  : ConRec;
        ELRF, ELSZ : Integer;
      begin
        KKonstant
         (Symset'((RANGE_Symbol | RParent | OF_Symbol => True, others => False)) +
          FSys,
          Low);

        if Low.TP = Floats then
          Error (err_illegal_array_bounds, "Float type expected");
          Low.TP := Ints;
          Low.I  := 0;
        end if;
        if Sy = RANGE_Symbol then
          InSymbol;
        else
          Error (err_expecting_dot_dot);
        end if;

        KKonstant
         (Symset'(Comma | RParent | OF_Symbol => True, others => False) + FSys,
          High);

        if High.TP /= Low.TP then
          Error (err_illegal_array_bounds, "types do not match");
          High.I := Low.I;
        end if;
        EnterArray (Low.TP, Low.I, High.I);
        ARef := A;
        if StrAr then
          ELTP := xChars;
          ELRF := 0;
          ELSZ := 1;
          if Sy = RParent then
            InSymbol;
          else
            Error (err_closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
        elsif Sy = Comma then -- Multidimensional array is array(range_1) of array(range_2,...)
          InSymbol;
          ELTP := Arrays;
          ArrayTyp (ELRF, ELSZ, StrAr);
        else
          if Sy = RParent then
            InSymbol;
          else
            Error (err_closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
          if Sy = OF_Symbol then
            InSymbol;
          else
            Error (err_missing_OF);
          end if;
          TYP (FSys, ELTP, ELRF, ELSZ);
        end if;
        Arsz     := (High.I - Low.I + 1) * ELSZ;
        declare
          r : ATabEntry renames ArraysTab (ARef);
        begin
          r.Size   := Arsz; -- NB: Index_TYP, Low, High already set
          r.Element_TYP  := ELTP;
          r.ELREF  := ELRF;
          r.ELSize := ELSZ;
        end;
      end ArrayTyp;

    begin  -- Type
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
            I := Locate_identifier (Id);
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
            if Sy = LParent then
              InSymbol;
            else
              Error (err_missing_an_opening_parenthesis);
              if Sy = LBrack then
                InSymbol;
              end if;
            end if;
            TP := Arrays;
            ArrayTyp (RF, Sz, StrArray);

          when RECORD_Symbol =>

            InSymbol;
            EnterBlock (T);
            TP := Records;
            RF := B;
            if Level = LMax then
              Fatal (LEVEL_overflow);
            end if;
            Level           := Level + 1;
            Display (Level) := B;
            Offset          := 0;

            while not FSys_gnagna (Sy) loop

              if Sy = IDent then  -- field section
                T0 := T;
                EnterVariable;
                while Sy = Comma loop
                  InSymbol;
                  EnterVariable;
                end loop;

                if Sy = Colon then
                  InSymbol;
                else
                  Error (err_colon_missing);
                end if;
                T1 := T;
                TYP
                 (FSys +
                  Symset'(Semicolon  |
                  END_Symbol         |
                  Comma              |
                  IDent              => True,
                          others => False),
                  ELTP,
                  ELRF,
                  ELSZ);
                while T0 < T1 loop
                  T0             := T0 + 1;
                  IdTab (T0).TYP := ELTP;
                  IdTab (T0).Ref := ELRF;
                  IdTab (T0).Adr := Offset;
                  Offset         := Offset + ELSZ;
                end loop;
              end if;
              if Sy /= END_Symbol then
                if Sy = Semicolon then
                  InSymbol;
                else
                  Error (err_SEMICOLON_missing);
                  if Sy = Comma then
                    InSymbol;
                  end if;
                end if;

                Test
                 (Symset'(IDent | END_Symbol | Semicolon => True, others => False),
                  FSys,
                  err_incorrectly_used_symbol);
              end if;
            end loop;

            BlockTab (RF).VSize := Offset;
            Sz                  := Offset;
            BlockTab (RF).PSize := 0;
            InSymbol;
            if Sy = RECORD_Symbol then
              InSymbol;
            else
              Error (err_RECORD_missing);
            end if;
            Level := Level - 1;
          -- end of RECORD_Symbol

          when LParent =>    -- Enumeration Type
            -- Hathorn
            TP     := Enums;
            RF     := T;
            ECount := 0;
            loop
              InSymbol;
              if Sy = IDent then
                ECount := ECount + 1;
                Enter (Id, Konstant);
                IdTab (T).TYP := Enums;
                IdTab (T).Ref := RF;
                IdTab (T).Adr := ECount;
              else
                Error (err_incorrectly_used_symbol);
              end if;
              InSymbol;
              exit when Sy /= Comma;
            end loop;

            Sz := ECount;
            if Sy = RParent then
              InSymbol;
            else
              Skip (Semicolon, err_closing_parenthesis_missing);
            end if;
          -- end of Enumeration Type

          when others =>
            null;

        end case; -- Sy
        Test (FSys, Empty_Symset, err_incorrectly_used_symbol);
      end if;
    end TYP;

    ------------------------------------------------------------------
    ----------------------------------------------------ParameterList-
    procedure ParameterList is  -- formal parameter list
      RF, Sz, X, T0 : Integer;
      TP            : Types := NOTYP;
      ValParam      : Boolean;
    begin
      InSymbol;
      RF := 0;
      Sz := 0;
      Test (IDent_set, FSys + RParent, err_identifier_missing);
      while Sy = IDent loop
        T0 := T;
        EnterVariable;
        while Sy = Comma loop
          InSymbol;
          EnterVariable;
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
            X := Locate_identifier (Id);
            InSymbol;
            if X /= 0 then
              if IdTab (X).Obj = TypeMark then
                TP := IdTab (X).TYP;
                RF := IdTab (X).Ref;
                if ValParam then
                  Sz := IdTab (X).Adr;
                else
                  Sz := 1;
                end if;
              else
                Error (err_missing_a_type_identifier);
              end if;
            end if; -- X /= 0
          end if;
          Test
           (Symset'(Semicolon  |
            RParent            |
            Comma              |
            IDent              => True,
                    others => False),
            FSys,
            err_SEMICOLON_missing);

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
          end loop; -- while T0 < T

        else
          Error (err_colon_missing);
        end if;
        if Sy /= RParent then
          if Sy = Semicolon then
            InSymbol;
          else
            Error (err_SEMICOLON_missing);
            if Sy = Comma then
              InSymbol;
            end if;
          end if;
          Test (IDent_set, FSys + RParent, err_incorrectly_used_symbol);
        end if;
      end loop;  -- while Sy = IDent
      if Sy = RParent then
        InSymbol;
        Test
         (Symset'(IS_Symbol | RETURN_Symbol | Semicolon => True, others => False),
          FSys,
          err_incorrectly_used_symbol);
      else
        Error (err_closing_parenthesis_missing);
      end if;
    end ParameterList;

    ------------------------------------------------------------------
    -------------------------------------------------Type_Declaration-
    procedure Type_Declaration is
      TP         : Types;
      RF, Sz, T1 : Integer;
    begin
      InSymbol;
      Test (IDent_set, Semicolon_set, err_identifier_missing);
      Enter (Id, TypeMark);
      T1 := T;
      InSymbol;
      if Sy = IS_Symbol then
        InSymbol;
      else
        Error (err_IS_missing);
      end if;
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      TYP (Semicolon_Comma_IDent + FSys, TP, RF, Sz);
      IdTab (T1).TYP := TP;
      IdTab (T1).Ref := RF;
      IdTab (T1).Adr := Sz;
      --
      Test_Semicolon;
    end Type_Declaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer);

    ------------------------------------------------------------------
    ---------------------------------------------------VarDeclaration-
    procedure VarDeclaration is               -- modified Hathorn
      -- This procedure processes both Variable and Constant declarations.
      T0, T1, RF, Sz, T0i, LC0, LC1 : Integer;
      TP                            : Types;
      ConstDec, TypeID,
      untyped_constant              : Boolean;
      C                             : ConRec;
      -- Y:          Item;
    begin
      while Sy = IDent loop
        T0 := T;
        EnterVariable;
        while Sy = Comma loop
          InSymbol;
          EnterVariable;
        end loop;

        if Sy = Colon then -- ':'
          InSymbol;
        else
          Error (err_colon_missing);
        end if;
        T1 := T;

        if Sy = IDent then  --MRC 6/91 from PC version
          I := Locate_identifier (Id);
        end if;

        Test (Type_Begin_Symbol + CONSTANT_Symbol, Semicolon_set, err_incorrectly_used_symbol);
        ConstDec := False;
        untyped_constant:= False;
        if Sy = CONSTANT_Symbol then
          ConstDec := True;
          InSymbol;
        end if;
        TypeID := False;
        if Type_Begin_Symbol (Sy) then -- Here, a type name or an anonymous type definition
          TypeID := True;
          TYP
           (Symset'(Semicolon  |
            Comma              |
            IDent              |
            Becomes            => True,
                    others => False) +
            FSys,
            TP,
            RF,
            Sz);
        else
          untyped_constant:= True;
        end if;
        Test
         (Symset'(Becomes | EQL | Semicolon => True, others => False),
          Empty_Symset,
          err_incorrectly_used_symbol);
        if Sy = EQL then
          Error (err_EQUALS_instead_of_BECOMES);
          Sy := Becomes;
        end if;
        if ConstDec then
          if Sy = Becomes then
            InSymbol;
            KKonstant
             (Semicolon_Comma_IDent +
              FSys,
              C);
          else
            Error (err_BECOMES_missing);
          end if;
        end if;
        T0i := T0;
        if ConstDec or TypeID then        -- update identifier table
          while T0 < T1 loop
            T0 := T0 + 1;
            declare
              r : TabEntry renames IdTab (T0);
            begin
              if ConstDec then
                r.Obj := Konstant;
                r.TYP := C.TP;
                case C.TP is
                  when Floats =>
                    EnterFloat (C.R);
                    r.Adr := C1;
                  when Ints =>
                    r.Adr := C.I;
                  when others =>
                    if untyped_constant then
                      Error (err_numeric_constant_expected);
                      -- "boo: constant:= True;" is wrong in Ada
                    end if;
                    r.Adr := C.I;
                end case;
              else
                r.TYP := TP;
                r.Ref := RF;
                r.Adr := Dx;
                Dx    := Dx + Sz;
              end if; -- ConstDec
            end;
          end loop; -- While T0 < T1
        end if;
        if not ConstDec and Sy = Becomes then
          -- create Variable initialization ObjCode
          LC0 := LC;
          Assignment (T1);
          T0 := T0i;
          while T0 < T1 - 1 loop
            T0 := T0 + 1;
            Emit2 (k_Load_Address, IdTab (T0).LEV, IdTab (T0).Adr);
            Emit2 (k_Push_Value, IdTab (T1).LEV, IdTab (T1).Adr);
            Emit (k_Store);
          end loop;

          LC1 := LC;
          -- reset ObjCode pointer as if ObjCode had not been generated
          LC := LC0;
          -- copy ObjCode to end of ObjCode table
          ICode := ICode + (LC1 - LC0);      -- Size of ObjCode
          while LC0 < LC1 loop
            ObjCode (CMax) := ObjCode (LC0);
            CMax           := CMax - 1;
            LC0            := LC0 + 1;
          end loop;

        end if;
        Test_Semicolon;
      end loop; -- While Sy = IDent

    end VarDeclaration;

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
      if IsFun then
        Enter (Id, Funktion);
      else
        Enter (Id, Prozedure);
      end if;
      InSymbol;
      Block (FSys, IsFun, False, Level + 1, T, IdTab(T).Name);
      if IsFun then
        Emit1 (k_Exit_Function, 1);
      else
        Emit1 (k_Exit_Call, CallSTDP);
      end if;
    end Proc_Func_Declaration;

    ------------------------------------------------------------------
    --------------------------------------------------TaskDeclaration-
    procedure TaskDeclaration is          -- Hathorn
      I, T0         : Integer;
      TaskID        : Alfa;
      saveLineCount : Integer;    -- Source line where Task appeared
    begin

      saveLineCount := Line_Count;

      InSymbol;
      if Sy = BODY_Symbol then     -- Task Body
        InSymbol;
        I      := Locate_identifier (Id);
        TaskID := IdTab (I).Name;
        BlockTab (IdTab (I).Ref).SrcFrom := saveLineCount;  --(* Manuel *)
        InSymbol;
        Block (FSys, False, False, Level + 1, I, TaskID);
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
          Fatal (TASKS_table_overflow);
        end if;
        Enter (TaskID, aTask);
        TaskDefTab (TCount) := T;
        EnterBlock (T);
        IdTab (T).Ref := B;
        InSymbol;
        if Sy = Semicolon then
          InSymbol;  -- Task with no entries
        else  -- Parsing the Entry specs
          if Sy = IS_Symbol then
            InSymbol;
          else
            Error (err_IS_missing);
          end if;
          if Level = LMax then
            Fatal (LEVEL_overflow);
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
              Fatal (ENTRIES_table_overflow);
            end if;
            Enter (Id, aEntry);
            EntryTab (ECount) := T;  --  point to identifier table location
            T0                := T;  --  of TaskID
            InSymbol;
            Block (FSys, False, False, Level + 1, T, IdTab(T).Name);
            IdTab (T0).Adr := TCount;
            if Sy = Semicolon then
              InSymbol;
            else
              Error (err_SEMICOLON_missing);
            end if;
          end loop; -- Sy = ENTRY_Symbol

          Level := Level - 1;
          TestEnd;
          if Sy = IDent and Id = TaskID then
            InSymbol;
          else
            Skip (Semicolon, err_incorrect_block_name);
          end if;
          Test_Semicolon;
        end if;
      end if;
    end TaskDeclaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Expression-
    procedure Expression (FSys : Symset; X : in out Item);

    ------------------------------------------------------------------
    ---------------------------------------------------------Selector-
    procedure Selector (FSys : Symset; V : in out Item) is
      X    : Item;
      a, J : Integer;
      err  : Error_code;
    begin      -- Sy IN [LParent, Period]
      loop
        if Sy = Period then
          InSymbol;                -- field Selector
          if Sy /= IDent then
            Error (err_identifier_missing);
          else
            if V.TYP /= Records then
              Error (err_var_with_field_selector_must_be_record);
            else  -- search field identifier
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

        else    -- array Selector
          if Sy /= LParent then
            Error (err_missing_an_opening_parenthesis);
          end if;
          loop
            InSymbol;
            Expression
             (FSys + Symset'(Comma | RParent => True, others => False),
              X);
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

          if Sy = RParent then
            InSymbol;
          else
            Error (err_closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
        end if;
        exit when not (Sy = LParent or else Sy = Period);
      end loop;

      if FSys = Semicolon_set then
        err := err_SEMICOLON_missing;
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
      if Sy = LParent then          -- actual parameter list
        loop

          InSymbol;
          if CP >= LastP then
            Error (err_number_of_parameters_do_not_match);
          else
            CP := CP + 1;
            if IdTab (CP).Normal then       -- value parameter
              Expression
               (FSys +
                Symset'((Comma | Colon | RParent => True, others => False)),
                X);
              if X.TYP = IdTab (CP).TYP then
                if X.Ref /= IdTab (CP).Ref then
                  Error (err_parameter_types_do_not_match);
                elsif X.TYP = Arrays then
                  Emit1 (k_Load_Block, ArraysTab (X.Ref).Size);
                elsif X.TYP = Records then
                  Emit1 (k_Load_Block, BlockTab (X.Ref).VSize);
                end if;
              elsif X.TYP = Ints and IdTab (CP).TYP = Floats then
                Emit1 (k_Integer_to_Float, 0);
              elsif X.TYP /= NOTYP then
                Error (err_parameter_types_do_not_match);
              end if;
            else              -- Variable (Name) parameter
              if Sy /= IDent then
                Error (err_identifier_missing);
              else
                K := Locate_identifier (Id);
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
                  if Sy = LParent or else Sy = Period then
                    Selector
                     (FSys +
                      Symset'((Comma    |
                      Colon             |
                      RParent           => True,
                               others => False)),
                      X);
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
          Test (Symset'(Comma | RParent => True, others => False), FSys,
            err_incorrectly_used_symbol
          );
          exit when Sy /= Comma;
        end loop;

        if Sy = RParent then
          InSymbol;
        else
          Error (err_closing_parenthesis_missing);
        end if;

      end if;
      if CP < LastP then -- too few actual parameters
        Error (err_number_of_parameters_do_not_match);
      end if;

      if CallType = CallSTDP then
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
      else
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
        Emit1 (k_Exit_Call, CallType); -- Return from Entry Call
      end if;

      if IdTab (I).LEV < Level then
        Emit2 (k_Update_Display_Vector, IdTab (I).LEV, Level);
      end if;
    end Call;

    ------------------------------------------------------------------
    --------------------------------------------------------EntryCall-
    procedure EntryCall (FSys : Symset; I, CallType : Integer) is -- Hathorn
      -- X:        Item;
      Addr, J : Integer;
    -- HoldCode:      Order;
    begin
      if Sy /= Period then
        Skip (Semicolon, err_incorrectly_used_symbol);
      else
        InSymbol;                  -- Task Entry Selector
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
    end EntryCall;

    ------------------------------------------------------------------
    -------------------------------------------------------ResultType-
    -- !! ResultType assumes there is an automatic conversion Ints -> Floats
    -- Pascal, not Ada!
    function ResultType (a, B : Types) return Types is
    begin
      if a > Floats or B > Floats then
        Error (err_illegal_type_for_arithmetic_expression);
        return NOTYP;
      elsif a = NOTYP or B = NOTYP then
        return NOTYP;
      elsif a = Ints then
        if B = Ints then
          return Ints;
        else
          Emit1 (k_Integer_to_Float, 1);
          return Floats;
        end if;
      else
        if B = Ints then
          Emit1 (k_Integer_to_Float, 0);
        end if;
        return Floats;
      end if;
    end ResultType;

    ------------------------------------------------------------------
    -------------------------------------------------------Expression-
    procedure Expression (FSys : Symset; X : in out Item) is
      --  Note: dynamic variables for Y have been used due to the
      --     constraints imposed upon local variables in recursion.
      type ItemPtr is access Item;  -- static > dynamic : SCHOENING
      procedure Dispose is new Ada.Unchecked_Deallocation (Item, ItemPtr);

      Y     : ItemPtr;
      OP    : KeyWSymbol;
      F     : Integer;
      OperZ : constant Symset :=
       Symset'((EQL | NEQ | LSS | LEQ | GTR | GEQ => True, others => False));

      procedure SimpleExpression (FSys : Symset; X : in out Item) is
        Y     : ItemPtr;
        OP    : KeyWSymbol;
        TermZ : constant Symset :=
          Symset'((Plus | MinUS | OR_Symbol | XOR_Symbol => True, others => False));

        procedure Term (FSys : Symset; X : in out Item) is
          Y  : ItemPtr;
          OP : KeyWSymbol;
          -- TS: TypSet;
          FactorZ : constant Symset :=
           Symset'(
             xTimes | Divide | MOD_Symbol | REM_Symbol | AND_Symbol => True,
             others => False
           );

          procedure Factor (FSys : Symset; X : in out Item) is
            I, F : Integer;
            err  : Error_code;

            procedure Standard_Function (Pseudo_Address : Integer) is
              TS : Typset;
              N  : Integer := Pseudo_Address;
            begin  -- STANDARD FUNCTION NO. N , N => 100 INDICATES
              -- a NILADIC FUNCTION.
              if N < 100 then
                if Sy = LParent then
                  InSymbol;
                else
                  Error (err_missing_an_opening_parenthesis);
                end if;
                if N < 17 or N = 19 then
                  Expression (FSys + RParent, X);
                  case N is

                    when 0 | 2 =>  -- abs, Sqr

                      TS            :=
                       Typset'((Ints | Floats => True, others => False));
                      IdTab (I).TYP := X.TYP;
                      if X.TYP = Floats then
                        N := N + 1;
                      end if;

                    -- Odd, Chr
                    when 4 | 5 =>
                      TS := Typset'((Ints => True, others => False));

                    -- Ord
                    when 6 =>
                      TS :=
                       Typset'(
                       (Ints    |
                        Bools   |
                        xChars  |
                        Enums   => True,
                        others => False));

                    -- Succ,  Pred
                    when 7 | 8 =>
                      TS            :=
                       Typset'(
                       (Ints    |
                        Bools   |
                        xChars  |
                        Enums   => True,
                        others => False));
                      IdTab (I).TYP := X.TYP;

                    -- Round,Trunc
                    when 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 =>
                      -- Sin,Cos,...
                      TS :=
                       Typset'((Ints | Floats => True, others => False));
                      if X.TYP = Ints then
                        Emit1 (k_Integer_to_Float, 0);
                      end if;

                    -- Random
                    when 19 =>
                      TS            :=
                       Typset'((Ints => True, others => False));
                      IdTab (I).TYP := X.TYP;

                    when others =>
                      null;

                  end case; -- N

                  if TS (X.TYP) then
                    Emit1 (k_Standard_Functions, N);
                  elsif X.TYP /= NOTYP then
                    Error (err_argument_to_std_function_of_wrong_type);
                  end if;

                else           -- N in [17,18]
                  -- EOF, Eoln
                  if Sy /= IDent then
                    Error (err_identifier_missing);
                  elsif Id(1..10) = "INPUT     " then
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
                end if;        -- N in [17,18]
                X.TYP := IdTab (I).TYP;
                if Sy = RParent then
                  InSymbol;
                else
                  Error (err_closing_parenthesis_missing);
                end if;
              else            -- NILADIC FUNCTION
                case N is
                  when 100 =>
                    Emit1 (k_Standard_Functions, N); -- CLOCK
                  when others =>
                    null;
                end case;
              end if;    -- NILADIC FUNCTIONS, N => 100
            end Standard_Function;

          begin  -- Factor
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
                  I := Locate_identifier (Id);
                  InSymbol;
                  exit when I = No_Id; -- Id. not found, error shown by Locate_identifier
                  declare
                    r : TabEntry renames IdTab (I);
                  begin
                    case r.Obj is
                      when Konstant =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                        if X.TYP = Floats then
                          Emit1 (k_Load_Float, r.Adr);
                        else
                          Emit1 (k_Literal, r.Adr);
                        end if;

                      when Variable =>
                        X.TYP := r.TYP;
                        X.Ref := r.Ref;
                        if Sy = LParent or Sy = Period then
                          if r.Normal then
                            F := k_Load_Address;
                          else
                            F := k_Push_Value;
                          end if;
                          Emit2 (F, r.LEV, r.Adr);
                          Selector (FSys, X);
                          if StanTyps (X.TYP) then
                            Emit (kCase34);
                          end if;
                        else
                          if X.TYP = Enums or StanTyps (X.TYP) then
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

                      when TypeMark =>
                        -- !! Float(integer_exp) Integer(float_exp) etc. here !!
                        Error (err_expected_variable_function_or_constant);

                      when Prozedure =>
                        Error (err_expected_variable_function_or_constant);

                      when Funktion =>
                        X.TYP := r.TYP;
                        if r.LEV /= 0 then
                          Call (FSys, I, CallSTDP);
                        else
                          Standard_Function (r.Adr);
                        end if;

                      when others =>
                        null;

                    end case;
                  end; -- WITH

                when CharCon | IntCon | FloatCon =>

                  if Sy = FloatCon then
                    X.TYP := Floats;
                    EnterFloat (RNum);
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

                when LParent =>    --  (
                  InSymbol;
                  Expression (FSys + RParent, X);
                  if Sy = RParent then
                    InSymbol;
                  else
                    Error (err_closing_parenthesis_missing);
                  end if;

                when NOT_Symbol =>
                  InSymbol;
                  Factor (FSys, X);
                  if X.TYP = Bools then
                    Emit (k_NOT_Boolean);
                  elsif X.TYP /= NOTYP then
                    Error (err_resulting_type_should_be_Boolean);
                  end if;

                when others =>
                  null;

              end case;

              if FSys = Semicolon_set then
                err := err_SEMICOLON_missing;
              else
                err := err_incorrectly_used_symbol;
              end if;
              Test (FSys, Factor_Begin_Symbol, err);
            end loop;
          end Factor;

        begin  -- Term
          Y := new Item;
          Factor (FSys + FactorZ, X);
          while FactorZ (Sy) loop
            OP := Sy;
            InSymbol;
            Factor (FSys + FactorZ, Y.all);
            if OP = xTimes then     --  *
              X.TYP := ResultType (X.TYP, Y.TYP);
              case X.TYP is
                when NOTYP =>
                  null;
                when Ints =>
                  Emit (k_MULT_Integer);
                when Floats =>
                  Emit (k_MULT_Float);
                when others =>
                  null;
              end case;
            elsif OP = Divide then    --  /
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_DIV_Integer);
              else
                if X.TYP = Ints then
                  Emit1 (k_Integer_to_Float, 1);
                  X.TYP := Floats;
                end if;
                if Y.TYP = Ints then
                  Emit1 (k_Integer_to_Float, 0);
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
            else            -- MOD  -  OP = MOD_Symbol
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_MOD_Integer);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (err_mod_requires_integer_arguments);
                end if;
                X.TYP := NOTYP;
              end if;
            end if;
          end loop;

          Dispose (Y);
        end Term;

      begin  -- SimpleExpression
        Y := new Item;

        -- +, -
        if Sy = Plus or else Sy = MinUS then
          OP := Sy;
          InSymbol;
          Term
           (FSys + Symset'((Plus | MinUS => True, others => False)),
            X);
          if X.TYP > Floats then
            Error (err_illegal_type_for_arithmetic_expression);
          elsif OP = MinUS then
            Emit (k_Unary_MINUS_Integer);
            -- !! obviously wrong when X.TYP = Floats
          end if;
        else
          Term (FSys + TermZ, X);
        end if;
        while TermZ (Sy) loop
          OP := Sy;
          InSymbol;
          Term (FSys + TermZ, Y.all);
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
              X.TYP := ResultType (X.TYP, Y.TYP);
              case X.TYP is
                when NOTYP =>
                  null;
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
                  null; -- !!
              end case;
            when others => -- Doesn't happen: TermZ(OP) is True.
              null;
          end case;
        end loop;

        Dispose (Y);
      end SimpleExpression;

    begin  -- Expression
      Y := new Item;
      SimpleExpression (FSys + OperZ, X);
      if OperZ (Sy) then
        OP := Sy;
        InSymbol;
        SimpleExpression (FSys, Y.all);
        if X.TYP = Ints and Y.TYP = Floats then
          X.TYP := Floats;
          Emit1 (k_Integer_to_Float, 1);
        end if;
        if Y.TYP = Ints and X.TYP = Floats then
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
      Dispose (Y);
    end Expression;

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
      if Sy = LBrack or Sy = LParent or Sy = Period then
        Selector
         (Symset'((Becomes | EQL => True, others => False)) + FSys,
          X);
      end if;
      if Sy = Becomes then
        InSymbol;
      elsif Sy = EQL then
        -- Common mistake by BASIC or C programmers.
        Error (err_EQUALS_instead_of_BECOMES);
        InSymbol;
      else
        Error (err_BECOMES_missing);
      end if;
      Expression (Semicolon_set, Y);
      if X.TYP = Y.TYP then
        if StanTyps (X.TYP) then
          Emit (k_Store);
        elsif X.Ref /= Y.Ref then
          Error (err_types_of_assignment_must_match);
        else
          case X.TYP is
            when Arrays =>
              Emit1 (k_Copy_Block, ArraysTab (X.Ref).Size);
            when Records =>
              Emit1 (k_Copy_Block, BlockTab (X.Ref).VSize);
            when Enums =>
              Emit (k_Store);
            when others =>
              null;
          end case;
        end if;
      elsif X.TYP = Floats and Y.TYP = Ints then
        Emit1 (k_Integer_to_Float, 0);
        Emit (k_Store);
      elsif X.TYP = Arrays and Y.TYP = Strings then
        if ArraysTab (X.Ref).Element_TYP /= xChars then
          Error (err_types_of_assignment_must_match);
        else
          Emit1 (k_String_assignment, ArraysTab (X.Ref).Size);    -- array Size
        end if;
      elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
        Error (err_types_of_assignment_must_match);
      end if;
    end Assignment;

    ------------------------------------------------------------------
    --------------------------------------------------------Statement--
    procedure Statement (FSys : Symset) is
      I : Integer;

      procedure MultiStatement (Sentinal : Symset) is   -- Hathorn
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
      end MultiStatement;

      procedure AcceptStatement is            -- Hathorn
        I : Integer;

        procedure AcceptCall (FSys : Symset; I : Integer) is
          pragma Unreferenced (I, FSys);
        begin -- check To make sure parameters match with Entry Statement
          if Sy = Semicolon then
            return; -- Exit(AcceptCall);
          end if;
          if Sy = LParent then          -- <--- temporary
            while not (Sy = DO_Symbol or Sy = RParent) loop
              InSymbol;
            end loop; -- !! should check no. and
          end if;    -- Types of parms.
          if Sy = RParent then
            InSymbol;
          end if;
        end AcceptCall;

      begin  -- AcceptStatement
        InSymbol;
        I := Locate_identifier (Id);
        if IdTab (I).Obj /= aEntry then
          Error (err_use_Small_Sp);
        end if;
        InSymbol;
        AcceptCall (FSys, I);
        Emit1 (k_Accept_Rendezvous, I);
        if Sy = DO_Symbol then
          if Level = LMax then
            Fatal (LEVEL_overflow);
          end if;
          Level           := Level + 1;
          Display (Level) := IdTab (I).Ref;
          InSymbol;
          MultiStatement (END_set);
          TestEnd;
          if Sy = IDent then
            if Id /= IdTab (I).Name then
              Error (err_incorrect_block_name);
            end if;
            InSymbol;
          end if;
          Level := Level - 1;
        end if;
        Emit1 (k_End_Rendezvous, I);
      end AcceptStatement;

      procedure Exit_Statement is
        -- Generate an absolute branch Statement with a dummy end loop address
        X : Item;
      begin
        if Sy = EXIT_Symbol then
          InSymbol;
        else
          Skip (Semicolon, err_incorrectly_used_symbol);
        end if;
        if Sy = WHEN_Symbol then      -- conditional Exit
          InSymbol;
          Expression (Semicolon_set, X);
          if not (X.TYP = Bools or X.TYP = NOTYP) then
            Error (err_expecting_a_boolean_expression);
          end if;
          Emit1 (k_Conditional_Jump, LC + 2);            -- conditional jump around Exit
        end if;
        Emit1 (k_Jump, dummy_address);  -- unconditional jump with dummy address to be patched
      end Exit_Statement;

      procedure IF_Statement is
        X        : Item;
        LC0, LC1 : Integer;
      begin
        InSymbol;
        Expression
         (FSys + Symset'((THEN_Symbol | DO_Symbol => True, others => False)),
          X);
        if not (X.TYP = Bools or else X.TYP = NOTYP) then
          Error (err_expecting_a_boolean_expression);
        end if;
        LC1 := LC;
        Emit (k_Conditional_Jump);                  -- JMPC
        if Sy = THEN_Symbol then
          InSymbol;
        else
          Error (err_THEN_missing);
          if Sy = DO_Symbol then
            InSymbol;
          end if;
        end if;
        MultiStatement
         (Symset'((ELSIF_Symbol | ELSE_Symbol | END_Symbol => True, others => False)));
        LC0 := LC;
        while Sy = ELSIF_Symbol loop     -- Added Hathorn
          InSymbol;
          Emit1 (k_Jump, dummy_address);    -- unconditional jump with dummy address to be patched
          ObjCode (LC1).Y := LC; -- patch the previous conditional jump
          Expression
           (FSys + Symset'((THEN_Symbol | DO_Symbol => True, others => False)),
            X);
          if not (X.TYP = Bools or else X.TYP = NOTYP) then
            Error (err_expecting_a_boolean_expression);
          end if;
          LC1 := LC;
          Emit (k_Conditional_Jump);                -- JMPC
          if Sy = THEN_Symbol then
            InSymbol;
          else
            Error (err_THEN_missing);
            if Sy = DO_Symbol then
              InSymbol;
            end if;
          end if;
          MultiStatement
           (Symset'((ELSIF_Symbol | ELSE_Symbol | END_Symbol => True, others => False)));
        end loop;

        if Sy = ELSE_Symbol then
          InSymbol;
          Emit1 (k_Jump, dummy_address);
          ObjCode (LC1).Y := LC;
          MultiStatement (END_set);
        else
          ObjCode (LC1).Y := LC;
        end if;
        if Sy = END_Symbol then -- END (IF)
          InSymbol;
        else
          Error (err_END_missing);
        end if;
        if Sy = IF_Symbol then -- (END) IF
          InSymbol;
        else
          Error (err_missing_closing_IF);
        end if;
        -- Go back and patch the dummy addresses in unconditional jumps
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
        MultiStatement (END_set);
        Emit1 (FCT, B);
        if Sy = END_Symbol then -- END (LOOP)
          InSymbol;
        else
          Error (err_END_missing);
        end if;
        if Sy = LOOP_Symbol then -- (END) LOOP
          InSymbol;
        else
          Error (err_closing_LOOP_missing);
        end if;

        -- Go back and patch the dummy addresses generated by Exit statements.
        while LC0 < LC loop
          if ObjCode (LC0).Y = dummy_address then
            ObjCode (LC0).Y := LC;
          end if;
          LC0 := LC0 + 1;
        end loop;
      end LOOP_Statement;

      procedure RETURN_Statement is           -- Hathorn
        -- Generate a procedure return Statement, calculate return value if
        --req'D
        X, Y : Item;
        F    : Integer;
      begin
        if BlockID = ProgramID then
          Error (err_illegal_return_statement_from_main); -- !! but... this is legal in Ada !!
        end if;
        I := Locate_identifier (BlockID);
        InSymbol;
        if Sy = Semicolon and Is_a_function then
          Error (err_functions_must_return_a_value);
        end if;
        if Sy /= Semicolon then          -- calculate return value
          if IdTab (I).Ref = Display (Level) then
            X.TYP := IdTab (I).TYP;
            X.Ref := IdTab (I).Ref;
            if IdTab (I).Normal then
              F := k_Load_Address;
            else
              F := k_Push_Value;
            end if;
            Emit2 (F, IdTab (I).LEV + 1, 0);
            Expression (Semicolon_set, Y);
            if X.TYP = Y.TYP then
              if StanTyps (X.TYP) then
                Emit (k_Store);
              elsif X.Ref /= Y.Ref then
                Error (err_types_of_assignment_must_match);
              elsif X.TYP = Floats and Y.TYP = Ints then
                Emit1 (k_Integer_to_Float, 0);
                Emit (k_Store);
              elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
                Error (err_types_of_assignment_must_match);
              end if;
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

      procedure DelayStatement is            -- Cramer
        -- Generate a Task delay
        Y : Item;
      begin
        InSymbol;
        if Sy = Semicolon then
          Skip (Semicolon, err_missing_expression_for_delay);
        else                  -- calculate delay value
          Expression (Semicolon_set, Y);
          if Y.TYP /= Floats then
            Error (err_wrong_type_in_DELAY);
          end if;
        end if;
        Emit (k_Delay);
      end DelayStatement;

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
          KKonstant
           (FSys + Symset'((Alt | Finger => True, others => False)),
            Lab);
          if Lab.TP /= X.TYP then
            Error (err_case_label_not_same_type_as_case_clause);
          elsif I = CSMax then
            Fatal (OBJECT_overflow);
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
            if ConstBegSys (Sy) then
              CASE_Label;
              while Sy = Alt loop
                InSymbol;
                CASE_Label;
              end loop;
            end if;
            if Sy = OTHERS_Symbol then        -- Hathorn
              if I = CSMax then
                Fatal (OBJECT_overflow);
              else
                I               := I + 1;
                CaseTab (I).Val := 0;
                CaseTab (I).LC  := -LC;
                InSymbol;
              end if;
            end if;
            if Sy = Finger then
              InSymbol;
            else
              Error (err_FINGER_missing);
            end if;
            MultiStatement (Symset'((WHEN_Symbol | END_Symbol => True, others => False)));
            J           := J + 1;
            ExitTab (J) := LC;
            Emit (k_Jump);
          else
            Error (err_WHEN_missing);
          end if;
        end One_CASE;

      begin -- CASE_Statement
        InSymbol;
        I := 0;
        J := 0;
        Expression
         (FSys +
          Symset'((OF_Symbol | IS_Symbol | Comma | Colon => True, others => False)),
          X);

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
          Error (err_OF_instead_of_IS); -- Common mistake by Pascal programmers
          InSymbol;
        else
          Error (err_IS_missing);
        end if;

        while Sy = WHEN_Symbol loop
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

        if Sy = END_Symbol then
          InSymbol;
        else
          Error (err_END_missing);
        end if;
        if Sy = CASE_Symbol then
          InSymbol;
        else
          Error (err_missing_closing_CASE);
        end if;
      end CASE_Statement;

      procedure WHILE_Statement is
        X        : Item;
        LC1, LC2 : Integer;
      begin
        InSymbol;
        LC1 := LC;
        Expression
         (FSys + Symset'((LOOP_Symbol | DO_Symbol => True, others => False)),
          X);
        if not (X.TYP = Bools or else X.TYP = NOTYP) then
          Error (err_expecting_a_boolean_expression);
        end if;
        LC2 := LC;
        Emit (k_Conditional_Jump);
        LOOP_Statement (k_Jump, LC1);
        ObjCode (LC2).Y := LC;
      end WHILE_Statement;

      ------------------------------------------------------------ForStatement
      procedure FOR_Statement is
        -- CVT:  Types;
        X            : Item;
        F, LC1, last : Integer;
      begin
        InSymbol;
        if Sy = IDent then
          if T = TMax then
            Fatal (IDENTIFIERS_table_overflow);
          else
            -- declare local loop control Variable  --  added Hathorn
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
          end if;
        else
          Skip
           (Symset'((
              IN_Symbol         |
              RANGE_Symbol           |
              LOOP_Symbol       |
              END_Symbol        => True,
              others => False)) +
            FSys,
            err_identifier_missing);
        end if;

        Emit2 (k_Load_Address, IdTab (T).LEV, IdTab (T).Adr);
        InSymbol;
        F := kFor1;
        if Sy = IN_Symbol then
          InSymbol;
          if Sy = REVERSE_Symbol then
            F := kFor1Rev;
            InSymbol;
          end if;
          Expression
           (Symset'((RANGE_Symbol | LOOP_Symbol | END_Symbol => True, others => False)) +
            FSys,
            X);
          IdTab (T).TYP := X.TYP;
          if not (X.TYP = Ints
                 or X.TYP = Bools
                 or X.TYP = xChars)
          then
            Error (err_control_variable_of_the_wrong_type);
          end if;
          if Sy = RANGE_Symbol then
            InSymbol;
            Expression (FSys + LOOP_Symbol, X);
            if IdTab (T).TYP /= X.TYP then
              Error (err_first_and_last_must_have_matching_types);
            end if;
          else
            Skip
             (Symset'((LOOP_Symbol | END_Symbol | Semicolon => True, others => False)) +
              FSys,
              err_expecting_dot_dot);
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

      procedure SelectStatement is
        procedure SelectError (N : Error_code) is
        begin
          Skip (Semicolon, N);
        end SelectError;

        -- Either a Timed or Conditional Entry Call.

        procedure QualifiedEntryCall is
          I, J, IStart, IEnd : Integer;
          patch              : array (0 .. 4) of Integer;
          O                  : Order;
          Y                  : Item;
        begin
          I := Locate_identifier (Id);
          if IdTab (I).Obj = aTask then
            InSymbol;
            EntryCall (FSys, I, -1);
            if ObjCode (LC - 2).F = kCall then     -- need To patch CallType later
              patch (0) := LC - 2;
            else
              patch (0) := LC - 3;
            end if;       -- LC-1 must be OP=3, update Display
            patch (1) := LC;           -- need To patch in JMPC address later
            Emit1 (k_Conditional_Jump, dummy_address);    -- JMPC, address patched in after ELSE
                                      --or OR
            if Sy = Semicolon then
              InSymbol;
            else
              Skip (Semicolon, err_SEMICOLON_missing);
            end if;
            if not (Sy = OR_Symbol or else Sy = ELSE_Symbol) then
              MultiStatement
               (Symset'((OR_Symbol | ELSE_Symbol => True, others => False)));
            end if;
            if Sy = OR_Symbol then -- =====================> Timed Entry Call
              ObjCode (patch (0)).X      := CallTMDE; -- Timed Entry Call
              ObjCode (patch (0) + 1).Y  := CallTMDE; -- Exit type matches
                                                      --Entry type
              InSymbol;
              if Sy = DELAY_Symbol then
                InSymbol;
                if Sy = Semicolon then
                  SelectError (err_missing_expression_for_delay);
                else          -- calculate delay value
                  patch (2) := LC;
                  Expression (Semicolon_set, Y);
                  patch (3) := LC - 1;
                  if Y.TYP /= Floats then
                    SelectError (err_wrong_type_in_DELAY);
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
                SelectError (err_expecting_DELAY);
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
              MultiStatement (END_set);
              ObjCode (patch (1)).Y  := patch (3);
              ObjCode (patch (2)).Y  := LC;
            end if;
            if Sy /= END_Symbol then
              SelectError (err_END_missing);
            end if;
            InSymbol;
            if Sy /= SELECT_Symbol then
              SelectError (err_SELECT_missing);
            end if;
          else
            SelectError (err_expecting_task_entry);
          end if;          -- Task.Entry Call expected
        end QualifiedEntryCall;

        procedure SelectiveWait is         -- Kurtz <===================
          -- Jay, this Buds for you !!

          type patch_ptr is array (1 .. 10) of Integer;

          JSD, Alt            : patch_ptr;
          ISD, IAlt, StartSel : Integer;
          SelectDone          : Boolean;
          Y, X                : Item;
          do_terminate        : Boolean;

          procedure AcceptStatement2 is      -- Kurtz
            I : Integer;

            procedure AcceptCall2 (FSys : Symset; I : Integer) is
            pragma Unreferenced (FSys, I);
            begin
              -- check To make sure parameters match with Entry Statement
              if Sy = Semicolon then
                return; -- Exit(AcceptCall2);
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
            end AcceptCall2;

          begin                -- AcceptStatment2
            InSymbol;
            I := Locate_identifier (Id);
            if IdTab (I).Obj /= aEntry then
              SelectError (err_use_Small_Sp);
            end if;
            InSymbol;
            AcceptCall2 (FSys, I);
            Emit2 (kSelectiveWait, 2, I);          -- Retain Entry Index
            if IAlt < 10 then
              IAlt := IAlt + 1;
            else
              Fatal (PATCHING_overflow);
            end if;
            Alt (IAlt) := LC;              -- SAVE LOCATION FOR PATCHING
            Emit2 (kSelectiveWait, 3, LC); -- ACCEPT IF Ready ELSE Skip To LC
            -- CONDITIONAL ACCEPT MUST BE ATOMIC
            if Sy = DO_Symbol then
              if Level = LMax then
                Fatal (LEVEL_overflow);
              end if;
              Level           := Level + 1;
              Display (Level) := IdTab (I).Ref;
              InSymbol;
              MultiStatement (END_set);
              TestEnd;
              if Sy = IDent then
                if Id /= IdTab (I).Name then
                  SelectError (err_incorrect_block_name);
                end if;
              end if;
              Level := Level - 1;
              InSymbol;
            end if;
            Emit1 (k_End_Rendezvous, I);
          end AcceptStatement2;

        begin          -- SelectiveWait ===============================> Kurtz
          ISD          := 0;
          IAlt         := 0;
          SelectDone   := False;
          do_terminate := False;
          StartSel     := LC;
          Emit2 (kSelectiveWait, 1, 0); -- START OF SELECT SELECTIVE Wait
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
                Expression (FSys + Finger, X);
                if not (X.TYP = Bools or X.TYP = NOTYP) then
                  SelectError (err_expecting_a_boolean_expression);
                end if;
                InSymbol;
                if Sy = ACCEPT_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING_overflow);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Conditional_Jump);
                    AcceptStatement2;
                  end if;
                elsif Sy = DELAY_Symbol then
                  if IAlt > 10 then
                    Fatal (PATCHING_overflow);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Conditional_Jump);
                    InSymbol;
                    Expression (FSys + Semicolon, Y);
                    Emit2 (kSelectiveWait, 4, LC + 2); -- Update delay time
                    if Y.TYP /= Floats then
                      SelectError (err_wrong_type_in_DELAY);
                    end if;
                    if IAlt > 10 then
                      Fatal (PATCHING_overflow);
                    end if;
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Jump);
                  end if;
                else
                  SelectError (err_missing_a_procedure_declaration);
                end if;
                InSymbol;
                MultiStatement
                 (Symset'((OR_Symbol | ELSE_Symbol | END_Symbol => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);          -- patch JMP ADDRESS AT EndSy
              -- end WHEN_Symbol

              when ACCEPT_Symbol =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                AcceptStatement2;
                InSymbol;
                MultiStatement
                 (Symset'((OR_Symbol | ELSE_Symbol | END_Symbol => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);

              when OR_Symbol => -- OR STATEMENT
                InSymbol;

              when ELSE_Symbol =>
                for I in 1 .. IAlt loop
                  -- patch ObjCode
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                IAlt := 0;
                InSymbol;
                MultiStatement (END_set);
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end ELSE_Symbol

              when DELAY_Symbol =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                -- Generate a Task delay, calculate return value if req'D
                InSymbol;
                if Sy = Semicolon then
                  Skip (Semicolon, err_missing_expression_for_delay);
                else          -- calculate return value
                  Expression (Semicolon_set, Y);
                  Emit2 (kSelectiveWait, 4, LC + 2);  -- Update delay time
                  if Y.TYP /= Floats then
                    SelectError (err_wrong_type_in_DELAY);
                  end if;
                  if IAlt > 10 then
                    Fatal (PATCHING_overflow);
                  end if;
                  IAlt       := IAlt + 1;
                  Alt (IAlt) := LC;
                  Emit (k_Jump);
                end if;
                InSymbol;
                MultiStatement
                 (Symset'((OR_Symbol | END_Symbol | ELSE_Symbol => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end DELAY_Symbol

              when TERMINATE_Symbol =>
                InSymbol;
                if Sy /= Semicolon then
                  SelectError (err_SEMICOLON_missing);
                end if;
                do_terminate := True;        -- Oguz
                InSymbol;

              when END_Symbol =>
                InSymbol;
                if Sy /= SELECT_Symbol then
                  SelectError (err_END_missing);
                end if;
                SelectDone := True;
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                if do_terminate then
                  Emit2 (kSelectiveWait, 5, StartSel);
                else
                  Emit2 (kSelectiveWait, 6, StartSel);
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

        end SelectiveWait;                    -- SelectiveWait

      begin                    -- Sy = SELECT_Symbol
        -- Next KeyWSymbol must be ACCEPT_Symbol, WHEN_Symbol, or a Task Entry object
        --Name.
        InSymbol;
        if Sy = ACCEPT_Symbol or Sy = WHEN_Symbol or Sy = IDent then
          if Sy = ACCEPT_Symbol or Sy = WHEN_Symbol then
            SelectiveWait;
          else
            QualifiedEntryCall;
          end if;         -- Timed or Conditional Entry Call
          InSymbol;
        else
          SelectError (err_expecting_accept_when_or_entry_id);
        end if;
      end SelectStatement;

      procedure StandProc (N : Integer) is
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
                  I := Locate_identifier (Id);
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
                      if Sy = LParent or Sy = Period then
                        Selector
                         (FSys +
                          Symset'((Comma | RParent => True, others => False)),
                          X);
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
                Test
                 (Symset'((Comma | RParent => True, others => False)),
                  FSys,
                  err_incorrectly_used_symbol);

                exit when Sy /= Comma;
              end loop;
              <<SKIP1b>>
              if Sy = RParent then
                InSymbol;
              else
                Error (err_closing_parenthesis_missing);
              end if;
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
                  Expression
                   (FSys +
                    Symset'((Comma    |
                    Colon             |
                    RParent           => True,
                             others => False)),
                    X);
                  if X.TYP = Enums then
                    X.TYP := Ints;
                  end if;
                  if (not StanTyps (X.TYP)) and X.TYP /= Strings then
                    Error (err_illegal_parameters_to_Put);
                  end if;
                  if Sy = Colon then
                    InSymbol;
                    Expression
                     (FSys +
                      Symset'((Comma    |
                      Colon             |
                      RParent           => True,
                               others => False)),
                      Y);
                    if Y.TYP /= Ints then
                      Error (err_parameter_must_be_Integer);
                    end if;
                    if Sy = Colon then -- ':' Pascal-ism (Write/WriteLn) !!
                      if X.TYP /= Floats then
                        Error (err_parameter_must_be_of_type_Float);
                      end if;
                      InSymbol;
                      Expression
                       (FSys +
                        Symset'((Comma | RParent => True, others => False)),
                        Y);
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
              if Sy = RParent then
                InSymbol;
              else
                Error (err_closing_parenthesis_missing);
              end if;
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
                I := Locate_identifier (Id);
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
                    if Sy = LParent or else Sy = Period then
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
              if Sy = RParent then
                InSymbol;
              else
                Error (err_closing_parenthesis_missing);
              end if;
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
              if Sy = RParent then
                InSymbol;
              else
                Error (err_closing_parenthesis_missing);
              end if;
            end if;  -- reset

          when 10 =>        -- CursorAt
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              begin
                InSymbol;
                Expression
                 (Symset'((Comma      |
                  LParent             |
                  RParent             |
                  Colon               |
                  Semicolon           => True,
                           others => False)),
                  X);
                if X.TYP /= Ints then
                  Skip (Semicolon, err_parameter_must_be_Integer);
                end if;
                if Sy /= Comma then
                  Skip (Semicolon, err_COMMA_missing);
                else
                  InSymbol;
                  Expression
                   (Symset'((Comma      |
                             LParent    |
                             RParent    |
                             Colon      |
                             Semicolon  => True,
                             others => False)),
                    X);
                  if X.TYP /= Ints then
                    Skip (Semicolon, err_parameter_must_be_Integer);
                  end if;
                  if Sy = Comma then
                    Skip (Semicolon, err_number_of_parameters_do_not_match);
                  elsif Sy /= RParent then
                    Skip (Semicolon, err_closing_parenthesis_missing);
                  else
                    Emit (kCursorAt);
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
              Expression (Singleton(RParent), X);
              if X.TYP /= Floats then
                Skip (Semicolon, err_parameter_must_be_of_type_Float);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (kSetQuatumTask);
                InSymbol;
              end if;
            end if;                -- Quantum

          when 12 =>                   -- Set Priority
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (Singleton(RParent), X);
              if X.TYP /= Ints then
                Skip (Semicolon, err_parameter_must_be_Integer);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (kSetTaskPriority);
                InSymbol;
              end if;
            end if;                -- Priority

          when 13 =>                   -- Set Priority Inheritance,INHERITP
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, err_missing_an_opening_parenthesis);
            else
              InSymbol;
              Expression (Singleton(RParent), X);
              if X.TYP /= Bools then
                Skip (Semicolon, err_parameter_must_be_of_type_Boolean);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, err_closing_parenthesis_missing);
              else
                Emit (kSetTaskPriorityInheritance);
                InSymbol;
              end if;
            end if;                -- Inheritp

          when others =>
            null;

        end case;
      end StandProc;

      procedure Block_statement(block_name: Alfa) is  -- RM: 5.6
      begin
        Block (FSys, Is_a_function, True, Level + 1, T, block_name);
        -- !! to check: * stack management of variables when entering / quitting the block
        -- !! * object code and nesting... works on some cases at least (test.adb) !...
        -- !! Perhaps keep same level but have local declarations as for the
        --    variable in a FOR_Statement.
        -- !! Local bodies of subprograms surely mess the object code.
      end Block_statement;

      procedure Named_statement is -- block_statement or loop
        new_label: constant Alfa:= Id;
      begin
        Enter (new_label, Label);
        Test (Singleton(Colon), FSys, err_colon_missing);
        InSymbol;
        case Sy is
          when BEGIN_Symbol | DECLARE_Symbol => -- Named block_statement
            Block_statement(new_label);
          when LOOP_Symbol | FOR_Symbol | WHILE_Symbol =>
            null; -- !! should check label after end loop
          when others =>
            null;
        end case;
      end Named_statement;

    begin  -- Statement
      if Err_Count > 0 then  --{MRC: added from PC version}
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
          I := Locate_identifier (Id, No_Id_Fail => False);
          InSymbol;
          if I = No_Id then -- New identifier: must be a label for named block_statement or loop
            Named_statement;
          else
            case IdTab (I).Obj is
              when Konstant | TypeMark | Funktion =>
                Error (err_illegal_statement_start_symbol);
                Assignment (I);
              when Variable =>
                Assignment (I);
              when aTask =>
                EntryCall (FSys, I, CallSTDE);
              when Prozedure =>
                if IdTab (I).LEV /= 0 then
                  Call (FSys, I, CallSTDP);
                else
                  StandProc (IdTab (I).Adr);
                end if;
              when Label =>
                Error (err_duplicate_label, Alfa_to_String(Id));
                Test (Singleton(Colon), FSys, err_colon_missing);
                InSymbol;
              when others =>
                null;
            end case;
          end if; -- end IDent
        when ACCEPT_Symbol =>
          AcceptStatement;
        when BEGIN_Symbol | DECLARE_Symbol => -- Anonymous block_statement
          Block_statement(Empty_Alfa);
        when CASE_Symbol =>
          CASE_Statement;
        when DELAY_Symbol =>
          DelayStatement;
        when EXIT_Symbol =>
          Exit_Statement;
        when FOR_Symbol =>
          FOR_Statement;
        when IF_Symbol =>
          IF_Statement;
        when LOOP_Symbol =>
          LOOP_Statement (k_Jump, LC);
        when NULL_Symbol =>
          InSymbol;
        when RETURN_Symbol =>
          RETURN_Statement;
        when SELECT_Symbol =>
          SelectStatement;
        when WHILE_Symbol =>
          WHILE_Statement;
        when others =>
          null;
        end case;

        if not (EofInput) then      --{MRC: added IF NOT... from PC version}
          if Sy = Semicolon then
            InSymbol;
          else
            Error (err_SEMICOLON_missing);
          end if;
        end if;
      end if;  -- Sy in Statement_Begin_Symbol

      if not EofInput then
        Test (FSys - Semicolon, Semicolon_set, err_incorrectly_used_symbol);
      end if;

    end Statement;

  begin  -- Block
    if Err_Count > 0 then --{MRC, from PC source}
      return;
    end if;

    Dx      := 5;
    ICode   := 0;
    if Level > LMax then
      Fatal (LEVEL_overflow);
      return;            --{MRC, from PC source}
    end if;

    if Is_a_block_statement then
      null; -- we should be here with Sy = BEGIN_Symbol or Sy = DECLARE_Symbol.
    else
      Test
       (Symset'(LParent | RETURN_Symbol | IS_Symbol | Semicolon => True, others => False),
        FSys,
        err_incorrectly_used_symbol);
    end if;
    if IdTab (Prt).Ref > 0 then
      PRB := IdTab (Prt).Ref;
    else
      EnterBlock (Prt);
      PRB             := B;
      IdTab (Prt).Ref := PRB;
    end if;
    Display (Level) := PRB;
    IdTab (Prt).TYP := NOTYP;
    if Sy = LParent and Level > 1 then
      ParameterList;
    end if;

    if Err_Count > 0 then
      return;
    end if;    --{MRC, from PC source}

    BlockTab (PRB).LastPar := T;
    BlockTab (PRB).PSize   := Dx;
    if Is_a_function and not Is_a_block_statement then
      if Sy = RETURN_Symbol then
        InSymbol;  -- FUNCTION TYPE
        if Sy = IDent then
          I := Locate_identifier (Id);
          InSymbol;
          if I /= 0 then
            if IdTab (I).Obj /= TypeMark then
              Error (err_missing_a_type_identifier);
              return;  --{MRC, from PC source}
            elsif StanTyps (IdTab (I).TYP) then
              IdTab (Prt).TYP := IdTab (I).TYP;
            else
              Error (err_bad_result_type_for_a_function);
              return;    --{MRC, from PC source}
            end if;
          end if;
        else
          Skip (FSys + Semicolon, err_identifier_missing);
        end if;
      else
        Error (err_RETURN_missing);
        return;  --{MRC, from PC source}
      end if;
    end if;

    if Sy = Semicolon then  -- end of specification part
      BlockTab (PRB).VSize := Dx;
      IdTab (Prt).Adr      := -1;    -- address of body TBD
      return; -- Exit(Block);
    end if;

    if Is_a_block_statement then
      case Sy is
        when DECLARE_Symbol => InSymbol;
        when BEGIN_Symbol   => null;
        when others         => raise Internal_error with "Unexpected " & KeyWSymbol'Image(Sy);
      end case;
    else
      if Sy = IS_Symbol then
        InSymbol;
      else
        Error (err_IS_missing);
        return;
      end if;
    end if;

    ----------------------
    -- Declarative_part --
    ----------------------

    loop
      if Sy = IDent then
        VarDeclaration;
      end if;
      if Sy = TYPE_Symbol then
        Type_Declaration;
      end if;
      if Sy = TASK_Symbol then
        TaskDeclaration;
      end if;
      BlockTab (PRB).VSize := Dx;

      while Sy = PROCEDURE_Symbol or Sy = FUNCTION_Symbol loop
        Proc_Func_Declaration;
      end loop;  -- !! loop seems useless (a ghost of the Pascal compiler)...

      exit when Sy = BEGIN_Symbol;
    end loop;

    -----------------------------
    -- Statements part : setup --
    -----------------------------

    MaxDX           := Dx;
    IdTab (Prt).Adr := LC;
    -- Copy initialization (elaboration) ObjCode from end of ObjCode table
    I := CMax + ICode;
    while I > CMax loop
      ObjCode (LC) := ObjCode (I);
      LC           := LC + 1;
      I            := I - 1;
    end loop;
    CMax := CMax + ICode; -- Restore CMax to the initial max (=CDMax)
    InSymbol;

    ------------------------------------------
    -- Statements part : list of statements --
    ------------------------------------------
    if Sy = END_Symbol then -- GdM 15-Aug-2014: there should be at least one statement.
      Error (err_statement_expected);
    end if;
    loop
      Statement (FSys + END_Symbol);
      if Err_Count > 0 then  --{MRC, from PC source}
        Sy := END_Symbol;
      end if;
      exit when Sy = END_Symbol or Err_Count > 0;
    end loop;
    --{MRC, added OR()... from PC source}
    BlockTab (PRB).SrcTo := Line_Count;

    if Sy = END_Symbol then
      InSymbol;
    else
      Error (err_END_missing);
      return;
    end if;

    if Sy = IDent then -- Verify that the name after "end" matches the unit name.
      if Id /= BlockID then
        Error (err_incorrect_block_name, hint => Alfa_to_String(BlockID));
      end if;
      InSymbol;
    elsif Is_a_block_statement and BlockID /= Empty_Alfa then  -- end [label] required
      Error (err_incorrect_block_name, hint => Alfa_to_String(BlockID));
    end if;

    if Sy /= Semicolon then
      Error (err_SEMICOLON_missing);
      return;
    end if;

    if BlockID /= ProgramID and not Is_a_block_statement then
      InSymbol;
      Test (FSys, Empty_Symset, err_incorrectly_used_symbol);
    end if;

  end Block;

end HAC.Parser;
