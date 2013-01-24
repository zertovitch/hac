with HAC.PCode;              use HAC.PCode;
with HAC.Scanner;            use HAC.Scanner;
with HAC.UErrors;            use HAC.UErrors;
with Unchecked_Deallocation;

package body HAC.Parser is

  Semicolon_set : constant Symset :=
   Symset'(Semicolon => True, others => False);

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block
   (FSys    : Symset;
    IsFun   : Boolean;
    Level_A : Integer;
    Prt     : Integer)
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
    BlockID : Alfa;    -- Name of the current Block

    ------------------------------------------------------------------
    -------------------------------------------------------EnterArray-

    procedure EnterArray (TP : Types; L, H : Integer) is
      Lz, Hz : Integer;
    begin
      if L > H then
        Error (illegal_array_bounds); -- !! legal in Ada (empty array) !!
      end if;
      Lz := L;
      Hz := H;
      if abs (L) > XMax or abs (H) > XMax then
        Error (illegal_array_bounds);
        Lz := 0;
        Hz := 0;
      end if;
      if A = AMax then
        Fatal (4);     -- array table has overflowed
      else
        A := A + 1;
        declare
          r : ATabEntry renames ArraysTab (A);
        begin
          r.InXTYP := TP;
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
        BlockTab (B).SrcFrom := LineCount;
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
    procedure Skip (FSys : Symset; N : Integer) is

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

    procedure Skip (S : KeyWSymbol; N : Integer) is
      to_skip : Symset := Symset'(others => False);
    begin
      to_skip (S) := True;
      Skip (to_skip, N);
    end Skip;

    ------------------------------------------------------------------
    -------------------------------------------------------------Test-
    procedure Test (S1, S2 : Symset; N : Integer) is
    begin
      if not S1 (Sy) then
        Skip (S1 + S2, N);
      end if;
    end Test;

    ------------------------------------------------------------------
    ----------------------------------------------------TestSemicolon-
    procedure TestSemicolon is
      comma_or_colon : constant Symset :=
       Symset'(Comma | Colon => True, others => False);
    begin
      if Sy = Semicolon then
        InSymbol;
      else
        Error (semicolon_missing);
        if comma_or_colon (Sy) then
          InSymbol;
        end if;
      end if;
      Test
       (Symset'((IDent | TypeSy | TaskSy => True, others => False)) +
        BlockBegSyS,
        FSys,
        6);
    end TestSemicolon;

    ------------------------------------------------------------------
    ----------------------------------------------------------TestEnd-
    procedure TestEnd is            -- Hathorn
    begin
      if Sy = EndSy then
        InSymbol;
      else
        Skip (Semicolon, 57);
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
        -- follow the chain of identifiers for
        -- current Level.
        if J /= 0 then
          Error (duplicate_identifier);
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
          BlockTab (Display (Level)).Last  := T;  -- update start of identifer
                                                  --chain
        end if;
      end if;
    end Enter;

    ------------------------------------------------------------------
    --------------------------------------------------------------LOC-
    function LOC (Id : Alfa) return Integer is
      L, J : Integer;        -- locate identifier, Id, in table
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
      if J = 0 then
        Error (undefined_identifier);
      end if;
      return J;
    end LOC;

    ------------------------------------------------------------------
    ------------------------------------------------------------GetFP-
    function GetFP (Id : Alfa) return Integer is    -- Schoening
      ResultGetFP : Integer;
    begin   -- locate Id in FileIOTab
      ResultGetFP := -1;
      for I in 1 .. FileIOTab.Kount loop
        if FileIOTab.Nam (I) (2) = ':' then
          if FileIOTab.Nam (I) (3 .. FileIOTab.LNam (I) - 2) =
             Id (1 .. FileIOTab.LNam (I) - 2)
          then
            ResultGetFP := I;
          end if;
        elsif FileIOTab.Nam (I) = Id (1 .. FileIOTab.LNam (I)) then
          ResultGetFP := I;
        end if;
      end loop;

      return ResultGetFP;

    end GetFP;

    ------------------------------------------------------------------
    ----------------------------------------------------EnterVariable-
    procedure EnterVariable is
    begin
      if Sy = IDent then
        Enter (Id, Variable);
        InSymbol;
      else
        Error (identifier_missing);
      end if;
    end EnterVariable;

    ------------------------------------------------------------------
    ---------------------------------------------------------Constant-
    procedure KKonstant (FSys : Symset; C : in out ConRec) is
      X, Sign : Integer;
    begin
      C.TP := NOTYP;
      C.I  := 0;
      Test (ConstBegSys, FSys, 50);
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
            X := LOC (Id);
            if X /= 0 then
              if IdTab (X).Obj /= Konstant then
                Error (25);
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
            Skip (FSys, 50);
          end if;
        end if;
        Test (FSys, Empty_Symset, 6);
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

      FSys_gnagna : constant Symset :=
       FSys -
       Symset'(Semicolon | Comma | IDent => True, others => False) +
       Symset'(EndSy => True, others => False);

      procedure ArrayTyp (ARef, Arsz : in out Integer; StrAr : Boolean) is
        ELTP       : Types;
        Low, High  : ConRec;
        ELRF, ELSZ : Integer;
      begin
        KKonstant
         (Symset'((RangeSy | RParent | OFSy => True, others => False)) +
          FSys,
          Low);

        if Low.TP = Floats then
          Error (illegal_array_bounds);
          Low.TP := Ints;
          Low.I  := 0;
        end if;
        if Sy = RangeSy then
          InSymbol;
        else
          Error (13);
        end if;

        KKonstant
         (Symset'(Comma | RParent | OFSy => True, others => False) + FSys,
          High);

        if High.TP /= Low.TP then
          Error (illegal_array_bounds);
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
            Error (closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
        elsif Sy = Comma then
          InSymbol;
          ELTP := Arrays;
          ArrayTyp (ELRF, ELSZ, StrAr);
        else
          if Sy = RParent then
            InSymbol;
          else
            Error (closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
          if Sy = OFSy then
            InSymbol;
          else
            Error (8);
          end if;
          TYP (FSys, ELTP, ELRF, ELSZ);
        end if;
        declare
          r : ATabEntry renames ArraysTab (ARef);
        begin
          Arsz     := (High.I - Low.I + 1) * ELSZ;
          r.Size   := Arsz;
          r.ELTYP  := ELTP;
          r.ELREF  := ELRF;
          r.ELSize := ELSZ;
        end;
      end ArrayTyp;

    begin  -- Type
      TP := NOTYP;
      RF := 0;
      Sz := 0;
      Test (TypeBegSys, FSys, 10);
      if TypeBegSys (Sy) then
        if (Id = "STRING    ") then
          Sy := StringSy;
        end if;
        case Sy is
          when IDent =>
            I := LOC (Id);     -- IDent
            if I /= 0 then
              declare
                r : TabEntry renames IdTab (I);
              begin
                if r.Obj /= TypeMark then
                  Error (29);
                else
                  TP := r.TYP;
                  RF := r.Ref;
                  Sz := r.Adr;
                  if TP = NOTYP then
                    Error (30);
                  end if;
                end if;
              end;
            end if;
            InSymbol;

          when ArraySy | StringSy => -- ArraySy or StringSy
            StrArray := Sy = StringSy;
            InSymbol;
            if Sy = LParent then
              InSymbol;
            else
              Error (9);
              if Sy = LBrack then
                InSymbol;
              end if;
            end if;
            TP := Arrays;
            ArrayTyp (RF, Sz, StrArray);

          when RecordSy =>

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
                  Error (colon_missing);
                end if;
                T1 := T;
                TYP
                 (FSys +
                  Symset'(Semicolon  |
                  EndSy              |
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
              if Sy /= EndSy then
                if Sy = Semicolon then
                  InSymbol;
                else
                  Error (semicolon_missing);
                  if Sy = Comma then
                    InSymbol;
                  end if;
                end if;

                Test
                 (Symset'(IDent | EndSy | Semicolon => True, others => False),
                  FSys,
                  6);
              end if;
            end loop;

            BlockTab (RF).VSize := Offset;
            Sz                  := Offset;
            BlockTab (RF).PSize := 0;
            InSymbol;
            if Sy = RecordSy then
              InSymbol;
            else
              Error (61);
            end if;
            Level := Level - 1;
          -- end of RecordSy

          when LParent =>    -- Enumeration Type
            -- Hathorn
            TP     := Enums;
            RF     := T;
            ECount := 0;
            loop
              InSymbol;
              if (Sy = IDent) then
                ECount := ECount + 1;
                Enter (Id, Konstant);
                IdTab (T).TYP := Enums;
                IdTab (T).Ref := RF;
                IdTab (T).Adr := ECount;
              else
                Error (6);
              end if;
              InSymbol;
              exit when Sy /= Comma;
            end loop;

            Sz := ECount;
            if Sy = RParent then
              InSymbol;
            else
              Skip (Semicolon, 4);
            end if;
          -- end of Enumeration Type

          when others =>
            null;

        end case; -- Sy
        Test (FSys, Empty_Symset, 6);
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
      Test (Symset'(IDent => True, others => False), FSys + RParent, 7);
      while Sy = IDent loop
        T0 := T;
        EnterVariable;
        while Sy = Comma loop
          InSymbol;
          EnterVariable;
        end loop;

        if Sy = Colon then
          InSymbol;
          if Sy = InSy then
            InSymbol;
          end if;
          if IsFun then -- if I am a function, no InOut parms allowed
            ValParam := True;
          elsif Sy /= OutSy then
            ValParam := True;
          else
            InSymbol;
            ValParam := False;
          end if;
          if Sy /= IDent then
            Error (identifier_missing);
          else
            X := LOC (Id);
            InSymbol;
            if X /= 0 then
              declare
                r : TabEntry renames IdTab (X);
              begin
                if r.Obj /= TypeMark then
                  Error (29);
                else
                  TP := r.TYP;
                  RF := r.Ref;
                  if ValParam then
                    Sz := r.Adr;
                  else
                    Sz := 1;
                  end if;
                end if;
              end;
            end if; -- X /= 0
          end if;
          Test
           (Symset'(Semicolon  |
            RParent            |
            Comma              |
            IDent              => True,
                    others => False),
            FSys,
            14);

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

        else  -- if Sy /= Colon
          Error (colon_missing);
        end if;
        if Sy /= RParent then
          if Sy = Semicolon then
            InSymbol;
          else
            Error (semicolon_missing);
            if Sy = Comma then
              InSymbol;
            end if;
          end if;
          Test (Symset'(IDent => True, others => False), FSys + RParent, 6);
        end if;
      end loop;  -- while Sy = IDent
      if Sy = RParent then
        InSymbol;
        Test
         (Symset'(IsSy | ReturnSy | Semicolon => True, others => False),
          FSys,
          6);
      else
        Error (closing_parenthesis_missing);
      end if;
    end ParameterList;

    ------------------------------------------------------------------
    --------------------------------------------------TypeDeclaration-
    procedure TypeDeclaration is
      TP         : Types;
      RF, Sz, T1 : Integer;
    begin
      InSymbol;
      Test (Symset'(IDent => True, others => False), Semicolon_set, 2);
      Enter (Id, TypeMark);
      T1 := T;
      InSymbol;
      if Sy = IsSy then
        InSymbol;
      else
        Error (IS_missing);
      end if;

      TP := NOTYP;
      RF := 0;
      Sz := 0;
      TYP
       (Symset'(Semicolon | Comma | IDent => True, others => False) + FSys,
        TP,
        RF,
        Sz);
      IdTab (T1).TYP := TP;
      IdTab (T1).Ref := RF;
      IdTab (T1).Adr := Sz;
      --
      TestSemicolon;
    end TypeDeclaration;

    ------------------------------------------------------------------
    -------------------------------------------------------Assignment-
    procedure Assignment (I : Integer);

    ------------------------------------------------------------------
    ---------------------------------------------------VarDeclaration-
    procedure VarDeclaration is               -- modified Hathorn
      -- This procedure processes both Variable and Constant declarations.
      T0, T1, RF, Sz, T0i, LC0, LC1 : Integer;
      TP                            : Types;
      ConstDec, TypeID              : Boolean;
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
          Error (colon_missing);
        end if;
        T1 := T;

        if Sy = IDent then  --MRC 6/91 from PC version
          I := LOC (Id);
        end if;

        Test (TypeBegSys + ConstSy, Semicolon_set, 6);
        ConstDec := False;
        if Sy = ConstSy then
          ConstDec := True;
          InSymbol;
        end if;
        TypeID := False;
        if TypeBegSys (Sy) then
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
        end if;
        Test
         (Symset'(Becomes | EQL | Semicolon => True, others => False),
          Empty_Symset,
          6);
        if Sy = EQL then
          Error (EQUALS_instead_of_BECOMES);
          Sy := Becomes;
        end if;
        if ConstDec then
          if Sy = Becomes then
            InSymbol;
            KKonstant
             (Symset'(Semicolon | Comma | IDent => True, others => False) +
              FSys,
              C);
          else
            Error (BECOMES_missing);
          end if;
        end if;
        T0i := T0;
        if ConstDec or TypeID then        -- update identifier table
          while T0 < T1 loop
            T0 := T0 + 1;
            declare
              r : TabEntry renames IdTab (T0);
            begin
              case ConstDec is
                when True =>
                  r.Obj := Konstant;
                  r.TYP := C.TP;
                  if C.TP /= Floats then
                    r.Adr := C.I;
                  else
                    EnterFloat (C.R);
                    r.Adr := C1;
                  end if;
                when False =>
                  r.TYP := TP;
                  r.Ref := RF;
                  r.Adr := Dx;
                  Dx    := Dx + Sz;
              end case; -- ConstDec
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
            Emit (kStore);
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
        TestSemicolon;
      end loop; -- While Sy = IDent

    end VarDeclaration;

    ------------------------------------------------------------------
    --------------------------------------------------ProcDeclaration-
    procedure ProcDeclaration is
      IsFun      : Boolean;
    begin
      IsFun := Sy = FuncSy;
      InSymbol;
      if Sy /= IDent then
        Error (identifier_missing);
        Id := Empty_Alfa;
      end if;
      if IsFun then
        Enter (Id, Funktion);
      else
        Enter (Id, Prozedure);
      end if;
      InSymbol;
      Block (FSys, IsFun, Level + 1, T);
      if IsFun then
        Emit1 (kExitFunction, 1);
      else
        Emit1 (kExitCall, CallSTDP);
      end if;
    end ProcDeclaration;

    ------------------------------------------------------------------
    --------------------------------------------------TaskDeclaration-
    procedure TaskDeclaration is          -- Hathorn
      I, T0         : Integer;
      TaskID        : Alfa;
      saveLineCount : Integer;    -- Source line where Task appeared
    begin

      saveLineCount := LineCount;

      InSymbol;
      if Sy = BodySy then          -- Task Body
        InSymbol;
        I      := LOC (Id);
        TaskID := IdTab (I).Name;

        BlockTab (IdTab (I).Ref).SrcFrom := saveLineCount;  --(* Manuel *)

        InSymbol;
        Block (FSys, False, Level + 1, I);
        Emit1 (kExitCall, CallSTDP);
      else                    -- Task Specification
        if Sy = IDent then
          TaskID := Id;
        else
          Error (identifier_missing);
          Id := Empty_Alfa;
        end if;
        TCount := TCount + 1;
        if TCount > TaskMax then
          Fatal (8);
        end if;
        Enter (TaskID, aTask);
        TaskDefTab (TCount) := T;
        EnterBlock (T);
        IdTab (T).Ref := B;
        InSymbol;
        if Sy = Semicolon then
          InSymbol;  -- Task with no entries
        else  -- Parsing the Entry specs
          if Sy = IsSy then
            InSymbol;
          else
            Error (IS_missing);
          end if;
          if Level = LMax then
            Fatal (LEVEL_overflow);
          end if;
          Level           := Level + 1;
          Display (Level) := B;
          while Sy = EntrySy loop
            InSymbol;
            if Sy /= IDent then
              Error (identifier_missing);
              Id := Empty_Alfa;
            end if;
            ECount := ECount + 1;
            if ECount > EntryMax then
              Fatal (9);
            end if;
            Enter (Id, aEntry);
            EntryTAB (ECount) := T;         -- point to identifier table
                                            --location
            T0                := T;              -- of TaskID
            InSymbol;
            Block (FSys, False, Level + 1, T);
            IdTab (T0).Adr := TCount;
            if Sy = Semicolon then
              InSymbol;
            else
              Error (semicolon_missing);
            end if;
          end loop; -- Sy = EntrySy

          Level := Level - 1;
          TestEnd;
          if Sy = IDent and Id = TaskID then
            InSymbol;
          else
            Skip (Semicolon, 22);
          end if;
          TestSemicolon;
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
    begin      -- Sy IN [LParent, Period]
      loop
        if Sy = Period then
          InSymbol;                -- field Selector
          if Sy /= IDent then
            Error (identifier_missing);
          else
            if V.TYP /= Records then
              Error (31);
            else  -- search field identifier
              J              := BlockTab (V.Ref).Last;
              IdTab (0).Name := Id;
              while IdTab (J).Name /= Id loop
                J := IdTab (J).Link;
              end loop;
              if J = 0 then
                Error (undefined_identifier);
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
            Error (9);
          end if;
          loop
            InSymbol;
            Expression
             (FSys + Symset'(Comma | RParent => True, others => False),
              X);
            if V.TYP /= Arrays then
              Error (28);
            else
              a := V.Ref;
              if ArraysTab (a).InXTYP /= X.TYP then
                Error (26);
              elsif ArraysTab (a).ELSize = 1 then
                Emit1 (kIndex1, a);
              else
                Emit1 (kIndex, a);
              end if;
              V.TYP := ArraysTab (a).ELTYP;
              V.Ref := ArraysTab (a).ELREF;
            end if;
            exit when Sy /= Comma;
          end loop;

          if Sy = RParent then
            InSymbol;
          else
            Error (closing_parenthesis_missing);
            if Sy = RBrack then
              InSymbol;
            end if;
          end if;
        end if;
        exit when not (Sy = LParent or else Sy = Period);
      end loop;

      if FSys = Semicolon_set then
        J := 14;
      else
        J := 6;
      end if;

      Test (FSys, Empty_Symset, J);
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
            Error (39);
          else
            CP := CP + 1;
            if IdTab (CP).Normal then       -- value parameter
              Expression
               (FSys +
                Symset'((Comma | Colon | RParent => True, others => False)),
                X);
              if X.TYP = IdTab (CP).TYP then
                if X.Ref /= IdTab (CP).Ref then
                  Error (36);
                elsif X.TYP = Arrays then
                  Emit1 (kLoadBlock, ArraysTab (X.Ref).Size);
                elsif X.TYP = Records then
                  Emit1 (kLoadBlock, BlockTab (X.Ref).VSize);
                end if;
              elsif X.TYP = Ints and IdTab (CP).TYP = Floats then
                Emit1 (kCase26, 0);
              elsif X.TYP /= NOTYP then
                Error (36);
              end if;
            else              -- Variable (Name) parameter
              if Sy /= IDent then
                Error (identifier_missing);
              else
                K := LOC (Id);
                InSymbol;
                if K /= 0 then
                  if IdTab (K).Obj /= Variable then
                    Error (variable_missing);
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
                    Error (36);
                  end if;
                end if;
              end if;
            end if;
          end if;
          Test (Symset'(Comma | RParent => True, others => False), FSys, 6);
          exit when Sy /= Comma;
        end loop;

        if Sy = RParent then
          InSymbol;
        else
          Error (closing_parenthesis_missing);
        end if;

      end if;
      if CP < LastP then -- too few actual parameters
        Error (39);
      end if;

      if CallType = CallSTDP then
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
      else
        Emit2 (kCall, CallType, BlockTab (IdTab (I).Ref).PSize - 1);
        Emit1 (kExitCall, CallType); -- Return from Entry Call
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
        Skip (Semicolon, 6);
      else
        InSymbol;                  -- Task Entry Selector
        if Sy /= IDent then
          Skip (Semicolon, 2);
        else
          J              := BlockTab (IdTab (I).Ref).Last;
          IdTab (0).Name := Id;
          while IdTab (J).Name /= Id loop
            J := IdTab (J).Link;
          end loop;

          if J = 0 then
            Error (undefined_identifier);
          end if;

          Addr := J;
          InSymbol;
          Call (FSys, Addr, CallType);
        end if;
      end if;
    end EntryCall;

    ------------------------------------------------------------------
    -------------------------------------------------------ResultType-
    function ResultType (a, B : Types) return Types is
    begin
      if a > Floats or B > Floats then
        Error (33);
        return NOTYP;
      elsif a = NOTYP or B = NOTYP then
        return NOTYP;
      elsif a = Ints then
        if B = Ints then
          return Ints;
        else
          Emit1 (kCase26, 1);
          return Floats;
        end if;
      else
        if B = Ints then
          Emit1 (kCase26, 0);
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
      procedure Dispose is new Unchecked_Deallocation (Item, ItemPtr);

      Y     : ItemPtr;
      OP    : KeyWSymbol;
      F     : Integer;
      OperZ : constant Symset :=
       Symset'((EQL | NEQ | LSS | LEQ | GTR | GEQ => True, others => False));

      procedure SimpleExpression (FSys : Symset; X : in out Item) is
        Y     : ItemPtr;
        OP    : KeyWSymbol;
        TermZ : constant Symset :=
         Symset'((Plus | MinUS | OrSy => True, others => False));

        procedure Term (FSys : Symset; X : in out Item) is
          Y  : ItemPtr;
          OP : KeyWSymbol;
          -- TS: TypSet;
          FactorZ : constant Symset :=
           Symset'(xTimes | Divide | ModSy | AndSy => True, others => False);

          procedure Factor (FSys : Symset; X : in out Item) is
            I, F : Integer;

            procedure StandFct (NS : Integer) is
              TS : Typset;
              N  : Integer := NS;
            begin  -- STANDARD FUNCTION NO. N , N => 100 INDICATES
              -- a NILADIC FUNCTION.
              if N < 100 then
                if Sy = LParent then
                  InSymbol;
                else
                  Error (9);
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
                        Emit1 (kCase26, 0);
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
                    Error (48);
                  end if;

                else           -- N in [17,18]
                  -- EOF, Eoln
                  if Sy /= IDent then
                    Error (identifier_missing);
                  elsif Id = "INPUT     " then
                    Emit2 (k_Standard_Functions, 0, N);
                  else
                    I := GetFP (Id);
                    if I = 0 then
                      Error (undefined_identifier);
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
                  Error (closing_parenthesis_missing);
                end if;
              else            -- NILADIC FUNCTION
                case N is
                  when 100 =>
                    Emit1 (k_Standard_Functions, N); -- CLOCK
                  when others =>
                    null;
                end case;
              end if;    -- NILADIC FUNCTIONS, N => 100
            end StandFct;

          begin  -- Factor
            X.TYP := NOTYP;
            X.Ref := 0;
            Test (Factor_Begin_Symbol + StrCon, FSys, 58);
            if Sy = StrCon then
              X.TYP := Strings;
              Emit1 (k_Literal, SLeng);       -- String Length
              Emit1 (k_Literal, INum);        -- pointer To String IdTab
              InSymbol;
            end if;
            while Factor_Begin_Symbol (Sy) loop

              case Sy is
                when IDent =>

                  I := LOC (Id);
                  InSymbol;
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

                      when TypeMark | Prozedure =>
                        Error (44);

                      when Funktion =>
                        X.TYP := r.TYP;
                        if r.LEV /= 0 then
                          Call (FSys, I, CallSTDP);
                        else
                          StandFct (r.Adr);
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
                    Error (closing_parenthesis_missing);
                  end if;
                when NOTSy =>      --  NOT
                  InSymbol;
                  Factor (FSys, X);
                  if X.TYP = Bools then
                    Emit (k_NOT_Boolean);
                  elsif X.TYP /= NOTYP then
                    Error (32);
                  end if;

                when others =>
                  null;

              end case;

              if FSys = Semicolon_set then
                F := 14;
              else
                F := 6;
              end if;

              Test (FSys, Factor_Begin_Symbol, F);
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
                  Emit1 (kCase26, 1);
                  X.TYP := Floats;
                end if;
                if Y.TYP = Ints then
                  Emit1 (kCase26, 0);
                  Y.TYP := Floats;
                end if;
                if X.TYP = Floats and Y.TYP = Floats then
                  Emit (k_DIV_Float);
                else
                  if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                    Error (33);
                  end if;
                  X.TYP := NOTYP;
                end if;
              end if;
            elsif OP = AndSy then      -- AND
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (k_AND_Boolean);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (32);
                end if;
                X.TYP := NOTYP;
              end if;
            else            -- MOD  -  OP = ModSy
              if X.TYP = Ints and Y.TYP = Ints then
                Emit (k_MOD_Integer);
              else
                if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                  Error (34);
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
            Error (33);
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
          -- OR
          if OP = OrSy then
            if X.TYP = Bools and Y.TYP = Bools then
              Emit (k_OR_Boolean);
            else
              if X.TYP /= NOTYP and Y.TYP /= NOTYP then
                Error (32);
              end if;
              X.TYP := NOTYP;
            end if;
          else
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
                null;
            end case;

          end if;
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
        if X.TYP = Ints and then Y.TYP = Floats then
          X.TYP := Floats;
          Emit1 (kCase26, 1);
        end if;
        if Y.TYP = Ints and then X.TYP = Floats then
          Y.TYP := Floats;
          Emit1 (kCase26, 0);
        end if;
        if X.TYP = Enums and then Y.TYP = Enums and then X.Ref /= Y.Ref then
          Error (35);
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
          Error (35);
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
        Error (EQUALS_instead_of_BECOMES);
        InSymbol;
      else
        Error (BECOMES_missing);
      end if;
      Expression (Semicolon_set, Y);
      if X.TYP = Y.TYP then
        if StanTyps (X.TYP) then
          Emit (kStore);
        elsif X.Ref /= Y.Ref then
          Error (types_of_assignment_must_match);
        else
          case X.TYP is
            when Arrays =>
              Emit1 (kCopyBlock, ArraysTab (X.Ref).Size);
            when Records =>
              Emit1 (kCopyBlock, BlockTab (X.Ref).VSize);
            when Enums =>
              Emit (kStore);
            when others =>
              null;
          end case;
        end if;
      elsif X.TYP = Floats and Y.TYP = Ints then
        Emit1 (kCase26, 0);
        Emit (kStore);
      elsif X.TYP = Arrays and Y.TYP = Strings then
        if ArraysTab (X.Ref).ELTYP /= xChars then
          Error (types_of_assignment_must_match);
        else
          Emit1 (kStringAssignment, ArraysTab (X.Ref).Size);    -- array Size
        end if;
      elsif (X.TYP /= NOTYP) and (Y.TYP /= NOTYP) then
        Error (types_of_assignment_must_match);
      end if;
    end Assignment;

    ------------------------------------------------------------------
    --------------------------------------------------------Statement--
    procedure Statement (FSys : Symset) is
      I : Integer;

      procedure MultiStatement (Sentinal : Symset) is   -- Hathorn
        nxtSym : Symset;
      begin
        nxtSym := Sentinal + StatBegSys;
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
            while not (Sy = doSy or Sy = RParent) loop
              InSymbol;
            end loop; -- !! should check no. and
          end if;    -- Types of parms.
          if Sy = RParent then
            InSymbol;
          end if;
        end AcceptCall;

      begin  -- AcceptStatement
        InSymbol;
        I := LOC (Id);
        if IdTab (I).Obj /= aEntry then
          Error (70);
        end if;
        InSymbol;
        AcceptCall (FSys, I);
        Emit1 (k_Accept_Rendezvous, I);
        if Sy = doSy then
          if Level = LMax then
            Fatal (LEVEL_overflow);
          end if;
          Level           := Level + 1;
          Display (Level) := IdTab (I).Ref;
          InSymbol;
          MultiStatement (Symset'((EndSy => True, others => False)));
          TestEnd;
          if Sy = IDent then
            if Id /= IdTab (I).Name then
              Error (incorrect_block_name);
            end if;
            InSymbol;
          end if;
          Level := Level - 1;
        end if;
        Emit1 (k_End_Rendezvous, I);
      end AcceptStatement;

      procedure CompoundStmnt is           -- modified Hathorn
      begin
        InSymbol;
        MultiStatement (Symset'((EndSy => True, others => False)));
        TestEnd;
        if Sy = IDent then
          if Id /= IdTab (Prt).Name then
            Error (incorrect_block_name);
          end if;
          InSymbol;
        end if;
      end CompoundStmnt;

      procedure ExitStatement is          -- Hathorn
        -- Generate an absolute branch Statement with a dummy end loop address
        X : Item;
      begin
        if Sy = ExitSy then
          InSymbol;
        else
          Skip (Semicolon, 6);
        end if;

        if Sy = WhenSy then      -- conditional Exit
          InSymbol;
          Expression (Semicolon_set, X);
          if not (X.TYP = Bools or X.TYP = NOTYP) then
            Error (17);
          end if;
          Emit1 (k_Conditional_Jump, LC + 2);            -- conditional jump around Exit
        end if;
        Emit1 (k_Jump, -1);        -- unconditional jump with address To be
                                  --patched
      end ExitStatement;

      procedure IfStatement is
        X        : Item;
        LC0, LC1 : Integer;
      begin
        InSymbol;
        Expression
         (FSys + Symset'((ThenSy | doSy => True, others => False)),
          X);
        if not (X.TYP = Bools or else X.TYP = NOTYP) then
          Error (17);
        end if;
        LC1 := LC;
        Emit (k_Conditional_Jump);                  -- JMPC
        if Sy = ThenSy then
          InSymbol;
        else
          Error (52);
          if Sy = doSy then
            InSymbol;
          end if;
        end if;
        MultiStatement
         (Symset'((ElsIfSy | ElseSy | EndSy => True, others => False)));
        LC0 := LC;
        while Sy = ElsIfSy loop     -- Added Hathorn
          InSymbol;
          Emit1 (k_Jump, -1);              -- unconditional jump with address
                                          --To be
          --                             patched
          ObjCode (LC1).Y := LC;            -- patch the previous conditional
                                            --jump
          Expression
           (FSys + Symset'((ThenSy | doSy => True, others => False)),
            X);
          if not (X.TYP = Bools or else X.TYP = NOTYP) then
            Error (17);
          end if;
          LC1 := LC;
          Emit (k_Conditional_Jump);                -- JMPC
          if Sy = ThenSy then
            InSymbol;
          else
            Error (52);
            if Sy = doSy then
              InSymbol;
            end if;
          end if;
          MultiStatement
           (Symset'((ElsIfSy | ElseSy | EndSy => True, others => False)));
        end loop;

        if Sy = ElseSy then
          InSymbol;
          Emit1 (k_Jump, -1);
          ObjCode (LC1).Y := LC;
          MultiStatement (Symset'((EndSy => True, others => False)));
        else
          ObjCode (LC1).Y := LC;
        end if;
        if Sy = EndSy then -- Added Hathorn
          InSymbol;
        else
          Error (END_missing);
        end if;
        if Sy = IfSy then
          InSymbol;
        else
          Error (62);
        end if;
        -- Go back and patch the dummy addresses in unconditional jumps
        while LC0 < LC loop
          if ObjCode (LC0).Y = -1 then
            ObjCode (LC0).Y := LC;
          end if;
          LC0 := LC0 + 1;
        end loop;
      end IfStatement;

      procedure LoopStatement (FCT, B : Integer) is    -- Hathorn
        LC0 : Integer := LC;
      begin
        if Sy = LoopSy then
          InSymbol;
        else
          Skip (StatBegSys, 62);
        end if;
        MultiStatement (Symset'((EndSy => True, others => False)));
        Emit1 (FCT, B);
        if Sy = EndSy then
          InSymbol;
        else
          Error (END_missing);
        end if;
        if Sy = LoopSy then
          InSymbol;
        else
          Error (54);
        end if;

        -- Go back and patch the dummy addresses generated by Exit stmts.
        while LC0 < LC loop
          if ObjCode (LC0).Y = -1 then
            ObjCode (LC0).Y := LC;
          end if;
          LC0 := LC0 + 1;
        end loop;
      end LoopStatement;

      procedure ReturnStatement is           -- Hathorn
        -- Generate a procedure return Statement, calculate return value if
        --req'D
        X, Y : Item;
        F    : Integer;
      begin

        if BlockID = ProgramID then
          Error (45);
        end if;
        I := LOC (BlockID);
        InSymbol;
        if Sy = Semicolon and IsFun then
          Error (68);
        end if;
        if Sy /= Semicolon then          -- calculate return value
          if IdTab (I).Ref = Display (Level) then
            X.TYP := IdTab (I).TYP;
            X.Ref := IdTab (I).Ref;
            if IdTab (I).Normal then
              F := 0;
            else
              F := 1;
            end if;

            Emit2 (F, IdTab (I).LEV + 1, 0);
            Expression (Semicolon_set, Y);
            if X.TYP = Y.TYP then
              if StanTyps (X.TYP) then
                Emit (kStore);
              elsif X.Ref /= Y.Ref then
                Error (types_of_assignment_must_match);
              elsif X.TYP = Floats and Y.TYP = Ints then
                Emit1 (kCase26, 0);
                Emit (kStore);
              elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
                Error (types_of_assignment_must_match);
              end if;
            end if;
          else
            Error (45);
          end if;
        end if;
        if IsFun then
          Emit1 (kExitFunction, CallSTDP);
        else
          Emit1 (kExitCall, CallSTDP);
        end if;
      end ReturnStatement;

      procedure DelayStatement is            -- Cramer
        -- Generate a Task delay
        Y : Item;
      begin
        InSymbol;
        if Sy = Semicolon then
          Skip (Semicolon, 72);
        else                  -- calculate delay value
          Expression (Semicolon_set, Y);
          if Y.TYP /= Floats then
            Error (73);
          end if;
        end if;
        Emit (kDelay);
      end DelayStatement;

      procedure CaseStatement is
        X         : Item;
        I, J, LC1 : Integer;
        type GrounfZ is record
          Val, LC : Index;
        end record;
        CaseTab : array (1 .. CSMax) of GrounfZ;
        ExitTab : array (1 .. CSMax) of Integer;

        procedure CaseLabel is
          Lab : ConRec;
          K   : Integer;
        begin

          KKonstant
           (FSys + Symset'((Alt | Finger => True, others => False)),
            Lab);
          if Lab.TP /= X.TYP then
            Error (47);
          elsif I = CSMax then
            Fatal (6);
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
              Error (duplicate_identifier);
            end if;
            -- MULTIPLE DEFINITION
          end if;
        end CaseLabel;

        procedure ONECASE is
        begin
          if Sy = WhenSy then
            InSymbol;
            if ConstBegSys (Sy) then
              CaseLabel;
              while Sy = Alt loop
                InSymbol;
                CaseLabel;
              end loop;

            end if;
            if Sy = OthersSy then        -- Hathorn
              if I = CSMax then
                Fatal (6);
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
              Error (64);
            end if;
            MultiStatement
             (Symset'((WhenSy | EndSy => True, others => False)));
            J           := J + 1;
            ExitTab (J) := LC;
            Emit (k_Jump);
          else
            Error (63);
          end if;
        end ONECASE;

      begin -- CaseStatement
        InSymbol;
        I := 0;
        J := 0;
        Expression
         (FSys +
          Symset'((OFSy | IsSy | Comma | Colon => True, others => False)),
          X);

        if not (X.TYP = Ints or
                X.TYP = Bools or
                X.TYP = xChars or
                X.TYP = NOTYP)
        then
          Error (23);
        end if;
        LC1 := LC;
        Emit (kSwitch); -- JMPX
        if Sy = IsSy then -- Was OfSy in SmallAda ! I.e. case x OF when 1 =>
                          --...
          InSymbol;
        elsif Sy = OFSy then
          Error (OF_instead_of_IS); -- Common mistake by Pascal programmers
          InSymbol;
        else
          Error (IS_missing);
        end if;

        while Sy = WhenSy loop
          ONECASE;
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

        if Sy = EndSy then
          InSymbol;
        else
          Error (END_missing);
        end if;
        if Sy = CaseSy then
          InSymbol;
        else
          Error (65);
        end if;
      end CaseStatement;

      procedure WhileStatement is
        X        : Item;
        LC1, LC2 : Integer;
      begin
        InSymbol;
        LC1 := LC;
        Expression
         (FSys + Symset'((LoopSy | doSy => True, others => False)),
          X);
        if not (X.TYP = Bools or else X.TYP = NOTYP) then
          Error (17);
        end if;
        LC2 := LC;
        Emit (k_Conditional_Jump);
        LoopStatement (10, LC1);
        ObjCode (LC2).Y := LC;
      end WhileStatement;

      ------------------------------------------------------------ForStatement
      procedure ForStatement is
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
           (Symset'((InSy     |
            RangeSy           |
            LoopSy            |
            EndSy             => True,
                     others => False)) +
            FSys,
            2);
        end if;

        Emit2 (k_Load_Address, IdTab (T).LEV, IdTab (T).Adr);
        InSymbol;
        F := kFor1;
        if Sy = InSy then
          InSymbol;
          if Sy = ReverseSy then
            F := kFor1Rev;
            InSymbol;
          end if;
          Expression
           (Symset'((RangeSy | LoopSy | EndSy => True, others => False)) +
            FSys,
            X);
          IdTab (T).TYP := X.TYP;
          if not (X.TYP = Ints
                 or else X.TYP = Bools
                 or else X.TYP = xChars)
          then
            Error (18);
          end if;
          if Sy = RangeSy then
            InSymbol;
            Expression (FSys + LoopSy, X);
            if (IdTab (T).TYP /= X.TYP) then
              Error (19);
            end if;
          else
            Skip
             (Symset'((LoopSy | EndSy | Semicolon => True, others => False)) +
              FSys,
              55);
          end if;
        else
          Skip (FSys + LoopSy, 53);
        end if;
        LC1 := LC;
        Emit (F);
        LoopStatement (F + 1, LC);
        ObjCode (LC1).Y                  := LC;
        T                                := T - 1;
        BlockTab (Display (Level)).Last  := last;
        Dx                               := Dx - 1;
      end ForStatement;

      procedure SelectStatement is
        procedure SelectError (N : Integer) is
        begin
          Skip (Semicolon, N);
        end SelectError; -- SelectError

        -- Either a Timed or Conditional Entry Call.

        procedure QualifiedEntryCall is
          I, J, IStart, IEnd : Integer;
          patch              : array (0 .. 4) of Integer;
          O                  : Order;
          Y                  : Item;
        begin
          I := LOC (Id);
          if IdTab (I).Obj = aTask then
            InSymbol;
            EntryCall (FSys, I, -1);
            if ObjCode (LC - 2).F = 19 then     -- need To patch CallType later
              patch (0) := LC - 2;
            else
              patch (0) := LC - 3;
            end if;       -- LC-1 must be OP=3, update Display
            patch (1) := LC;           -- need To patch in JMPC address later
            Emit1 (k_Conditional_Jump, -1);    -- JMPC, address patched in after ELSE
                                      --or OR
            if Sy = Semicolon then
              InSymbol;
            else
              Skip (Semicolon, 14);
            end if;
            if not (Sy = OrSy or else Sy = ElseSy) then
              MultiStatement
               (Symset'((OrSy | ElseSy => True, others => False)));
            end if;
            if Sy = OrSy then      -- =====================> Timed Entry Call
              ObjCode (patch (0)).X      := CallTMDE; -- Timed Entry Call
              ObjCode (patch (0) + 1).Y  := CallTMDE; -- Exit type matches
                                                      --Entry type
              InSymbol;
              if Sy = DelaySy then
                InSymbol;
                if Sy = Semicolon then
                  SelectError (72);
                else          -- calculate delay value
                  patch (2) := LC;
                  Expression (Semicolon_set, Y);
                  patch (3) := LC - 1;
                  if Y.TYP /= Floats then
                    SelectError (73);
                  else        -- end of timed Entry select ObjCode, do patching
                    ObjCode (patch (1)).Y  := LC; -- if Entry not made, Skip
                                                  --rest
                    J                      := patch (3) - patch (2) + 1;
                    IStart                 := patch (0);
                    IEnd                   := LC - 1;
                    while J > 0 loop     -- move delay time ObjCode To before
                      O := ObjCode (IEnd);  -- opcodes 19 Call, 32 return
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
                SelectError (79);
              end if;      -- DELAY EXPECTED
            -- end Sy = OrSy
            else              -- Sy = ElseSy, ===============> Conditional
                              --                             Entry Call
              ObjCode (patch (0)).X      := CallCNDE; -- Conditional Entry Call
              ObjCode (patch (0) + 1).Y  := CallCNDE;
              patch (2)                  := LC;
              Emit1 (k_Jump, -1);          -- JMP, address patched in after END
                                          --SELECT
              patch (3) := LC;
              InSymbol;
              MultiStatement (Symset'((EndSy => True, others => False)));
              ObjCode (patch (1)).Y  := patch (3);
              ObjCode (patch (2)).Y  := LC;
            end if;
            if Sy /= EndSy then
              SelectError (END_missing);
            end if;
            InSymbol;
            if Sy /= SelectSy then
              SelectError (80);
            end if;
          else
            SelectError (77);
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
                while not (Sy = doSy or Sy = RParent) loop
                  InSymbol;
                end loop;
              end if;        -- of parameters.
              if Sy = RParent then
                InSymbol;
              end if;
            end AcceptCall2;

          begin                -- AcceptStatment2
            InSymbol;
            I := LOC (Id);
            if IdTab (I).Obj /= aEntry then
              SelectError (70);
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
            if Sy = doSy then
              if Level = LMax then
                Fatal (LEVEL_overflow);
              end if;
              Level           := Level + 1;
              Display (Level) := IdTab (I).Ref;
              InSymbol;
              MultiStatement (Symset'((EndSy => True, others => False)));
              TestEnd;
              if Sy = IDent then
                if Id /= IdTab (I).Name then
                  SelectError (incorrect_block_name);
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
              when WhenSy =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                InSymbol;          -- WHENSTATEMENT
                Expression (FSys + Finger, X);
                if not (X.TYP = Bools or else X.TYP = NOTYP) then
                  SelectError (17);
                end if;
                InSymbol;
                if Sy = AcceptSy then
                  if IAlt > 10 then
                    Fatal (PATCHING_overflow);
                  else
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Conditional_Jump);
                    AcceptStatement2;
                  end if;
                elsif Sy = DelaySy then
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
                      SelectError (73);
                    end if;
                    if IAlt > 10 then
                      Fatal (PATCHING_overflow);
                    end if;
                    IAlt       := IAlt + 1;
                    Alt (IAlt) := LC;
                    Emit (k_Jump);
                  end if;
                else
                  SelectError (3);
                end if;
                InSymbol;
                MultiStatement
                 (Symset'((OrSy | ElseSy | EndSy => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);          -- patch JMP ADDRESS AT EndSy
              -- end WhenSy

              when AcceptSy =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                AcceptStatement2;
                InSymbol;
                MultiStatement
                 (Symset'((OrSy | ElseSy | EndSy => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end AcceptSy

              when OrSy =>       -- OR STATEMENT
                InSymbol;

              when ElseSy =>
                for I in 1 .. IAlt loop
                  -- patch ObjCode
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                IAlt := 0;
                InSymbol;
                MultiStatement (Symset'((EndSy => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end ElseSy

              when DelaySy =>
                for I in 1 .. IAlt loop
                  ObjCode (Alt (I)).Y  := LC;
                end loop;
                -- patch
                IAlt := 0;
                -- Generate a Task delay, calculate return value if req'D
                InSymbol;
                if Sy = Semicolon then
                  Skip (Semicolon, 72);
                else          -- calculate return value
                  Expression
                   (Symset'((Semicolon => True, others => False)),
                    Y);
                  Emit2 (kSelectiveWait, 4, LC + 2);  -- Update delay time
                  if Y.TYP /= Floats then
                    SelectError (73);
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
                 (Symset'((OrSy | EndSy | ElseSy => True, others => False)));
                if ISD > 10 then
                  Fatal (PATCHING_overflow);
                end if;
                ISD       := ISD + 1;
                JSD (ISD) := LC;
                Emit (k_Jump);
              -- end DelaySy

              when TerminateSy =>
                InSymbol;
                if Sy /= Semicolon then
                  SelectError (semicolon_missing);
                end if;
                do_terminate := True;        -- Oguz
                InSymbol;

              when EndSy =>
                InSymbol;
                if Sy /= SelectSy then
                  SelectError (END_missing);
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

      begin                    -- Sy = SelectSy
        -- Next KeyWSymbol must be AcceptSy, WhenSy, or a Task Entry object
        --Name.
        InSymbol;
        if Sy = AcceptSy or Sy = WhenSy or Sy = IDent then
          if Sy = AcceptSy or Sy = WhenSy then
            SelectiveWait;
          else
            QualifiedEntryCall;
          end if;         -- Timed or Conditional Entry Call
          InSymbol;
        else
          SelectError (76);
        end if;
      end SelectStatement;                      -- SelectStatement

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
              I := GetFP (Id);  -- Schoening
              if I /= -1 then -- First parameter is a file variable
                Emit1 (k_Set_current_file_pointer, I);
                InSymbol;
                if Sy /= Comma then
                  if Sy = RParent then
                    goto SKIP1b;
                  else
                    Error (identifier_missing);
                  end if;
                end if;
              else
                Emit1 (k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              end if;
              loop
                if do_first_InSymbol then
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if Sy /= IDent then
                  Error (identifier_missing);
                else
                  I := LOC (Id);
                  InSymbol;
                  if I /= 0 then
                    if IdTab (I).Obj /= Variable then
                      Error (variable_missing);
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
                        Error (41);
                      end if;
                    end if;
                  end if;
                end if;
                Test
                 (Symset'((Comma | RParent => True, others => False)),
                  FSys,
                  6);

                exit when Sy /= Comma;
              end loop;
              <<SKIP1b>>
              if Sy = RParent then
                InSymbol;
              else
                Error (closing_parenthesis_missing);
              end if;
            end if;
            if N = 2 then
              Emit (kGetNewline);
            end if;

          when 3 | 4 =>          -- PUT, PUT_LINE

            if Sy = LParent then
              InSymbol;
              I := GetFP (Id);   -- Schoening
              if I /= -1 then -- First parameter is a file variable
                Emit1 (k_Set_current_file_pointer, I);
                InSymbol;
                if Sy /= Comma then
                  if Sy = RParent then
                    goto Label_21; -- skip the loop
                  else
                    Error (identifier_missing);
                  end if;
                end if;
              else
                Emit1 (k_Set_current_file_pointer, 0);
                do_first_InSymbol := False;
              end if;
              loop
                if do_first_InSymbol then
                  InSymbol;
                end if;
                do_first_InSymbol := True;
                if Sy = StrCon then
                  Emit1 (k_Literal, SLeng);
                  Emit1 (kWriteString, INum);
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
                  if (not StanTyps (X.TYP)) and (X.TYP /= Strings) then
                    Error (41);
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
                      Error (parameter_must_be_integer);
                    end if;
                    if Sy = Colon then
                      if X.TYP /= Floats then
                        Error (42);
                      end if;
                      InSymbol;
                      Expression
                       (FSys +
                        Symset'((Comma | RParent => True, others => False)),
                        Y);
                      if Y.TYP /= Ints then
                        Error (parameter_must_be_integer);
                      end if;
                      Emit (kCase37);
                    else
                      Emit1 (kWrite2, Types'Pos (X.TYP));
                    end if;
                  elsif X.TYP = Strings then
                    Emit1 (kWriteString, X.Ref);
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
                Error (closing_parenthesis_missing);
              end if;
            end if;
            if N = 4 then
              Emit (kPutNewline);
            end if;

          when 5 | 6 =>                  -- Wait,SIGNAL
            if Sy /= LParent then
              Error (9);
            else
              InSymbol;
              if Sy /= IDent then
                Error (undefined_identifier);
              else
                I := LOC (Id);
                InSymbol;
                if I /= 0 then
                  if IdTab (I).Obj /= Variable then
                    Error (variable_missing);
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
                      Error (parameter_must_be_integer);
                    end if;
                  end if;
                end if;
              end if;
              if Sy = RParent then
                InSymbol;
              else
                Error (closing_parenthesis_missing);
              end if;
            end if;

          when 7 | 8 | 9 =>    -- reset, Rewrite, Close
            -- Schoening
            if Sy /= LParent then
              Error (9);
            else
              InSymbol;
              I := GetFP (Id);
              if I /= -1 then
                Emit2 (kFile_I_O, I, N);
              else
                Error (identifier_missing);
              end if;
              InSymbol;
              if Sy = RParent then
                InSymbol;
              else
                Error (closing_parenthesis_missing);
              end if;
            end if;  -- reset

          when 10 =>        -- CursorAt
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, 9);
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
                  Skip (Semicolon, parameter_must_be_integer);
                end if;
                if Sy /= Comma then
                  Skip (Semicolon, 74);
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
                    Skip (Semicolon, parameter_must_be_integer);
                  end if;
                  if Sy = Comma then
                    Skip (Semicolon, 39);
                  elsif Sy /= RParent then
                    Skip (Semicolon, 4);
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
              Skip (Semicolon, 9);
            else
              InSymbol;
              Expression (Symset'((RParent => True, others => False)), X);
              if X.TYP /= Floats then
                Skip (Semicolon, 42);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, 4);
              else
                Emit (kSetQuatumTask);
                InSymbol;
              end if;
            end if;                -- Quantum

          when 12 =>                   -- Set Priority
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, 9);
            else
              InSymbol;
              Expression (Symset'((RParent => True, others => False)), X);
              if X.TYP /= Ints then
                Skip (Semicolon, parameter_must_be_integer);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, 4);
              else
                Emit (kSetTaskPriority);
                InSymbol;
              end if;
            end if;                -- Priority

          when 13 =>                   -- Set Priority Inheritance,INHERITP
            -- Cramer
            if Sy /= LParent then
              Skip (Semicolon, 9);
            else
              InSymbol;
              Expression (Symset'((RParent => True, others => False)), X);
              if X.TYP /= Bools then
                Skip (Semicolon, 75);
              end if;
              if Sy /= RParent then
                Skip (Semicolon, 4);
              else
                Emit (kSetTaskPriorityInheritance);
                InSymbol;
              end if;
            end if;                -- Inheritp

          when others =>
            null;

        end case;
      end StandProc;

    begin  -- Statement
      if Err_Count > 0 then  --{MRC: added from PC version}
        return;
      end if;

      --{ Mark the following opcodes as belonging to LineCount # }
      Emit1 (kHighlightSource, LineCount);  --{MRC: this line is not in PC version}
      --{ This did not work because the LineCount was off by one. Why? }
      --{ MRC: This line is needed in order to highlight lines in task windows
      --}

      if StatBegSys (Sy) then
        case Sy is
        when IDent =>
          I := LOC (Id);
          InSymbol;
          if I /= 0 then
            case IdTab (I).Obj is
              when Konstant | TypeMark | Funktion =>
                Error (16);
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
              when others =>
                null;
            end case;

          end if;
        -- end IdEnt

        when AcceptSy =>
          AcceptStatement;
        when BeginSy =>
          CompoundStmnt;
        when CaseSy =>
          CaseStatement;
        when DelaySy =>
          DelayStatement;
        when ExitSy =>
          ExitStatement;
        when ForSy =>
          ForStatement;
        when IfSy =>
          IfStatement;
        when LoopSy =>
          LoopStatement (10, LC);
        when NullSy =>
          InSymbol;
        when ReturnSy =>
          ReturnStatement;
        when SelectSy =>
          SelectStatement;
        when WhileSy =>
          WhileStatement;
        when others =>
          null;
        end case;

        if not (EofInput) then      --{MRC: added IF NOT... from PC version}
          if Sy = Semicolon then
            InSymbol;
          else
            Error (semicolon_missing);
          end if;
        end if;
      end if;  -- Sy in StatBegSys

      if not EofInput then
        Test (FSys - Semicolon, Semicolon_set, 6);
      end if;

    end Statement;

  begin  -- Block
    if Err_Count > 0 then --{MRC, from PC source}
      return;
    end if;

    BlockID := IdTab (Prt).Name;
    Dx      := 5;
    ICode   := 0;
    if Level > LMax then
      Fatal (LEVEL_overflow);
      return;            --{MRC, from PC source}
    end if;

    Test
     (Symset'(LParent | ReturnSy | IsSy | Semicolon => True, others => False),
      FSys,
      6);
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
    if IsFun then
      if Sy = ReturnSy then
        InSymbol;  -- FUNCTION TYPE
        if Sy = IDent then
          I := LOC (Id);
          InSymbol;
          if I /= 0 then
            if IdTab (I).Obj /= TypeMark then
              Error (29);
              return;  --{MRC, from PC source}
            elsif StanTyps (IdTab (I).TYP) then
              IdTab (Prt).TYP := IdTab (I).TYP;
            else
              Error (15);
              return;    --{MRC, from PC source}
            end if;
          end if;
        else
          Skip (FSys + Semicolon, 2);
        end if;
      else
        Error (59);
        return;  --{MRC, from PC source}
      end if;
    end if;

    if Sy = Semicolon then  -- end of specification part
      BlockTab (PRB).VSize := Dx;
      IdTab (Prt).Adr      := -1;    -- address of body TBD
      return; -- Exit(Block);
    end if;

    if Sy = IsSy then
      InSymbol;
    else
      Error (IS_missing);
      return;
    end if;

    loop
      if Sy = IDent then
        VarDeclaration;
      end if;
      if Sy = TypeSy then
        TypeDeclaration;
      end if;
      if Sy = TaskSy then
        TaskDeclaration;
      end if;
      BlockTab (PRB).VSize := Dx;

      while Sy = ProcSy or else Sy = FuncSy loop
        ProcDeclaration;
      end loop;

      exit when Sy = BeginSy;
    end loop;

    MaxDX           := Dx;
    IdTab (Prt).Adr := LC;
    -- copy initialization ObjCode from end of ObjCode table
    I := CMax + ICode;
    while I > CMax loop
      ObjCode (LC) := ObjCode (I);
      LC           := LC + 1;
      I            := I - 1;
    end loop;

    CMax := CMax + ICode;
    InSymbol;
    loop
      Statement (FSys + EndSy);
      if Err_Count > 0 then  --{MRC, from PC source}
        Sy := EndSy;
      end if;
      exit when Sy = EndSy or Err_Count > 0;
    end loop;
    --{MRC, added OR()... from PC source}
    BlockTab (PRB).SrcTo := LineCount;

    if Sy = EndSy then
      InSymbol;
    else
      Error (END_missing);
      return;
    end if; -- Missing END

    if Sy = IDent then
      if Id /= BlockID then
        Error (incorrect_block_name);
        return;
      end if;
      InSymbol;
    end if;

    if Sy /= Semicolon then
      Error (semicolon_missing);
      return;
    end if;

    if BlockID /= ProgramID then
      InSymbol;
      Test (FSys, Empty_Symset, 6);
    end if;

  end Block;

end HAC.Parser;
