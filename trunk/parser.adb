with PCode;                             use PCode;
with Scanner;                           use Scanner;
with UErrors;                           use UErrors;
with Unchecked_deallocation;

package body Parser is

  Semicolon_set: constant Symset:= Symset'(Semicolon=> true, others=> false);

------------------------------------------------------------------
------------------------------------------------------------Block-

PROCEDURE Block(FSys: Symset; IsFun: Boolean; Level_A: Integer; Prt: Integer) IS

  Level: Integer:= Level_A;

 TYPE ConRec IS RECORD
   TP: Types;
   I:  Integer;
   R:  Float;
--   CASE TP IS
--     WHEN Ints | xChars | Bools => I: Integer;
--     WHEN Floats => R: Float;
--     WHEN others => null;
--   END CASE;
 END RECORD;

 Dx:       Integer;  -- data allocation Index
 MaxDX:    Integer;
 PRB:      Integer;  -- B-Index of this procedure
 I:        Integer;  -- Index into the identifier table IdTab
 ICode:    Integer;  -- Size of initialization ObjCode generated
 BlockID:  Alfa;    -- Name of the current Block

 ------------------------------------------------------------------
 -------------------------------------------------------EnterArray-

  PROCEDURE EnterArray(TP: Types; L, H: Integer)IS
    Lz,Hz: Integer;
    BEGIN
    IF  L > H THEN Error(27); END IF;
    Lz:= L;
    Hz:= H;
    IF  (abs(L) > XMax)  OR  (abs(H) > XMax) THEN
      Error(27);
      Lz := 0;
      Hz := 0;
    END IF;
    IF  a = AMax THEN
      Fatal(4);     -- array table has overflowed
    ELSE
      a := a + 1;
      DECLARE
        r : ATabEntry RENAMES ArraysTab(a);
      BEGIN
        r.InXTYP:= TP;
        r.Low   := Lz;
        r.High  := Hz;
      END;
    END IF;
  END EnterArray;

 ------------------------------------------------------------------
 -------------------------------------------------------EnterBlock-
  PROCEDURE EnterBlock(Tptr: Integer) IS
  BEGIN
    IF  B = BMax THEN
      Fatal(2);
    ELSE
      B := B + 1;
      BlockTab(B).Id := IdTab(Tptr).Name;
      BlockTab(B).last := 0;
      BlockTab(B).LastPar := 0;
      BlockTab(B).SrcFrom := LineCount;
    END IF;
  END EnterBlock;

  ------------------------------------------------------------------
  -------------------------------------------------------EnterFloat-
  PROCEDURE EnterFloat(X: Float) IS
  BEGIN
    IF  C2 = C2Max - 1 THEN
      Fatal(3);
    ELSE
      FloatPtTab(C2 + 1) := X;
      C1 := 1;
      WHILE  FloatPtTab(C1) /= X  LOOP C1 := C1 + 1; END LOOP;
      IF  C1 > C2 THEN C2 := C1; END IF;
    END IF;
  END EnterFloat;

  ------------------------------------------------------------------
  -------------------------------------------------------------Skip-
  PROCEDURE Skip(FSys: Symset; N: Integer) IS

   FUNCTION  StopMe  RETURN  Boolean IS BEGIN RETURN false;END;

   BEGIN
    Error(N);
    SkipFlag := True;
    WHILE  NOT FSys(Sy)  LOOP
      InSymbol;
      IF  StopMe THEN raise Failure_1_0; END IF;
    END LOOP;

    InSymbol;    -- Manuel:  If this InSymbol call is
          -- omitted, the system will get in an
          -- infinite loop on the statement:
          --  put_lin("Typo is on purpose");

    IF  StopMe THEN raise Failure_1_0; END IF;
    IF  SkipFlag THEN  EndSkip; END IF;
  END Skip;

  PROCEDURE Skip(S: KeyWSymbol; N: Integer) IS
    to_skip: Symset:= SymSet'(others=> false);
    BEGIN
      to_skip(S):= true;
      Skip( to_skip , N);
    END;

  ------------------------------------------------------------------
  -------------------------------------------------------------Test-
  PROCEDURE Test(S1, S2: Symset; N: Integer) IS
       BEGIN
    IF  NOT S1(Sy) THEN Skip(S1 + S2, N); END IF;
   END;

  ------------------------------------------------------------------
  ----------------------------------------------------TestSemicolon-
  PROCEDURE TestSemicolon IS
   comma_or_colon: constant Symset:=
     Symset'(comma|colon=> true, others=> false);
   BEGIN
      IF  Sy = Semicolon THEN
        InSymbol;
      ELSE
        Error(14);
        IF comma_or_colon(SY) THEN InSymbol; END IF;
      END IF;
      Test(Symset'((IDent | TypeSy | TaskSy => true, others=> false)) +
                    BlockBegSyS, FSys, 6);
   END TestSemicolon;

  ------------------------------------------------------------------
  ----------------------------------------------------------TestEnd-
  PROCEDURE TestEnd   IS            -- Hathorn
   BEGIN
    IF Sy = EndSy THEN InSymbol; ELSE Skip(Semicolon, 57); END IF;
   END;

  ------------------------------------------------------------------
  ------------------------------------------------------------Enter-
  PROCEDURE Enter(Id: Alfa; K: aObject) IS
    J, L:  Integer;
  BEGIN

    IF  T = TMax THEN
      Fatal(1)              -- identifier table overflow
    ;ELSE
      IdTab(0).Name := Id;        -- sentinel
      J := BlockTab(Display(Level)).last;
      L := J;
      WHILE  IdTab(J).Name /= Id  LOOP J := IdTab(J).Link; END LOOP;
       -- follow the chain of identifiers for
      -- current Level.
      IF  J /= 0 THEN    -- duplicate identifier
        Error(1);
      ELSE      -- Enter identifier if there is room in table IdTab
        T := T + 1;
        IdTab(T):=
         (Name=> Id, Link=> L, Obj=> K, TYP=> NOTYP,
          Ref=> 0, Normal=> True, LEV=> Level, Adr=> 0);
        BlockTab(Display(Level)).last := T; -- update start of identifer chain
      END IF;
    END IF;
  END Enter;

  ------------------------------------------------------------------
  --------------------------------------------------------------LOC-
  FUNCTION LOC(Id: Alfa) RETURN  Integer IS
    L, J: Integer;        -- locate identifier, Id, in table
  BEGIN
    L := Level;
    IdTab(0).Name := Id;      -- sentinel
    LOOP
      J := BlockTab(Display(L)).last;
      WHILE IdTab(J).Name /= Id LOOP
        J := IdTab(J).Link;
      END LOOP;
      L := L - 1;
      EXIT WHEN L < 0 OR J /= 0;
    END LOOP;
    IF J = 0 THEN
      Error(0);
    END IF;
    RETURN J;
  END LOC;

  ------------------------------------------------------------------
  ------------------------------------------------------------GetFP-
  FUNCTION GetFP(Id: Alfa) RETURN  Integer IS    -- Schoening
    ResultGetFP : Integer;
    BEGIN   -- locate Id in FileIOTab
      ResultGetFP := - 1;
      FOR I IN 1.. FileIOTab.Kount LOOP
        IF  FileIOTab.NAM(I)(2) = ':' THEN
          IF FileIOTab.NAM(I)(3..FileIOTab.LNAM(I) - 2)
             = Id(1..FileIOTab.LNAM(I) - 2) THEN
            ResultGetFP := I;
          END IF;
        ELSIF FileIOTab.NAM(I) = Id( 1.. FileIOTab.LNAM(I)) THEN
          ResultGetFP := I;
        END IF;
      END LOOP;

      RETURN ResultGetFP;

    END GetFP;

  ------------------------------------------------------------------
  ----------------------------------------------------EnterVariable-
  PROCEDURE EnterVariable IS
   BEGIN  IF Sy = IDent THEN Enter(Id, Variable); InSymbol;
               ELSE Error(2);
    END IF;
   END EnterVariable;

  ------------------------------------------------------------------
  ---------------------------------------------------------Constant-
  PROCEDURE KKonstant(FSys: Symset;  C: IN OUT  ConRec) IS
    X, Sign:  Integer;
  BEGIN
    C.TP := NOTYP;
    C.I := 0;
    Test(ConstBegSys, FSys, 50);
    IF ConstBegSys(Sy) THEN
      IF  Sy = CharCon THEN
        C.TP := xChars;
        C.I := INUM;
        InSymbol;
      ELSE
        Sign := 1;
        IF Sy=Plus or Sy=MinUS THEN
          IF Sy = MinUS THEN
            Sign := - 1;
          END IF;
          InSymbol;
        END IF;

        IF Sy = IDent THEN
          X := LOC(Id);
          IF  X /= 0 THEN
            IF  IdTab(X).Obj /= Konstant THEN
              Error(25);
            ELSE
              C.TP := IdTab(X).TYP;
              IF  C.TP = Floats THEN
                C.R := Float(Sign) * FloatPtTab(IdTab(X).Adr);
              ELSE
                C.I := Sign * IdTab(X).Adr;
              END IF;
            END IF;
          END IF; -- X /= 0
          InSymbol;
        ELSIF  Sy = IntCon THEN
          C.TP := Ints;
          C.I := Sign * INUM;
          InSymbol;
        ELSIF  Sy = FloatCon THEN
          C.TP := Floats;
          C.R := Float(Sign) * RNum;
          InSymbol;
        ELSE
          Skip(FSys, 50);
        END IF;
      END IF;
      Test(FSys, Empty_Symset , 6);
    END IF;

  END KKonstant;

  ------------------------------------------------------------------
  --------------------------------------------------------------TYP-

  PROCEDURE TYP(FSys: Symset;  TP: out Types;  RF, Sz: out Integer) IS
    I, ECount:      Integer;
    ELTP:        Types;
    ELRF:        Integer;
    ELSZ, Offset, T0, T1: Integer;
    StrArray:      Boolean;

    FSys_gnagna: constant Symset:=
       FSys
     - Symset'(Semicolon|Comma|IDent=> true, others=> false)
     + Symset'(EndSy=> true, others=> false);


    PROCEDURE ArrayTyp( ARef, Arsz: IN OUT  Integer;
                                    StrAr: Boolean) IS
      ELTP:           Types;
      Low, High:      ConRec;
      ELRF, ELSZ:     Integer;
    BEGIN
      KKonstant(Symset'((RangeSy | RParent |OFSy => true,
                         others=> false)) + FSys, Low);

      IF  Low.TP = Floats THEN
        Error(27);
        Low.TP := Ints;
        Low.I := 0;
      END IF;
      IF  Sy = RangeSy THEN InSymbol; ELSE Error(13); END IF;

      KKonstant(Symset'(Comma | RParent | OFSy=> true,
                        others=> false) + FSys, High);

      IF  High.TP /= Low.TP THEN
        Error(27);
        High.I := Low.I;
      END IF;
      EnterArray(Low.TP, Low.I, High.I);
      ARef := a;
      IF  StrAr THEN
        ELTP := xChars;
        ELRF := 0;
        ELSZ := 1;
        IF  Sy = RParent THEN
          InSymbol;
        ELSE
          Error(4);
          IF  Sy = RBrack THEN InSymbol; END IF;
        END IF;
      ELSIF  Sy = Comma THEN
        InSymbol;
        ELTP := Arrays;
        ArrayTyp(ELRF, ELSZ, StrAr);
      ELSE
        IF  Sy = RParent THEN
          InSymbol;
        ELSE
          Error(4);
          IF  Sy = RBrack THEN InSymbol; END IF;
        END IF;
        IF  Sy = OFSy THEN InSymbol; ELSE Error(8); END IF;
        TYP(FSys, ELTP, ELRF, ELSZ);
      END IF;
      DECLARE
        r : ATabEntry RENAMES ArraysTab(ARef);
      BEGIN
        Arsz := (High.I - Low.I + 1) * ELSZ;
        r.Size := Arsz;
        r.ELTYP := ELTP;
        r.ELREF := ELRF;
        r.ELSize := ELSZ;
      END;
    END ArrayTyp;

  BEGIN  -- Type
    TP := NOTYP;
    RF := 0;
    Sz := 0;
    Test(TypeBegSys, FSys, 10);
    IF  TypeBegSys(Sy) THEN
      IF  (Id = "STRING    ") THEN Sy := StringSy; END IF;
      CASE  Sy  IS
       WHEN IDent=>
        I := LOC(Id);     -- IDent
        IF  I /= 0 THEN
          DECLARE r: TabEntry RENAMES IdTab(I);
          BEGIN
            IF  r.Obj /= TypeMark THEN
              Error(29);
            ELSE
              TP := r.TYP;
              RF := r.Ref;
              Sz := r.Adr;
              IF  TP = NOTYP THEN Error(30); END IF;
            END IF;
          END;
        END IF;
        InSymbol;

       WHEN ArraySy | StringSy=> -- ArraySy or StringSy
          StrArray := (Sy = StringSy);
          InSymbol;
          IF  Sy = LParent THEN
            InSymbol
          ;ELSE
            Error(9);
            IF  Sy = LBrack THEN InSymbol; END IF;
          END IF;
          TP := Arrays;
          ArrayTyp(RF, Sz, StrArray);

      WHEN RecordSy=>

        InSymbol;
        EnterBlock(T);
        TP := Records;
        RF := B;
        IF  Level = LMax THEN Fatal(5); END IF;
        Level := Level + 1;
        Display(Level) := B;
        Offset := 0;

        WHILE  NOT FSys_gnagna(Sy) LOOP

          IF Sy = IDent THEN  -- field section
            T0 := T;
            EnterVariable;
            WHILE  Sy = Comma  LOOP
              InSymbol;
              EnterVariable;
            END LOOP;

            IF  Sy = Colon THEN
              InSymbol;
            ELSE
              Error(5);
            END IF;
              T1 := T;
              TYP(FSys + Symset'(Semicolon | EndSy | Comma | IDent=>true,
                                 others=> false), ELTP, ELRF, ELSZ);
              WHILE  T0 < T1  LOOP
                T0 := T0 + 1;
                DECLARE r: TabEntry RENAMES IdTab(T0);
                BEGIN
                  r.TYP := ELTP;
                  r.Ref := ELRF;
                  r.Adr := Offset;
                  Offset := Offset + ELSZ;
                END;
              END LOOP;
          END IF;
            IF  Sy /= EndSy THEN
              IF  Sy = Semicolon THEN
                InSymbol;
              ELSE
                Error(14);
                IF  Sy = Comma THEN InSymbol; END IF;
              END IF;

              Test( Symset'(IDent | EndSy | Semicolon=>true,
                            others=> false), FSys, 6);
            END IF;
          END LOOP;

          BlockTab(RF).VSize := Offset;
          Sz := Offset;
          BlockTab(RF).PSize := 0;
          InSymbol;
          IF  Sy = RecordSy THEN InSymbol; ELSE Error(61);END IF;
          Level := Level - 1;
      -- end of RecordSy

      WHEN LParent=>    -- Enumeration Type
          -- Hathorn
          TP := Enums;
          RF := T;
          ECount := 0;
          LOOP
            InSymbol;
            IF  (Sy = IDent) THEN
              ECount := ECount + 1;
              Enter(Id, Konstant);
              IdTab(T).TYP := Enums;
              IdTab(T).Ref := RF;
              IdTab(T).Adr := ECount;
            ELSE
              Error(6);END IF;
            InSymbol;
           EXIT WHEN  Sy /= Comma;
          END LOOP;

          Sz := ECount;
          IF Sy = RParent THEN InSymbol; ELSE Skip(Semicolon, 4); END IF;
      -- end of Enumeration Type

        WHEN others=> null;

      END CASE; -- Sy
      Test(FSys, Empty_Symset, 6);
    END IF;
  END TYP;

  ------------------------------------------------------------------
  ----------------------------------------------------ParameterList-
  PROCEDURE ParameterList IS  -- formal parameter list
    RF, Sz, X, T0:  Integer;
    TP:             Types:= NOTYP;
    ValParam:       Boolean;
  BEGIN
    InSymbol;
    RF := 0;
    Sz := 0;
    Test( Symset'(IDent=> true, others=> false), FSys + RParent, 7);
    WHILE  Sy = IDent  LOOP
      T0 := T;
      EnterVariable;
      WHILE  Sy = Comma  LOOP
        InSymbol;
        EnterVariable;
      END LOOP;

      IF  Sy = Colon THEN
        InSymbol;
        IF  Sy = InSy THEN InSymbol; END IF;
        IF IsFun THEN -- if I am a function, no InOut parms allowed
          ValParam := True;
        ELSIF Sy /= OutSy THEN
          ValParam := True;
        ELSE
          InSymbol;
          ValParam := False;
        END IF;
        IF  Sy /= IDent THEN
          Error(2);
        ELSE
          X := LOC(Id);
          InSymbol;
          IF  X /= 0 THEN
            DECLARE r: TabEntry RENAMES IdTab(X);
            BEGIN
              IF r.Obj /= TypeMark THEN
                Error(29);
              ELSE
                TP := r.TYP;
                RF := r.Ref;
                IF  ValParam THEN Sz:= r.Adr;
                                                     ELSE Sz:= 1; END IF;
              END IF;
            END;
          END IF; -- X /= 0
        END IF;
          Test( Symset'(Semicolon | RParent | Comma | IDent=>true,
                        others=> false), FSys, 14);

        WHILE  T0 < T  LOOP
          T0 := T0 + 1;
          DECLARE r: TabEntry RENAMES IdTab(T0);
          BEGIN
            r.TYP := TP;
            r.Ref := RF;
            r.Normal := ValParam;
            r.Adr := Dx;
            r.LEV := Level;
            Dx := Dx + Sz;
          END;
        END LOOP; -- while T0 < T

      ELSE  -- if Sy /= Colon
        Error(5);
      END IF;
      IF  Sy /= RParent THEN
        IF  Sy = Semicolon THEN
          InSymbol;
        ELSE
          Error(14);
          IF  Sy = Comma THEN InSymbol; END IF;
        END IF;
        Test( Symset'(IDent=> true, others=> false), FSys + RParent, 6);
      END IF;
    END LOOP;  -- while Sy = IDent
    IF  Sy = RParent THEN
      InSymbol;
      Test( Symset'(IsSy | ReturnSy | Semicolon=> true, others=> false),
            FSys, 6);
    ELSE
      Error(4);
    END IF;
  END ParameterList;

  ------------------------------------------------------------------
  --------------------------------------------------TypeDeclaration-
  PROCEDURE TypeDeclaration IS
    TP: Types;
    RF, Sz, T1: Integer;
  BEGIN
    InSymbol;
    Test( Symset'(IDent=>     true, others=> false),
          Semicolon_set, 2);
    Enter(Id, TypeMark);
    T1 := T;
    InSymbol;
    IF  Sy = IsSy THEN InSymbol; ELSE Error(20); END IF;

    TP := NOTYP;
    RF := 0;
    Sz := 0;
    TYP( Symset'(Semicolon | Comma | IDent=> true,
                 others=> false) + FSys, TP, RF, Sz);

    DECLARE r: TabEntry RENAMES IdTab(T1);
    BEGIN
      r.TYP := TP;
      r.Ref := RF;
      r.Adr := Sz;
    END;
    TestSemicolon;
  END TypeDeclaration;

  ------------------------------------------------------------------
  -------------------------------------------------------Assignment-
  PROCEDURE Assignment(I: Integer) ;


  ------------------------------------------------------------------
  ---------------------------------------------------VarDeclaration-
  PROCEDURE VarDeclaration IS               -- modified Hathorn
  -- This procedure processes both Variable and Constant declarations.
    T0, T1, RF, Sz, T0i, LC0, LC1: Integer;
    TP:         Types;
    ConstDec, TypeID:  Boolean;
    C:          ConRec;
    -- Y:          Item;
  BEGIN
    WHILE Sy = IDent LOOP
      T0 := T;
      EnterVariable;
      WHILE Sy = Comma LOOP
        InSymbol;
        EnterVariable;
      END LOOP;

      IF Sy = Colon THEN -- ':'
        InSymbol;
      ELSE
        Error(5);
      END IF;
      T1 := T;

      IF Sy = Ident THEN  --MRC 6/91 from PC version
        I := LOC(Id);
      END IF;

      Test( TypeBegSys + ConstSy, Semicolon_set, 6);
      ConstDec := False;
      IF Sy = ConstSy THEN
        ConstDec := True;
        InSymbol;
      END IF;
      TypeID := False;
      IF  TypeBegSys(Sy) THEN
        TypeID := True;
        TYP( Symset'(Semicolon | Comma | IDent | BecomeS=> true,
                     others=> false) + FSys, TP, RF, Sz);
      END IF;
      Test( Symset'(BecomeS | EQL | Semicolon=> true,
                    others=> false), Empty_Symset , 6);
      IF Sy = EQL THEN
        Error(51);
        Sy := BecomeS;
      END IF;
      IF  ConstDec THEN
        IF Sy = BecomeS THEN
          InSymbol;
          KKonstant( Symset'(Semicolon | Comma | IDent=> true,
                             others=> false) + FSys, C);
        ELSE
          Error(51);
        END IF;
      END IF;
      T0i := T0;
      IF  ConstDec  OR  TypeID THEN        -- update identifier table
        WHILE T0 < T1 LOOP
          T0 := T0 + 1;
          DECLARE r: TabEntry RENAMES IdTab(T0);
          BEGIN
           CASE  ConstDec  IS
            WHEN True =>
                r.Obj := Konstant;
                r.TYP := C.TP;
                IF  C.TP /= Floats THEN
                  r.Adr := C.I;
                ELSE
                  EnterFloat(C.R);
                  r.Adr := C1;
                END IF;
            WHEN  False =>
                r.TYP := TP;
                r.Ref := RF;
                r.Adr := Dx;
                Dx := Dx + Sz;
           END CASE; -- ConstDec
          END;
        END LOOP; -- While T0 < T1
      END IF;
      IF NOT (ConstDec) AND  (Sy = BecomeS) THEN
        -- create Variable initialization ObjCode
        LC0 := LC;
        Assignment(T1);
        T0 := T0i;
        WHILE T0 < T1 - 1 LOOP
          T0 := T0 + 1;
          Emit2(0, IdTab(T0).LEV, IdTab(T0).Adr);
          Emit2(1, IdTab(T1).LEV, IdTab(T1).Adr);
          Emit(38);
        END LOOP;

        LC1 := LC;
        -- reset ObjCode pointer as if ObjCode had not been generated
        LC := LC0;
        -- copy ObjCode to end of ObjCode table
        ICode := ICode + (LC1 - LC0);      -- Size of ObjCode
        WHILE  (LC0 < LC1)  LOOP
          ObjCode(CMax) := ObjCode(LC0);
          CMax := CMax - 1;
          LC0 := LC0 + 1;
        END LOOP;

      END IF;
      TestSemicolon;
    END LOOP; -- While Sy = IDent

  END VarDeclaration;

  ------------------------------------------------------------------
  --------------------------------------------------ProcDeclaration-
  PROCEDURE ProcDeclaration IS
    IsFun:      Boolean;
    NewBlockID:     Alfa;
  BEGIN
    IsFun := (Sy = FuncSy);
    InSymbol;
    IF  Sy = IDent THEN
      NewBlockID := Id;
    ELSE
      Error(2);
      Id := "          ";
    END IF;
    IF  IsFun THEN Enter(Id, Funktion); ELSE Enter(Id, Prozedure); END IF;

    InSymbol;
    Block(FSys, IsFun, Level + 1, T);
    IF  IsFun THEN
      Emit1(33, 1);           -- Exit
    ELSE
      Emit1(32, CallSTDP);
    END IF;
  END ProcDeclaration;


  ------------------------------------------------------------------
  --------------------------------------------------TaskDeclaration-
  PROCEDURE TaskDeclaration IS          -- Hathorn
    I, T0:        Integer;
    TaskID, EntryID:  Alfa;
    saveLineCount : Integer;    -- Source line where Task appeared
  BEGIN

    saveLineCount := LineCount;

    InSymbol;
    IF  (Sy = BodySy) THEN          -- Task Body
      InSymbol;
      I := LOC(Id);
      TaskID := IdTab(I).Name;

      BlockTab(IdTab(I).Ref).SrcFrom := saveLineCount;  --(* Manuel *)

      InSymbol;
      Block(FSys, False, Level + 1, I);
      Emit1(32, CallSTDP);             -- Exit
    ELSE                    -- Task Specification
      IF Sy = IDent THEN
        TaskID := Id;
      ELSE
        Error(2);
        Id := "          ";
      END IF;
      TCount := TCount + 1;
      IF TCount > TaskMax THEN Fatal(8); END IF;

      Enter(TaskID, aTask);
      TskDefTab(TCount) := T;           -- point To identifier Table location
      EnterBlock(T);                -- of TaskID
      IdTab(T).Ref := B;
      InSymbol;
      IF  Sy = Semicolon THEN
        InSymbol;  -- Task with no entries
      ELSE  -- Parsing the Entry specs
        IF  Sy = IsSy THEN
          InSymbol;
        ELSE
          Error(20);
        END IF;
        IF  Level = LMax THEN
          Fatal(5);
        END IF;
        Level := Level + 1;
        Display(Level) := B;
        WHILE Sy = EntrySy  LOOP
          InSymbol;
          IF Sy = IDent THEN
            EntryID := Id;
          ELSE
            Error(2);
            Id := "          ";
          END IF;
          ECount := ECount + 1;
          IF ECount > EntryMax THEN Fatal(9); END IF;
          Enter(Id, aEntry);
          EntryTAB(ECount) := T;        -- point to identifier table location
          T0 := T;              -- of TaskID
          InSymbol;
          Block(FSys, False, Level + 1, T);
          IdTab(T0).Adr := TCount;
          IF  Sy = Semicolon THEN InSymbol; ELSE Error(14); END IF;
        END LOOP; -- Sy = EntrySy

        Level := Level - 1;
        TestEnd;
        IF  Sy = IDent  AND  Id = TaskID THEN
          InSymbol
        ;ELSE
          Skip(Semicolon, 22);
        END IF;
        TestSemicolon;
      END IF;
    END IF;
  END TaskDeclaration;

  ------------------------------------------------------------------
  -------------------------------------------------------Expression-
  PROCEDURE Expression(FSys: Symset;  X: IN OUT  Item) ;


  ------------------------------------------------------------------
  ---------------------------------------------------------Selector-
  PROCEDURE Selector(FSys: Symset;  V: IN OUT  Item) IS
    X:  Item;
    a, J:  Integer;
  BEGIN      -- Sy IN [LParent, Period]
    LOOP
      IF  Sy = Period THEN
        InSymbol;                -- field Selector
        IF  Sy /= IDent THEN
          Error(2);
        ELSE
          IF  V.TYP /= Records THEN
            Error(31);

          ELSE  -- search field identifier

            J := BlockTab(V.Ref).last;
            IdTab(0).Name := Id;
            WHILE  IdTab(J).Name /= Id  LOOP
              J := IdTab(J).Link;
            END LOOP;

            IF  J = 0 THEN
              Error(0);END IF;
            V.TYP := IdTab(J).TYP;
            V.Ref := IdTab(J).Ref;
            a := IdTab(J).Adr;
            IF  a /= 0 THEN
              Emit1(9, a);END IF;
          END IF;
          InSymbol;
        END IF;

      ELSE    -- array Selector

        IF  Sy /= LParent THEN Error(9); END IF;
        LOOP

          InSymbol;
          Expression(FSys + Symset'(Comma | RParent=> true, others=> false), X);
          IF  V.TYP /= Arrays THEN
            Error(28);
          ELSE
            a := V.Ref;
            IF  ArraysTab(a).InXTYP /= X.TYP THEN
              Error(26);
            ELSIF  ArraysTab(a).ELSize = 1 THEN
              Emit1(20, a);
            ELSE
              Emit1(21, a);
            END IF;
            V.TYP := ArraysTab(a).ELTYP;
            V.Ref := ArraysTab(a).ELREF;
          END IF;
         EXIT WHEN  Sy /= Comma;
        END LOOP;

        IF  Sy = RParent THEN
          InSymbol;
        ELSE
          Error(4);
          IF  Sy = RBrack THEN InSymbol; END IF;
        END IF;
      END IF;
     EXIT WHEN  NOT (Sy=LParent or else Sy=Period);
    END LOOP;

    IF  FSys = Semicolon_set THEN
      J := 14;
    ELSE
      J := 6;
    END IF;

    Test(FSys, Empty_Symset, J);
  END Selector;

  ------------------------------------------------------------------
  -------------------------------------------------------------Call-
  PROCEDURE Call(FSys: Symset; I, CallType: Integer) IS
  --****************************************************************
  -- Generate ObjCode for procedure or Task Entry Call
  -- CallType specifies type of Call
  --  = 0 then standard procedure Call,    CallSTDP
  --  = 1 then standard Task Entry Call,    CallSTDE
  --  = 2 then timed Task Entry Call,      CallTMDE
  --  = 3 then conditional Task Entry Call,   CallCNDE
  --****************************************************************
    X:        Item;
    LastP, CP, K:    Integer;
  BEGIN
    Emit1(18, I);                  -- mark stack
    LastP := BlockTab(IdTab(I).Ref).LastPar;
    CP := I;
    IF  Sy = LParent THEN BEGIN            -- actual parameter list
      LOOP

        InSymbol;
        IF  CP >= LastP THEN
          Error(39)
        ;ELSE BEGIN
          CP := CP + 1;
          IF  IdTab(CP).Normal THEN BEGIN      -- value parameter
            Expression(FSys + Symset'((Comma | Colon | RParent => true,
                                       others=> false)), X);
            IF  (X.TYP = IdTab(CP).TYP) THEN BEGIN
              IF  X.Ref /= IdTab(CP).Ref THEN BEGIN
                Error(36);
              END;
              ELSE IF  X.TYP = Arrays THEN
                Emit1(22, ArraysTab(X.Ref).Size)
              ;ELSE IF  X.TYP = Records THEN
                Emit1(22, BlockTab(X.Ref).VSize)
            ;END IF;END IF;END IF;END;
            ELSE IF  (X.TYP = Ints)  AND  (IdTab(CP).TYP = Floats) THEN
              Emit1(26, 0)
            ;ELSE IF  X.TYP /= NOTYP THEN BEGIN
                Error(36);
            END;
          END IF;END IF;END IF;END;
          ELSE BEGIN              -- Variable (Name) parameter
            IF  Sy /= IDent THEN
              Error(2)
            ;ELSE BEGIN
              K := LOC(Id);
              InSymbol;
              IF  K /= 0 THEN
                IF  IdTab(K).Obj /= Variable THEN
                  Error(37);END IF;
                X.TYP := IdTab(K).TYP;
                X.Ref := IdTab(K).Ref;
                IF  IdTab(K).Normal THEN
                  Emit2(0, IdTab(K).LEV, IdTab(K).Adr);
                ELSE
                  Emit2(1, IdTab(K).LEV, IdTab(K).Adr);
                END IF;
                IF  Sy = LParent or else Sy = Period THEN
                  Selector(FSys + Symset'((Comma|Colon|RParent=> true,
                                       others=> false)), X);
                END IF;
                IF  (X.TYP /= IdTab(CP).TYP)  OR  (X.Ref /= IdTab(CP).Ref) THEN
                  Error(36);
                END IF;
              END IF;
            END;END IF;
          END;END IF;
        END;END IF;
        Test( Symset'(Comma | RParent=> true,
                      others=> false), FSys, 6);
       EXIT WHEN  Sy /= Comma;
      END LOOP;

      IF  Sy = RParent THEN InSymbol; ELSE Error(4); END IF;

    END;END IF;
    IF  CP < LastP THEN Error(39); END IF;  -- too few actual parameters

    IF  CallType = CallSTDP THEN
      Emit2(19, CallType, BlockTab(IdTab(I).Ref).PSize - 1) -- 19 = Call
    ;ELSE
      Emit2(19, CallType, BlockTab(IdTab(I).Ref).PSize - 1); -- 19 = Call
      Emit1(32, CallType);            -- RETURN FROM Entry Call
    END IF;

    IF  IdTab(I).LEV < Level THEN Emit2(3, IdTab(I).LEV, Level); END IF;
  END Call;

  ------------------------------------------------------------------
  --------------------------------------------------------EntryCall-
  PROCEDURE EntryCall(FSys: Symset; I, CallType: Integer) IS -- Hathorn
    -- X:        Item;
    Addr, J:      Integer;
    -- HoldCode:      Order;
  BEGIN
    IF  Sy /= Period THEN
      Skip(Semicolon, 6);
    ELSE
      InSymbol;                  -- Task Entry Selector
      IF  Sy /= IDent THEN
        Skip(Semicolon, 2);
      ELSE
        J := BlockTab(IdTab(I).Ref).last;
        IdTab(0).Name := Id;
        WHILE  IdTab(J).Name /= Id  LOOP J := IdTab(J).Link; END LOOP;

        IF  J = 0 THEN Error(0); END IF;

        Addr := J;
        InSymbol;
        Call(FSys, Addr, CallType);
      END IF;
    END IF;
  END EntryCall;

  ------------------------------------------------------------------
  -------------------------------------------------------ResultType-
  FUNCTION  ResultType(a, B: Types) RETURN  Types IS
  BEGIN
    IF  a > Floats  OR  B > Floats THEN
      Error(33);
      return NOTYP;
    ELSIF  (a = NOTYP)  OR  (B = NOTYP) THEN
      return NOTYP;
    ELSIF  a = Ints THEN
      IF  B = Ints THEN
        return Ints;
      ELSE
        Emit1(26, 1);
        return Floats;
      END IF;
    ELSE
      IF  B = Ints THEN  Emit1(26, 0); END IF;
      return Floats;
    END IF;
  END ResultType;

  ------------------------------------------------------------------
  -------------------------------------------------------Expression-
  PROCEDURE Expression(FSys: Symset;  X: IN OUT  Item) IS
  --  Note: dynamic variables for Y have been used due to the
  --     constraints imposed upon local variables in recursion.
    TYPE ItemPtr IS ACCESS Item;  -- static > dynamic : SCHOENING
    procedure Dispose is new Unchecked_deallocation(item, itemptr);


    Y: ItemPtr;
    OP: KeyWSymbol; F: Integer;
    OperZ: constant Symset:=
             Symset'((EQL|NEQ|LSS|LEQ|GTR|GEQ=> true, others=> false));

    PROCEDURE SimpleExpression(FSys: Symset;  X: IN OUT  Item) IS
      Y:  ItemPtr;
      OP: KeyWSymbol;
      TermZ: constant Symset:=
               Symset'((Plus|MinUS|OrSy=> true, others=> false));

      PROCEDURE Term(FSys: Symset;  X: IN OUT  Item) IS
        Y:  ItemPtr;
        OP: KeyWSymbol;
        -- TS: TypSet;
        FactorZ: constant Symset:=
          Symset'(xTimes | Divide | ModSy | AndSy => true,
                  others=> false);

        PROCEDURE Factor(FSys: Symset;  X: IN OUT  Item) IS
          I, F: Integer;

          PROCEDURE StandFct(NS: Integer) IS
            TS: TypSet;
            N: Integer:= NS;
          BEGIN  -- STANDARD FUNCTION NO. N , N => 100 INDICATES
              -- a NILADIC FUNCTION.
            IF  N < 100 THEN
              IF  Sy = LParent THEN
                InSymbol
              ;ELSE
                Error(9);END IF;
              IF  (N < 17)  OR  (N = 19) THEN BEGIN
                Expression(FSys + RParent, X);
                CASE  N  IS

                 WHEN 0 |  2=>  -- abs, Sqr

                    TS := TypSet'((Ints | Floats=> true, others=> false));
                    IdTab(I).TYP := X.TYP;
                    IF  X.TYP = Floats THEN
                      N := N + 1;
                    END IF;

                  -- Odd, Chr
                  WHEN 4 |  5=> TS := TypSet'((Ints=> true, others=> false));

                  -- Ord
                  WHEN 6=> TS := TypSet'((Ints | Bools | xChars | Enums=> true,
                                          others=> false));

                  -- Succ,  Pred
                  WHEN 7 |  8=>
                    TS := TypSet'((Ints | Bools | xChars | Enums=> true,
                                   others=> false));
                    IdTab(I).TYP := X.TYP;

                  -- Round,Trunc
                  WHEN 9 |  10 |  11 |  12 |  13 |  14 |  15 |  16=>
                  -- Sin,Cos,...
                    TS := TypSet'((Ints | Floats=> true, others=> false));
                    IF  X.TYP = Ints THEN Emit1(26, 0);END IF;

                  -- Random
                  WHEN 19=>
                    TS := TypSet'((Ints=> true, others=> false));
                    IdTab(I).TYP := X.TYP;

                  WHEN others=> null;

                END CASE; -- N

                IF  TS(X.TYP) THEN
                  Emit1(8, N);
                ELSIF  X.TYP /= NOTYP THEN
                  Error(48);
                END IF;

              END;
              ELSE BEGIN          -- N in [17,18]
                -- EOF, Eoln
                IF  Sy /= IDent THEN
                  Error(2)
                ;ELSIF  Id = "INPUT     " THEN
                  Emit2(8, 0, N)
                ;ELSE
                  I := GetFP(Id);
                  IF  I = 0 THEN
                    Error(0)
                  ;ELSE
                    Emit2(8, I, N);END IF;
                END IF;
                InSymbol;
              END;END IF;            -- N in [17,18]
              X.TYP := IdTab(I).TYP;
              IF  Sy = RParent THEN
                InSymbol
              ;ELSE
                Error(4)
              ;END IF;
            ELSE            -- NILADIC FUNCTION
              CASE  N  IS
                WHEN 100=> Emit1(8, N); -- CLOCK
                WHEN others=> null;
              END CASE;

            END IF;    -- NILADIC FUNCTIONS, N => 100
          END StandFct;

        BEGIN  -- Factor
          X.TYP := NOTYP;
          X.Ref := 0;
          Test(FacBegSys + StrCon, FSys, 58);
          IF Sy = StrCon THEN
            X.TYP := Strings;
            Emit1(24, SLeng);        -- String Length
            Emit1(24, INUM);        -- pointer To String IdTab
            InSymbol;
          END IF;
          WHILE  FacBegSys(Sy)  LOOP

            CASE Sy is
              when IDent =>

              I := Loc(Id);
              InSymbol;
              DECLARE  r : Tabentry RENAMES  IdTab(I) ;
              BEGIN
                CASE  r.Obj  IS
                   WHEN Konstant=>
                    X.TYP := r.TYP;
                    X.Ref := r.Ref;
                    IF  X.TYP = Floats THEN
                      Emit1(25, r.Adr);
                    ELSE
                      Emit1(24, r.Adr);
                    END IF;

                  WHEN Variable=>
                    X.TYP := r.TYP;
                    X.Ref := r.Ref;
                    IF  Sy = LParent or else Sy = Period THEN
                      IF  r.Normal THEN
                        F := 0;
                      ELSE
                        F := 1;
                      END IF;
                      Emit2(F, r.LEV, r.Adr);
                      Selector(FSys, X);
                      IF  StanTyps(X.TYP) THEN
                        Emit(34);
                      END IF;
                    ELSE
                      IF  X.TYP = enums or else StanTyps(X.TYP) THEN
                        IF  r.Normal THEN
                          F := 1;
                        ELSE
                          F := 2;
                        END IF;
                      ELSIF  r.Normal THEN
                        F := 0;
                      ELSE
                        F := 1;
                      END IF;
                      Emit2(F, r.LEV, r.Adr);
                    END IF;

                  WHEN TypeMark |  Prozedure=>
                    Error(44);

                  WHEN Funktion=>
                    X.TYP := r.TYP;
                    IF  r.LEV /= 0 THEN
                      Call(FSys, I, CallSTDP);
                    ELSE
                      StandFct(r.Adr);
                    END IF;

                  WHEN others=> null;

                END CASE;
              END; -- WITH

            when CharCon | IntCon | FloatCon =>

              IF  Sy = FloatCon THEN
                X.TYP := Floats;
                EnterFloat(RNum);
                Emit1(25, C1);
              ELSE
                IF  Sy = CharCon THEN
                  X.TYP := xChars
                ;ELSE
                  X.TYP := Ints;END IF;
                Emit1(24, INUM);
              END IF;
              X.Ref := 0;
              InSymbol;

            when LParent =>    --  (
              InSymbol;
              Expression(FSys + RParent, X);
              IF  Sy = RParent THEN
                InSymbol
              ;ELSE
                Error(4);END IF;
            when NOTSy =>      --  NOT
              InSymbol;
              Factor(FSys, X);
              IF  X.TYP = Bools THEN
                Emit(35);
              ELSIF  X.TYP /= NOTYP THEN
                Error(32);
              END IF;

            WHEN others=> null;

            END CASE;

            IF  FSys = Semicolon_set THEN
              F:= 14;
            ELSE
              F:= 6;
            END IF;

            Test(FSys, FacBegSys, F);
          END LOOP;
        END Factor;

      BEGIN  -- Term
        Y:= New Item;
        Factor(FSys + FactorZ, X);
        WHILE FactorZ(sy) LOOP
          OP := Sy;
          InSymbol;
          Factor(FSys + FactorZ, Y.all);
          IF  OP = xTimes THEN     --  *
            X.TYP := ResultType(X.TYP, Y.all.TYP);
            CASE  X.TYP  IS
              WHEN NOTYP=>  null;
              WHEN Ints=>   Emit(57);
              WHEN Floats=> Emit(60);
              WHEN others=> null;
            END CASE;
          ELSIF OP = Divide THEN    --  /
            IF  (X.TYP = Ints)  AND  (Y.all.TYP = Ints) THEN
              Emit(58)
            ;ELSE
              IF  X.TYP = Ints THEN
                Emit1(26, 1);
                X.TYP := Floats;
              END IF;
              IF  Y.all.TYP = Ints THEN
                Emit1(26, 0);
                Y.all.TYP := Floats;
              END IF;
              IF  (X.TYP = Floats)  AND  (Y.all.TYP = Floats) THEN
                Emit(61)
              ;ELSE
                IF  (X.TYP /= NOTYP)  AND  (Y.all.TYP /= NOTYP) THEN
                  Error(33);END IF;
                X.TYP := NOTYP;
              END IF;
            END IF;
          ELSIF  OP = AndSy THEN      -- AND
            IF  (X.TYP = Bools)  AND  (Y.all.TYP = Bools) THEN
              Emit(56)
            ;ELSE BEGIN
              IF  (X.TYP /= NOTYP)  AND  (Y.all.TYP /= NOTYP) THEN
                Error(32);END IF;
              X.TYP := NOTYP
            ;END;END IF;
          ELSE            -- MOD  -  OP = ModSy
            IF  (X.TYP = Ints)  AND  (Y.all.TYP = Ints) THEN
              Emit(59)
            ;ELSE
              IF  (X.TYP /= NOTYP)  AND  (Y.all.TYP /= NOTYP) THEN
                Error(34);END IF;
              X.TYP := NOTYP;
            END IF;
          END IF;
        END LOOP;

        Dispose(Y);
      END Term;

    BEGIN  -- SimpleExpression
      Y:= New Item;

      -- +, -
      IF  Sy = Plus or else Sy = MinUS THEN
        OP := Sy;
        InSymbol;
        Term(FSys + Symset'((Plus|MinUS=> true,
                             others=> false)), X);
        IF  X.TYP > Floats THEN
          Error(33);
        ELSIF  OP = MinUS THEN
          Emit(36);
        END IF;
      ELSE
        Term(FSys + TermZ, X);
      END IF;
      WHILE  TermZ(Sy)  LOOP
        OP := Sy;
        InSymbol;
        Term(FSys + TermZ, Y.all);
        -- OR
        IF  OP = OrSy THEN
          IF  (X.TYP = Bools)  AND  (Y.all.TYP = Bools) THEN
            Emit(51);
          ELSE
            IF  (X.TYP /= NOTYP)  AND  (Y.all.TYP /= NOTYP) THEN
              Error(32);
            END IF;
            X.TYP := NOTYP;
          END IF;
        ELSE
          X.TYP := ResultType(X.TYP, Y.all.TYP);
          CASE  X.TYP  IS
            WHEN NOTYP=> null;
            WHEN Ints=>
              IF  OP = Plus THEN
                Emit(52)
              ;ELSE
                Emit(53);END IF;
            WHEN Floats=>
              IF  OP = Plus THEN
                Emit(54)
              ;ELSE
                Emit(55);
              END IF;
            WHEN others=> null;
          END CASE;

        END IF;
      END LOOP;

      Dispose(Y);
    END SimpleExpression;

  BEGIN  -- Expression
    Y:= New Item;
    SimpleExpression(FSys + OperZ, X);
    IF  OperZ(Sy) THEN
      OP := Sy;
      InSymbol;
      SimpleExpression(FSys, Y.all);
      IF  (X.TYP = Ints)  AND THEN (Y.all.TYP = Floats) THEN
        X.TYP := Floats;
        Emit1(26, 1);
      END IF;
      IF  (Y.all.TYP = Ints)  AND THEN (X.TYP = Floats) THEN
        Y.all.TYP := Floats;
        Emit1(26, 0);
      END IF;
      IF  X.TYP = Enums  AND THEN  Y.all.TYP = Enums AND THEN
        X.Ref /= Y.all.Ref THEN
       Error(35);
      END IF;
      IF X.TYP = Y.all.TYP THEN
        IF  X.TYP = Floats THEN  F:= 0; ELSE  F:= 6; END IF;
        CASE  OP  IS
          WHEN EQL=> Emit(39 + F);
          WHEN NEQ=> Emit(40 + F);
          WHEN LSS=> Emit(41 + F);
          WHEN LEQ=> Emit(42 + F);
          WHEN GTR=> Emit(43 + F);
          WHEN GEQ=> Emit(44 + F);
          WHEN others=> null;
        END CASE;
      ELSE
        Error(35);
      END IF;
      X.TYP := Bools;
    END IF;
    Dispose(Y);
  END Expression;

  ------------------------------------------------------------------
  -------------------------------------------------------Assignment-
  PROCEDURE Assignment(I: Integer) IS
    X, Y:  Item;
    F:  Integer;
    -- IdTab[I].Obj = Variable
  BEGIN
    X.TYP := IdTab(I).TYP;
    X.Ref := IdTab(I).Ref;
    IF  IdTab(I).Normal THEN F:= 0; ELSE F:= 1; END IF;

    Emit2(F, IdTab(I).LEV, IdTab(I).Adr);
    IF  Sy = LBrack or else Sy = LParent or else Sy = Period THEN
      Selector( Symset'((BecomeS|EQL => true, others=> false)) + FSys, X);
    END IF;
    IF  Sy = BecomeS THEN
      InSymbol;
    ELSE
      Error(51);
      IF  Sy = EQL THEN  InSymbol; END IF;
    END IF;
    Expression(Semicolon_set, Y);
    IF  X.TYP = Y.TYP THEN
      IF  StanTyps(X.TYP) THEN
        Emit(38)
      ;ELSIF  X.Ref /= Y.Ref THEN
        Error(46)
      ;ELSE
        CASE  X.TYP  IS
         WHEN Arrays=> Emit1(23, ArraysTab(X.Ref).Size);
         WHEN Records=>Emit1(23, BlockTab(X.Ref).VSize);
         WHEN Enums=>  Emit(38);
         WHEN others=> null;
        END CASE;
      END IF;
    ELSIF  (X.TYP = Floats)  AND  (Y.TYP = Ints) THEN
      Emit1(26, 0);
      Emit(38);
    ELSIF  (X.TYP = Arrays)  AND  (Y.TYP = Strings) THEN
      IF  ArraysTab(X.Ref).ELTYP /= xChars THEN
        Error(46);
      ELSE
        Emit1(67, ArraysTab(X.Ref).Size);       -- array Size
      END IF;
    ELSIF  (X.TYP /= NOTYP)  AND  (Y.TYP /= NOTYP) THEN
      Error(46);
    END IF;
  END Assignment;

  ------------------------------------------------------------------
  --------------------------------------------------------Statement--
  PROCEDURE Statement(FSys: Symset) IS
    I: Integer;

    PROCEDURE MultiStatement(Sentinal: Symset) IS   -- Hathorn
     nxtSym: Symset;
     BEGIN
      nxtSym := Sentinal + StatBegSys;
      LOOP
       Statement(nxtSym); --MRC,was: UNTIL (Sy IN Sentinal);
       EXIT WHEN  Sentinal(SY)  OR  Err_count > 0;
      END LOOP;
    END MultiStatement;

    PROCEDURE AcceptStatement IS            -- Hathorn
      I:  Integer;

      PROCEDURE AcceptCall(FSys: Symset; I: Integer) IS
      BEGIN -- check To make sure parameters match with Entry Statement
        IF  Sy = Semicolon THEN
          return; -- Exit(AcceptCall);
        END IF;
        IF  Sy = LParent THEN          -- <--- temporary
          WHILE NOT (Sy = doSy or Sy = RParent) LOOP
            InSymbol;
          END LOOP; -- should check no. and
        END IF;    -- Types of parms.
        IF Sy = RParent THEN
          InSymbol;
        END IF;
      END AcceptCall;

    BEGIN  -- AcceptStatement
      InSymbol;
      I := LOC(Id);
      IF IdTab(I).Obj /= aEntry THEN
        Error(70);
      END IF;
      InSymbol;
      AcceptCall(FSys, I);
      Emit1(4, I);                -- Accept rendezvous opcode = 4
      IF Sy = doSy THEN
        IF Level = LMax THEN
          Fatal(5);
        END IF;
        Level := Level + 1;
        Display(Level) := IdTab(I).Ref;
        InSymbol;
        MultiStatement(Symset'((EndSy=> true, others=> false)));
        TestEnd;
        IF Sy = IDent THEN
          IF  Id /= IdTab(I).Name THEN Error(22); END IF;
          InSymbol;
        END IF;
        Level := Level - 1;
      END IF;
      Emit1(5, I);                -- End rendezvous opcode = 5
    END AcceptStatement;

    PROCEDURE CompoundStmnt IS           -- modified Hathorn
    BEGIN
      InSymbol;
      MultiStatement(Symset'((EndSy=> true, others=> false)));
      TestEnd;
      IF Sy = IDent THEN
        IF Id /= IdTab(Prt).Name THEN
          Error(22);
        END IF;
        InSymbol;
      END IF;
    END CompoundStmnt ;

    PROCEDURE ExitStatement  IS          -- Hathorn
    -- Generate an absolute branch Statement with a dummy end loop address
      X:  Item;
    BEGIN
      IF Sy = ExitSy THEN
        InSymbol;
      ELSE
        Skip(Semicolon, 6);
      END IF;

      IF Sy = WhenSy THEN      -- conditional Exit
        InSymbol;
        Expression(Semicolon_set, X);
        IF NOT (X.TYP = Bools or X.TYP = NOTYP) THEN
          Error(17);
        END IF;
        Emit1(11, LC + 2);            -- conditional jump around Exit
      END IF;
      Emit1(10, - 1);       -- unconditional jump with address To be patched
    END ExitStatement;

    PROCEDURE IfStatement IS
      X:  Item;
      LC0, LC1:  Integer;
    BEGIN
      InSymbol;
      Expression(FSys + Symset'((ThenSy | doSy=> true, others=> false)), X);
      IF  NOT (X.TYP = Bools or else X.TYP = NOTYP) THEN
        Error(17);
      END IF;
      LC1 := LC;
      Emit(11);                  -- JMPC
      IF  Sy = ThenSy THEN
        InSymbol;
      ELSE
        Error(52);
        IF Sy = doSy THEN
          InSymbol;
        END IF;
      END IF;
      MultiStatement(Symset'((ElsIfSy|ElseSy|EndSy=> true, others=> false)) );
      LC0 := LC;
      WHILE  Sy = ElsIfSy  LOOP     -- Added Hathorn
        InSymbol;
        Emit1(10, - 1);             -- unconditional jump with address To be
--                             patched
        ObjCode(LC1).Y := LC;           -- patch the previous conditional jump
        Expression(FSys + Symset'((ThenSy | doSy=> true, others=> false)), X);
        IF  NOT (X.TYP = Bools or else X.TYP = NOTYP) THEN
          Error(17);
        END IF;
        LC1 := LC;
        Emit(11);                -- JMPC
        IF  Sy = ThenSy THEN
          InSymbol
        ;ELSE
          Error(52);
          IF  Sy = doSy THEN InSymbol; END IF;
        END IF;
        MultiStatement(Symset'((ElsIfSy | ElseSy | EndSy=> true,
                                others=> false)) );
      END LOOP;

      IF  Sy = ElseSy THEN
        InSymbol;
        Emit1(10, - 1);
        ObjCode(LC1).Y := LC;
        MultiStatement(Symset'((EndSy=> true, others=> false)) );
      ELSE
        ObjCode(LC1).Y := LC;
      END IF;
      IF  Sy = EndSy THEN InSymbol; ELSE Error(57); END IF;  -- Added Hathorn
      IF  Sy = IfSy  THEN InSymbol; ELSE Error(62); END IF;

      -- Go back and patch the dummy addresses in unconditional jumps
      WHILE  LC0 < LC  LOOP
        IF  (ObjCode(LC0).Y = - 1) THEN
          ObjCode(LC0).Y := LC;END IF;
        LC0 := LC0 + 1;
      END LOOP;
    END IfStatement;

    PROCEDURE LoopStatement(FCT, B: Integer) IS    -- Hathorn
      LC0: Integer:= LC;
    BEGIN
      IF  Sy = LoopSy THEN InSymbol; ELSE Skip(StatBegSys, 62); END IF;
      MultiStatement( Symset'((EndSy=> true, others=> false)) );
      Emit1(FCT, B);
      IF  Sy = EndSy THEN InSymbol; ELSE Error(57); END IF;
      IF  Sy = LoopSy THEN InSymbol; ELSE  Error(54); END IF;

      -- Go back and patch the dummy addresses generated by Exit stmts.
      WHILE  LC0 < LC  LOOP
        IF  (ObjCode(LC0).Y = - 1) THEN
          ObjCode(LC0).Y := LC;END IF;
        LC0 := LC0 + 1;
      END LOOP;
    END LoopStatement;

    PROCEDURE ReturnStatement            -- Hathorn
    -- Generate a procedure return Statement, calculate return value if req'D

       IS

        X, Y:        Item;
        F:          Integer; BEGIN

      IF  (BlockID = ProgramID) THEN
        Error(45);END IF;
      I := LOC(BlockID);
      InSymbol;
      IF  Sy = Semicolon  AND  IsFun THEN Error(68);END IF;

      IF  Sy /= Semicolon THEN          -- calculate return value
        IF  IdTab(I).Ref = Display(Level) THEN
          X.TYP := IdTab(I).TYP;
          X.Ref := IdTab(I).Ref;
          IF  IdTab(I).Normal THEN F:= 0; ELSE F:= 1; END IF;

          Emit2(F, IdTab(I).LEV + 1, 0);
          Expression(Semicolon_set, Y);
          IF  X.TYP = Y.TYP THEN
            IF  StanTyps(X.TYP) THEN
              Emit(38)
            ;ELSIF  X.Ref /= Y.Ref THEN
              Error(46)
            ;ELSIF  X.TYP = Floats  AND  Y.TYP = Ints THEN
              Emit1(26, 0);
              Emit(38);
            ELSIF  X.TYP /= NOTYP  AND  Y.TYP /= NOTYP THEN
              Error(46);
            END IF;
          END IF;
        ELSE
          Error(45);
        END IF;
      END IF;
      Emit1(32 + Boolean'pos(IsFun), CallSTDP);      -- unconditional procedure Exit
    END ReturnStatement;

    PROCEDURE DelayStatement IS            -- Cramer
    -- Generate a Task delay
      Y: Item;
    BEGIN
      InSymbol;
      IF  Sy = Semicolon THEN
        Skip(Semicolon, 72)
      ;ELSE                  -- calculate delay value
        Expression(Semicolon_set, Y);
        IF  Y.TYP /= Floats THEN Error(73); END IF;
      END IF;
      Emit(68);                  -- Task delay instruction
    END DelayStatement;

    PROCEDURE CaseStatement IS
      X:      Item;
      I, J, LC1:  Integer;
      type GrounfZ is RECORD Val, LC:Index; END RECORD;
      CaseTab:  ARRAY ( 1..CSMax ) OF  GrounfZ;
      ExitTab:  ARRAY ( 1..CSMax ) OF  Integer;

      PROCEDURE CaseLabel IS
        Lab: ConRec;
        K:   Integer;
      BEGIN

        KKonstant(FSys + Symset'((Alt | Finger=> true,
                                others=> false)), Lab);
        IF  Lab.TP /= X.TYP THEN
          Error(47);
        ELSIF  I = CSMax THEN
          Fatal(6);
        ELSE
          I := I + 1;
          CaseTab(I).Val := Lab.I;
          CaseTab(I).LC := LC;
          K := 0;
          LOOP
            K := K + 1;
           EXIT WHEN CaseTab(K).Val = Lab.I;
          END LOOP;

          IF  K < I THEN Error(1); END IF;
                                   -- MULTIPLE DEFINITION
        END IF;
      END CaseLabel;

      PROCEDURE ONECASE IS
      BEGIN
        IF  Sy = WhenSy THEN
          InSymbol;
          IF  ConstBegSys(Sy) THEN
            CaseLabel;
            WHILE  Sy = Alt  LOOP
              InSymbol;
              CaseLabel;
            END LOOP;

          END IF;
          IF  Sy = OthersSy THEN        -- Hathorn
            IF  I = CSMax THEN
              Fatal(6)
            ;ELSE
              I := I + 1;
              CaseTab(I).Val := 0;
              CaseTab(I).LC := - LC;
              InSymbol;
            END IF;
          END IF;
          IF  Sy = Finger THEN
            InSymbol
          ;ELSE
            Error(64);END IF;
          MultiStatement( Symset'((WhenSy | EndSy=> true, others=> false)) );
          J := J + 1;
          ExitTab(J) := LC;
          Emit(10);
        ELSE
          Error(63);
        END IF;
      END ONECASE;

    BEGIN
      InSymbol;
      I := 0;
      J := 0;
      Expression(FSys + Symset'((OFSy | Comma | Colon=> true,
                                 others=> false)), X);

      IF  NOT (X.TYP = Ints   or else X.TYP = Bools or else
               X.TYP = xChars or else X.TYP = NOTYP) THEN
        Error(23);
      END IF;
      LC1 := LC;
      Emit(12);                  -- JMPX
      IF  Sy = OFSy THEN InSymbol; ELSE Error(8); END IF;

      WHILE  Sy = WhenSy  LOOP ONECASE; END LOOP;

      ObjCode(LC1).Y := LC;
      FOR  K  IN   1.. I  LOOP
        IF  (CaseTab(K).LC > 0) THEN
          Emit2(13, 1, CaseTab(K).Val);
          Emit1(13, CaseTab(K).LC);
        ELSE
          Emit2(13, - 1, CaseTab(K).Val);
          Emit1(13, - CaseTab(K).LC);
        END IF;
      END LOOP;

      Emit1(10, 0);
      FOR  K  IN   1.. J  LOOP ObjCode(ExitTab(K)).Y := LC; END LOOP;

      IF  Sy = EndSy  THEN InSymbol; ELSE Error(57); END IF;
      IF  Sy = CaseSy THEN InSymbol; ELSE  Error(65); END IF;
    END CaseStatement;

    PROCEDURE WhileStatement IS
      X:  Item;
      LC1, LC2:  Integer;
    BEGIN
      InSymbol;
      LC1 := LC;
      Expression(FSys + Symset'((LoopSy | doSy=> true,
                                 others=> false)), X);
      IF  NOT (X.TYP = Bools or else X.TYP = NOTYP) THEN
        Error(17);
      END IF;
      LC2 := LC;
      Emit(11);
      LoopStatement(10, LC1);
      ObjCode(LC2).Y := LC;
    END WhileStatement ;

    ------------------------------------------------------------ForStatement
    PROCEDURE ForStatement IS
      -- CVT:  Types;
      X:    Item;
      F, LC1, last: Integer;
    BEGIN
      InSymbol;
      IF  Sy = IDent THEN
        IF  T = TMax THEN
          Fatal(1);
        ELSE BEGIN
          -- declare local loop control Variable  --  added Hathorn
          last := BlockTab(Display(Level)).last;
          T := T + 1;

          DECLARE  r : TabEntry RENAMES  IdTab(T);
          BEGIN
            r.Name   := Id;
            r.Link   := last;
            r.Obj    := Variable;
            r.TYP    := NOTYP;
            r.Ref    := 0;
            r.Normal := True;
            r.LEV    := Level;
            r.Adr    := Dx;
          END;
          BlockTab(Display(Level)).last := T;
          Dx := Dx + 1;
          IF  Dx > MaxDX THEN
            MaxDX := Dx;
          END IF;
          BlockTab(Display(Level)).VSize := MaxDX;
        END;END IF;
      ELSE
        Skip( Symset'((InSy | RangeSy | LoopSy | EndSy=> true,
                       others=> false)) + FSys, 2);
      END IF;

      Emit2(0, IdTab(T).LEV, IdTab(T).Adr);
      InSymbol;
      F := 14;
      IF  Sy = InSy THEN BEGIN
        InSymbol;
        IF  (Sy = ReverseSy) THEN
          F := 16;
          InSymbol;
        END IF;
        Expression( Symset'((RangeSy | LoopSy | EndSy=> true,
                    others=> false)) + FSys, X);
        IdTab(T).TYP := X.TYP;
        IF  NOT (X.TYP = Ints or else X.TYP = Bools or else X.TYP = xChars) THEN
          Error(18);
        END IF;
        IF  Sy = RangeSy THEN
          InSymbol;
          Expression(FSys + LoopSy, X);
          IF  (IdTab(T).TYP /= X.TYP) THEN
            Error(19);END IF;
        ELSE
          Skip( Symset'((LoopSy | EndSy | Semicolon=> true,
                         others=> false)) + FSys, 55);END IF;
      END;
      ELSE
        Skip( FSys + LoopSy, 53);END IF;
      LC1 := LC;
      Emit(F);
      LoopStatement(F + 1, LC);
      ObjCode(LC1).Y := LC;
      T := T - 1;
      BlockTab(Display(Level)).last := last;
      Dx := Dx - 1;
    END ForStatement;

    PROCEDURE SelectStatement IS
      PROCEDURE SelectError(N: Integer) IS BEGIN Skip(Semicolon, N); END;                    -- SelectError

      -- Either a Timed or Conditional Entry Call.

      PROCEDURE QualifiedEntryCall IS
        I, J, IStart, IEnd: Integer;
        patch:  ARRAY ( 0..4 ) OF  Integer;
        O: Order;
        Y: Item;
      BEGIN
        I := LOC(Id);
        IF  IdTab(I).Obj = aTask THEN
          InSymbol;
          EntryCall(FSys, I, - 1);
          IF  ObjCode(LC - 2).F = 19 THEN     -- need To patch CallType later
            patch(0) := LC - 2
          ;ELSE
            patch(0) := LC - 3;END IF;       -- LC-1 must be OP=3, update Display
          patch(1) := LC;           -- need To patch in JMPC address later
          Emit1(11, - 1);           -- JMPC, address patched in after ELSE or OR
          IF  Sy /= Semicolon THEN
            Skip(Semicolon, 14)
          ;ELSE
            InSymbol;END IF;
          IF  NOT (Sy = OrSy or else Sy = ElseSy) THEN
            MultiStatement( Symset'((OrSy | ElseSy=> true, others=> false)) );
          END IF;
          IF  Sy = OrSy THEN BEGIN       -- =====================> Timed Entry Call
            ObjCode(patch(0)).X := CallTMDE; -- Timed Entry Call
            ObjCode(patch(0) + 1).Y := CallTMDE; -- Exit type matches Entry type
            InSymbol;
            IF  Sy = DelaySy THEN
              InSymbol;
              IF  (Sy = Semicolon) THEN
                SelectError(72);
              ELSE          -- calculate delay value
                patch(2) := LC;
                Expression( Semicolon_set, Y);
                patch(3) := LC - 1;
                IF  Y.TYP /= Floats THEN
                  SelectError(73);
                ELSE        -- end of timed Entry select ObjCode, do patching
                  ObjCode(patch(1)).Y := LC; -- if Entry not made, Skip rest
                  J := patch(3) - patch(2) + 1;
                  IStart := patch(0);
                  IEnd := LC - 1;
                  WHILE  J > 0  LOOP     -- move delay time ObjCode To before
                    O := ObjCode(IEnd);  -- opcodes 19 Call, 32 return
                    FOR  I  IN  REVERSE IEnd-1.. IStart  LOOP
                      ObjCode(I + 1) := ObjCode(I);
                    END LOOP;
                    ObjCode(IStart) := O;
                    J := J - 1;
                  END LOOP;

                  InSymbol;
                END IF;
              END IF;
            ELSE
              SelectError(79);
            END IF;      -- DELAY EXPECTED
          END;                 -- Sy = OrSy
          ELSE             -- Sy = ElseSy, ===============> Conditional
--                             Entry Call
            ObjCode(patch(0)).X := CallCNDE; -- Conditional Entry Call
            ObjCode(patch(0) + 1).Y := CallCNDE;
            patch(2) := LC;
            Emit1(10, - 1);         -- JMP, address patched in after END SELECT
            patch(3) := LC;
            InSymbol;
            MultiStatement( Symset'((EndSy=> true, others=> false)));
            ObjCode(patch(1)).Y := patch(3);
            ObjCode(patch(2)).Y := LC;
          END IF;
          IF  Sy /= EndSy THEN
            SelectError(57);END IF;
          InSymbol;
          IF  Sy /= SelectSy THEN
            SelectError(80);
          END IF;
        ELSE
          SelectError(77);
        END IF;          -- Task.Entry Call expected
      END QualifiedEntryCall;

      PROCEDURE SelectiveWait IS         -- Kurtz <===================
      -- Jay, this Buds for you !!

        TYPE   patch_ptr   IS   ARRAY ( 1..10 ) OF  Integer;

        JSD, Alt:      patch_ptr;
        ISD, IAlt, StartSel: Integer;
        SelectDone: Boolean;
        Y, X:        Item;
        do_terminate : boolean;

        PROCEDURE AcceptStatement2       -- Kurtz
         IS

          I: Integer;

          PROCEDURE AcceptCall2(FSys: Symset; I: Integer) IS
           BEGIN
            -- check To make sure parameters match with Entry Statement
            IF  Sy = Semicolon THEN
              return; -- Exit(AcceptCall2);
            END IF;
            IF  Sy = LParent THEN      -- should be modified
              -- To check no. and
              WHILE  NOT (Sy = doSy or else Sy = RParent)  LOOP
                InSymbol;
              END LOOP;
            END IF;        -- of parameters.
            IF  Sy = RParent THEN InSymbol; END IF;
          END AcceptCall2;

        BEGIN                -- AcceptStatment2
          InSymbol;
          I := LOC(Id);
          IF  IdTab(I).Obj /= aEntry THEN
            SelectError(70);
          END IF;
          InSymbol;
          AcceptCall2(FSys, I);
          Emit2(73, 2, I);          -- Retain Entry Index
          IF  (IAlt < 10) THEN
            IAlt := IAlt + 1
          ;ELSE
            Fatal(10);END IF;
          Alt(IAlt) := LC;          -- SAVE LOCATION FOR PATCHING
          Emit2(73, 3, LC);          -- ACCEPT IF Ready ELSE Skip To LC
          -- CONDITIONAL ACCEPT MUST BE ATOMIC
          IF  Sy = doSy THEN BEGIN
            IF  Level = LMax THEN
              Fatal(5);END IF;
            Level := Level + 1;
            Display(Level) := IdTab(I).Ref;
            InSymbol;
            MultiStatement( Symset'((EndSy=> true, others=> false)));
            TestEnd;
            IF  (Sy = IDent) THEN BEGIN
              IF  (Id /= IdTab(I).Name) THEN
                SelectError(22);END IF;
            END;END IF;
            Level := Level - 1;
            InSymbol;
          END;END IF;
          Emit1(5, I);            -- END RENDEZVOUS
        END;                  -- AcceptStatement2

      BEGIN          -- SelectiveWait ===============================> Kurtz
        ISD := 0;
        IAlt := 0;
        SelectDone := False;
        do_terminate := False ;
        StartSel := LC;
        Emit2(73, 1, 0);            -- START OF SELECT SELECTIVE Wait SEQUENCE
        LOOP

          CASE  Sy  IS
            WHEN
            WhenSy=> BEGIN
              FOR  I  IN   1.. IAlt  LOOP

                ObjCode(Alt(I)).Y := LC;END LOOP;
 -- patch
              IAlt := 0;
              InSymbol;          -- WHENSTATEMENT
              Expression(FSys + Finger, X);
              IF  NOT (X.TYP = Bools or else X.TYP = NOTYP) THEN
                SelectError(17);
              END IF;
              InSymbol;
              IF  Sy = AcceptSy THEN BEGIN
                IF  (IAlt > 10) THEN
                  Fatal(10)
                ;ELSE BEGIN
                  IAlt := IAlt + 1;
                  Alt(IAlt) := LC;
                  Emit(11);
                  AcceptStatement2;
                END;END IF;
              END;
              ELSE IF  (Sy = DelaySy) THEN BEGIN
                IF  (IAlt > 10) THEN
                  Fatal(10)
                ;ELSE BEGIN
                  IAlt := IAlt + 1;
                  Alt(IAlt) := LC;
                  Emit(11);
                  InSymbol;
                  Expression(FSys + Semicolon, Y);
                  Emit2(73, 4, LC + 2); -- Update delay time
                  IF  Y.TYP /= Floats THEN
                    SelectError(73);END IF;
                  IF  (IAlt > 10) THEN
                    Fatal(10);END IF;
                  IAlt := IAlt + 1;
                  Alt(IAlt) := LC;
                  Emit(10);
                END;END IF;
              END;
              ELSE
                SelectError(3);END IF;END IF;
              InSymbol;
              MultiStatement( Symset'((OrSy | ElseSy | EndSy=> true,
                                       others=> false)));
              IF  (ISD > 10) THEN
                Fatal(10);END IF;
              ISD := ISD + 1;
              JSD(ISD) := LC;
              Emit(10);          -- patch JMP ADDRESS AT EndSy
            END;              -- WhenSy

            WHEN AcceptSy=> BEGIN
              FOR  I  IN   1.. IAlt  LOOP

                ObjCode(Alt(I)).Y := LC;END LOOP;
 -- patch
              IAlt := 0;
              AcceptStatement2;
              InSymbol;
              MultiStatement( Symset'((OrSy | ElseSy | EndSy=> true,
                                       others=> false)));
              IF  (ISD > 10) THEN
                Fatal(10);END IF;
              ISD := ISD + 1;
              JSD(ISD) := LC;
              Emit(10);
            END;              -- AcceptSy

            WHEN OrSy=>       -- OR STATEMENT
              InSymbol;

            WHEN ElseSy=>
              FOR  I  IN   1.. IAlt  LOOP
    -- patch ObjCode
                ObjCode(Alt(I)).Y := LC;END LOOP;

              IAlt := 0;
              InSymbol;
              MultiStatement( Symset'((EndSy=> true, others=> false)) );
              IF  ISD > 10 THEN
                Fatal(10);END IF;
              ISD := ISD + 1;
              JSD(ISD) := LC;
              Emit(10);
            -- end ElseSy

            WHEN DelaySy=>
              FOR  I  IN   1.. IAlt  LOOP

                ObjCode(Alt(I)).Y := LC;END LOOP;
 -- patch
              IAlt := 0;
              -- Generate a Task delay, calculate return value if req'D
              InSymbol;
              IF  Sy = Semicolon THEN
                Skip(Semicolon, 72)
              ;ELSE BEGIN          -- calculate return value
                Expression( Symset'((Semicolon=> true, others=> false)), Y);
                Emit2(73, 4, LC + 2);  -- Update delay time
                IF  Y.TYP /= Floats THEN
                  SelectError(73);END IF;
                IF  (IAlt > 10) THEN
                  Fatal(10);END IF;
                IAlt := IAlt + 1;
                Alt(IAlt) := LC;
                Emit(10);
              END;END IF;
              InSymbol;
              MultiStatement( Symset'((OrSy | EndSy | ElseSy=> true,
                                       others=> false)));
              IF  (ISD > 10) THEN
                Fatal(10);END IF;
              ISD := ISD + 1;
              JSD(ISD) := LC;
              Emit(10);
            -- end DelaySy

            WHEN TerminateSy=>
              InSymbol;
              IF  Sy /= Semicolon THEN SelectError(14); END IF;
              do_terminate := True;        -- Oguz
              InSymbol;
            -- end TerminateSy

            WHEN EndSy=>
              InSymbol;
              IF  Sy /= SelectSy THEN SelectError(57); END IF;
              SelectDone := True;
              FOR  I  IN   1.. IAlt  LOOP
                ObjCode(Alt(I)).Y := LC;
              END LOOP;
 -- patch
              IAlt := 0;
              IF  do_terminate THEN
                Emit2(73, 5, StartSel);
              ELSE
                Emit2(73, 6, StartSel);
              END IF;   -- Suspend
              FOR  I  IN   1.. ISD  LOOP

                ObjCode(JSD(I)).Y := LC;END LOOP;
 -- patch
              ISD := 0;
            -- end EndSy

             WHEN OTHERS => SelectDone := True;
          END CASE;
         EXIT WHEN  SelectDone;
        END LOOP;

      END;                    -- SelectiveWait

      BEGIN                    -- Sy = SelectSy
      -- Next KeyWSymbol must be AcceptSy, WhenSy, or a Task Entry object Name.
      InSymbol;
      IF  Sy = AcceptSy or else Sy = WhenSy or else Sy = IDent THEN BEGIN
        IF  Sy = AcceptSy or else Sy = WhenSy THEN
          SelectiveWait;
        ELSE
          QualifiedEntryCall;END IF;         -- Timed or Conditional Entry Call
        InSymbol;
      END;
      ELSE
        SelectError(76);END IF;
    END;                      -- SelectStatement

    PROCEDURE StandProc(N: Integer) IS
      I, F:  Integer;
      X, Y:  Item;
    BEGIN
      CASE  N  IS
        WHEN  1 | 2 =>  -- GET

          IF  Sy = LParent THEN BEGIN
            InSymbol;
            I := GetFP(Id);         -- Schoening
            IF  (I /= - 1) THEN
              Emit1(64, I);
              InSymbol;
              IF  Sy /= Comma THEN
                IF  Sy = RParent THEN
                  InSymbol;
                ELSE
                  Error(2);
                END IF;
              END IF;
            ELSE
              Emit1(64, 0);
            END IF;

            LOOP

              InSymbol;
              IF  Sy /= IDent THEN
                Error(2)
              ;ELSE BEGIN
                I := LOC(Id);
                InSymbol;
                IF  I /= 0 THEN
                  IF  IdTab(I).Obj /= Variable THEN
                    Error(37);
                  ELSE
                    X.TYP := IdTab(I).TYP;
                    X.Ref := IdTab(I).Ref;
                    IF  IdTab(I).Normal THEN
                      F := 0
                    ;ELSE
                      F := 1;END IF;
                    Emit2(F, IdTab(I).LEV, IdTab(I).Adr);
                    IF  Sy = LParent or else Sy = Period THEN
                      Selector(FSys + Symset'((Comma | RParent=> true,
                                      others=> false)), X);
                    END IF;
                    IF  X.TYP = Ints   or else X.TYP = Floats or else
                        X.TYP = xChars or else X.TYP = NOTYP THEN
                      Emit1(27, Types'Pos(X.TYP));
                    ELSE
                      Error(41);
                    END IF;
                  END IF;END IF;
              END;END IF;

              Test(
                           Symset'((Comma | RParent=> true,
                                      others=> false)), FSys, 6);

             EXIT WHEN  Sy /= Comma;
            END LOOP;

            IF  Sy = RParent THEN
              InSymbol
            ;ELSE
              Error(4)
          ;END IF;END;END IF;
          IF  N = 2 THEN
            Emit(62);
          END IF;

        WHEN 3 | 4 =>          -- PUT

          IF  Sy = LParent THEN BEGIN
            InSymbol;
            I := GetFP(Id);         -- Schoening
            IF  I /= - 1 THEN
              Emit1(64, I);
              InSymbol;
              IF  Sy /= Comma THEN
                IF  Sy = RParent THEN
                  GOTO Label_21; -- skip the loop

                ELSE
                  Error(2);END IF;
              END IF;
            ELSE
              Emit1(64, 0);
              InSymbol;
            END IF;

            LOOP

              InSymbol;
              IF  Sy = StrCon THEN BEGIN
                Emit1(24, SLeng);
                Emit1(28, INUM);
                InSymbol;
              END;
              ELSE BEGIN
                Expression(FSys + Symset'((Comma | Colon | RParent=> true,
                                           others=> false)), X);
                IF  X.TYP = Enums THEN
                  X.TYP := Ints;END IF;
                IF  (NOT StanTyps(X.TYP))  AND  (X.TYP /= Strings) THEN
                  Error(41);
                END IF;
                IF  Sy = Colon THEN BEGIN
                  InSymbol;
                  Expression(FSys + Symset'((Comma | Colon | RParent=> true,
                                             others=> false)), Y);
                  IF  Y.TYP /= Ints THEN
                    Error(43);END IF;
                  IF  Sy = Colon THEN BEGIN
                    IF  X.TYP /= Floats THEN
                      Error(42);END IF;
                    InSymbol;
                    Expression(FSys + Symset'((Comma | RParent=> true,
                                               others=> false)), Y);
                    IF  Y.TYP /= Ints THEN
                      Error(43);END IF;
                    Emit(37)
                  ;END;
                  ELSE
                    Emit1(30, Types'Pos(X.TYP))
                ;END IF;END;
                ELSE IF  X.TYP = Strings THEN
                  Emit1(28, X.ref)
                ;ELSE
                  Emit1(29, Types'Pos(X.TYP))
              ;END IF;END IF;END;END IF;
             EXIT WHEN  Sy /= Comma;
            END LOOP;

          <<Label_21>>
            IF  Sy = RParent THEN
              InSymbol
            ;ELSE
              Error(4);END IF;
          END;END IF;
          IF  N = 4 THEN
            Emit(63);
          END IF;

        WHEN 5 |  6=>                  -- Wait,SIGNAL
          IF  Sy /= LParent THEN
            Error(9)
          ;ELSE BEGIN
            InSymbol;
            IF  Sy /= IDent THEN
              Error(0)
            ;ELSE BEGIN
              I := LOC(Id);
              InSymbol;
              IF  I /= 0 THEN
                IF  IdTab(I).Obj /= Variable THEN
                  Error(37)
                ;ELSE BEGIN
                  X.TYP := IdTab(I).TYP;
                  X.Ref := IdTab(I).Ref;
                  IF  IdTab(I).Normal THEN
                    F := 0
                  ;ELSE
                    F := 1;END IF;
                  Emit2(F, IdTab(I).LEV, IdTab(I).Adr);
                  IF  Sy = LParent or else Sy = Period THEN
                    Selector(FSys + RParent, X);END IF;
                  IF  X.TYP = Ints THEN
                    Emit(N + 1)    -- N is 5, or 6. Opcode is 6 or 7
                  ;ELSE
                    Error(43)
                ;END IF;END;END IF;
            END IF;END;END IF;
            IF  Sy = RParent THEN
              InSymbol
            ;ELSE
              Error(4)
          ;END IF;END;END IF;

        WHEN 7 |  8 | 9 =>    -- reset, Rewrite, Close
        -- Schoening
          IF  Sy /= LParent THEN
            Error(9);
          ELSE
            InSymbol;
            I := GetFP(Id);
            IF  I /= - 1 THEN
              Emit2(65, I, N)
            ;ELSE
              Error(2);END IF;
            InSymbol;
            IF  (Sy = RParent) THEN
              InSymbol
            ;ELSE
              Error(4);END IF;
          END IF;  -- reset

        WHEN 10=>        -- CursorAt
        -- Cramer
          IF  Sy /= LParent THEN
            Skip(Semicolon, 9)
          ;ELSE BEGIN
            InSymbol;
            Expression( Symset'((Comma | LParent | RParent |
                                 Colon | Semicolon=> true,
                                 others=> false)), X);
            IF  X.TYP /= Ints THEN
              Skip(Semicolon, 43);END IF;
            IF  Sy /= Comma THEN
              Skip(Semicolon, 74);
            ELSE
              InSymbol;
              Expression( Symset'((Comma | LParent | RParent |
                                   Colon | Semicolon=> true,
                                   others=> false)), X);
              IF  X.TYP /= Ints THEN
                Skip(Semicolon, 43);END IF;
              IF  Sy = Comma THEN
                Skip(Semicolon, 39)
              ;ELSE IF  Sy /= RParent THEN
                Skip(Semicolon, 4)
              ;ELSE
                Emit(69);        -- opcode for CursorAt
                InSymbol;
              END IF;END IF;
            END IF;
          END;END IF;                -- CursorAt

        WHEN 11=>                   -- Quantum
        -- Cramer
          IF  Sy /= LParent THEN
            Skip(Semicolon, 9)
          ;ELSE BEGIN
            InSymbol;
            Expression( Symset'((RParent=> true, others=> false)), X);
            IF  X.TYP /= Floats THEN
              Skip(Semicolon, 42);END IF;
            IF  Sy /= RParent THEN
              Skip(Semicolon, 4)
            ;ELSE
              Emit(70);          -- opcode for Set Quantum
              InSymbol;
            END IF;
          END;END IF;                -- Quantum

        WHEN 12=>                   -- Set Priority
        -- Cramer
          IF  Sy /= LParent THEN
            Skip(Semicolon, 9)
          ;ELSE BEGIN
            InSymbol;
            Expression( Symset'((RParent=> true, others=> false)), X);
            IF  X.TYP /= Ints THEN
              Skip( Semicolon, 43);
            END IF;
            IF  Sy /= RParent THEN
              Skip( Semicolon, 4)
            ;ELSE BEGIN
              Emit(71);          -- opcode for Set Priority
              InSymbol;
            END;END IF;
          END;END IF;                -- Priority

        WHEN 13=>                   -- Set Priority Inheritance,INHERITP
        -- Cramer
          IF  Sy /= LParent THEN
            Skip( Semicolon, 9);
          ELSE
            InSymbol;
            Expression( Symset'((RParent=> true, others=> false)), X);
            IF  X.TYP /= Bools THEN
              Skip(Semicolon, 75);END IF;
            IF  Sy /= RParent THEN
              Skip(Semicolon, 4)
            ;ELSE BEGIN
              Emit(72);          -- opcode for InheritP
              InSymbol;
            END;END IF;
          END IF;                -- Inheritp

          WHEN others=> null;

      END CASE;
    END StandProc;

  BEGIN  -- Statement
    IF  Err_count>0 THEN return; END IF;  --{MRC: added from PC version}

    --{ Mark the following opcodes as belonging to LineCount # }
    Emit1(74, LineCount);              --{MRC: this line is not in PC version}
    --{ This did not work because the LineCount was off by one. Why? }
    --{ MRC: This line is needed in order to highlight lines in task windows }

    IF StatBegSys(Sy) THEN
      CASE  Sy  IS
        WHEN IdEnt=>
          I := LOC(Id);
          InSymbol;
          IF  I /= 0 THEN
            CASE  IdTab(I).Obj  IS
              WHEN Konstant | TypeMark | Funktion => Error(16); Assignment(I);
              WHEN Variable                       => Assignment(I);
              WHEN aTask                   => EntryCall(FSys, I, CallSTDE);
              WHEN Prozedure=>
                IF  IdTab(I).LEV /= 0 THEN
                  Call(FSys, I, CallSTDP);
                ELSE
                  StandProc(IdTab(I).Adr);
                END IF;
              WHEN others=> null;
            END CASE;

          END IF;
        -- end IdEnt

        WHEN AcceptSy=>  AcceptStatement;
        WHEN BeginSy=>   CompoundStmnt;
        WHEN CaseSy=>    CaseStatement;
        WHEN DelaySy=>   DelayStatement;
        WHEN ExitSy=>    ExitStatement;
        WHEN ForSy=>     ForStatement;
        WHEN IfSy=>      IfStatement;
        WHEN LoopSy=>    LoopStatement(10, LC);
        WHEN NullSy=>    InSymbol;
        WHEN ReturnSy=>  ReturnStatement;
        WHEN SelectSy=>  SelectStatement;
        WHEN WhileSy=>   WhileStatement;
        WHEN others=>    null;
      END CASE;

      IF  NOT(EOFInput) THEN      --{MRC: added IF NOT... from PC version}
        IF  Sy = Semicolon THEN
          InSymbol;
        ELSE
          Error(14);
        END IF;
      END IF;
    END IF;  -- Sy in StatBegSys

    IF  NOT EofInput THEN
      Test(FSys - Semicolon, Semicolon_set, 6);
    END IF;

  END Statement;

BEGIN  -- Block
  IF Err_count > 0 THEN return; END IF;    --{MRC, from PC source}

  BlockID := IdTab(Prt).Name;
  Dx := 5;
  ICode := 0;
  IF  Level > LMax THEN
    Fatal(5);
    return;            --{MRC, from PC source}
  END IF;

  Test( Symset'(LParent | ReturnSy | IsSy | Semicolon => true,
                others=> false), FSys, 6);
  IF  IdTab(Prt).Ref > 0 THEN
    PRB := IdTab(Prt).Ref
  ;ELSE
    EnterBlock(Prt);
    PRB := B;
    IdTab(Prt).Ref := PRB;
  END IF;
  Display(Level) := PRB;
  IdTab(Prt).TYP := NOTYP;
  IF  Sy = LParent  AND  Level > 1 THEN
    ParameterList;
  END IF;

  IF  Err_count > 0 THEN return; END IF;    --{MRC, from PC source}

  BlockTab(PRB).LastPar := T;
  BlockTab(PRB).PSize := Dx;
  IF  IsFun THEN
    IF  Sy = ReturnSy THEN
      InSymbol;  -- FUNCTION TYPE
      IF  Sy = IDent THEN
        I := LOC(Id);
        InSymbol;
        IF  I /= 0 THEN
          IF  IdTab(I).Obj /= TypeMark THEN
            Error(29);
            return;  --{MRC, from PC source}

          ELSIF  StanTyps(IdTab(I).TYP) THEN
            IdTab(Prt).TYP := IdTab(I).TYP;
          ELSE
            Error(15);
            return;    --{MRC, from PC source}
          END IF;
        END IF;
      ELSE
        Skip( FSys + Semicolon, 2);
      END IF;
    ELSE
      Error(59);
      return;  --{MRC, from PC source}
    END IF;
  END IF;

  IF  Sy = Semicolon THEN  -- end of specification part
    BlockTab(PRB).VSize := Dx;
    IdTab(Prt).Adr := - 1;    -- address of body TBD
    return; -- Exit(Block);
  END IF;

  IF  Sy = IsSy THEN InSymbol; ELSE Error(20); return; END IF;

  LOOP
    IF  Sy = IDent  THEN  VarDeclaration;  END IF;
    IF  Sy = TypeSy THEN  TypeDeclaration; END IF;
    IF  Sy = TaskSy THEN  TaskDeclaration; END IF;
    BlockTab(PRB).VSize := Dx;

    WHILE Sy = ProcSy or else Sy = FuncSy LOOP ProcDeclaration; END LOOP;

   EXIT WHEN Sy = BeginSy;
  END LOOP;

  MaxDX := Dx;
  IdTab(Prt).Adr := LC;
  -- copy initialization ObjCode from end of ObjCode table
  I := CMax + ICode;
  WHILE  I > CMax  LOOP
    ObjCode(LC) := ObjCode(I);
    LC := LC + 1;
    I := I - 1;
  END LOOP;

  CMax := CMax + ICode;
  InSymbol;
  LOOP
    Statement( FSys + EndSy );
    IF  Err_count > 0 THEN Sy := EndSY; END IF;  --{MRC, from PC source}
   EXIT WHEN  Sy = EndSy OR Err_count > 0;
  END LOOP;
          --{MRC, added OR()... from PC source}
  BlockTab(PRB).SrcTo := LineCount;

  IF Sy = EndSy THEN InSymbol; ELSE Error(57); return; END IF; -- Missing END

  IF  Sy = IDent THEN
    IF  Id /= BlockID THEN Error(22); return; END IF;
    InSymbol;
  END IF;

  IF  Sy /= Semicolon THEN Error(14);  return; END IF;

  IF  BlockID /= ProgramID THEN InSymbol; Test(FSys, Empty_Symset, 6); END IF;

END Block;

END Parser;
