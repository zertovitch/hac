with HAC.Data;                        use HAC.Data;
with HAC.UErrors;                       use HAC.UErrors;
with HAC.Parser;                        use HAC.Parser;
with HAC.PCode;                         use HAC.PCode;
with HAC.Scanner;                       use HAC.Scanner;

with Ada.Text_IO;

package body HAC.Compiler is

  use Ada.Text_IO;
  package IIO is new Integer_IO(integer); use IIO;

  procedure Init_Tables is
  begin
    -- Arrays and blocks are clearly 1-based
    A:= 0;
    B:= 0;
    -- Identifiers
    T:= 0;
    -- Strings
    Sx:= 0;
    -- Tasks, Entries
    TCount:= 0;
    ECount:= 0;
    -- Location Counter (in output code)
    LC:= 0;
  end Init_Tables;

  PROCEDURE PrintTables IS
  BEGIN
    New_Line;
    Put(" Identifiers          Link  Obj  TYP  Ref  NRM  LEV  Adr");
    New_Line;
    FOR  I  IN   BlockTab(1).last.. T  LOOP
      DECLARE r: TabEntry RENAMES IdTab(I);
      BEGIN
        Put(I, 4); Put( ' '); Put( r.Name);
        Put( r.Link, 10);
        Put( aObject'Pos(r.Obj), 5);
        Put( Types'Pos(r.TYP), 5);
        Put( r.Ref, 5);
        Put( Boolean'Pos(r.Normal), 5);
        Put( r.LEV, 5);
        Put( r.Adr, 5);
        New_Line;
      END;
    END LOOP;

    New_Line;
    Put_Line(" Tasks       Block#");
    FOR I IN 0..TCount LOOP
      Put(I, 4);
      Put( ' ');
      Put( IdTab(TskDefTab(I)).Name);
      Put( "  ");
      Put(
          IdTab(TskDefTab(I)).Ref);
      New_Line;
    END LOOP;

    New_Line;

    IF  ECount > 0 THEN
      Put(" Entries ");
      New_Line;
      FOR  I  IN   1.. ECount  LOOP
        Put(I, 4);
        Put( ' ');
        Put( IdTab(EntryTAB(I)).Name);
        Put( "in Task ");
        Put(IdTab(TskDefTab(IdTab(EntryTAB(I)).Adr)).Name);
        New_Line;
      END LOOP;
      New_Line;
    END IF;

    Put_Line(" Blocks               last LPar PSze Vsze");
    -- There is a hidden block #0, "the Universe", with Standard
    FOR I IN 1..B LOOP
      DECLARE r: BTabEntry RENAMES BlockTab(I) ;
      BEGIN
        Put(I, 4); Put( ' '); Put( r.Id);
        Put( r.last, 10);
        Put( r.LastPar, 5);
        Put( r.PSize, 5);
        Put( r.VSize, 5);
        New_Line;
      END;
    END LOOP;

    New_Line;

    IF A > 0 THEN
      Put_Line(" Arrays    Xtyp Etyp Eref  Low High ELSZ Size");
      FOR I IN 1..A LOOP
        DECLARE r: ATabEntry RENAMES  ArraysTab(I);
        BEGIN
          Put(I, 4);
          Put( Types'Pos(r.InXTYP), 10);
          Put( Types'Pos(r.ELTYP), 5);
          Put( r.ELREF, 5);
          Put( r.Low, 5);
          Put( r.High, 5);
          Put( r.ELSize, 5);
          Put( r.Size, 5);
          New_Line;
        END;
      END LOOP;
    END IF;

  END PrintTables;


  ---------------------------------------------------------------------------

  PROCEDURE Compile IS

    PROCEDURE EnterStdFcns IS

      PROCEDURE Enter(X0: String; X1: aObject; X2: Types; X3: Integer) IS
        X0A: Alfa:= Empty_Alfa;
      BEGIN
        X0A(1..X0'Length):= X0;
        T := T + 1; -- Enter standard identifier
        IdTab(T):=
          (Name=> X0A, Link=> T - 1,
           Obj=> X1, TYP=> X2, Ref=> 0,
           Normal=> True,
           LEV=> 0, Adr=> X3);
      END Enter;

    BEGIN -- EnterStdFcns
	Enter("          ", Variable, NOTYP, 0);
	Enter("FALSE     ", Konstant, Bools, 0);
	Enter("TRUE      ", Konstant, Bools, 1);
	Enter("FLOAT     ", TypeMark, FloatS, 1);
	Enter("CHARACTER ", TypeMark, xChars, 1);
	Enter("BOOLEAN   ", TypeMark, Bools, 1);
	Enter("INTEGER   ", TypeMark, Ints, 1);
	Enter("STRING    ", TypeMark, Strings, 1);		--{ Hathorn }
	Enter("SEMAPHORE ", TypeMark, Ints, 1);			--{ Hathorn }
	Enter("TEXT      ", TypeMark, Ints, 1);			--{ Schoening }
	Enter("ABS       ", Funktion, FloatS, 0);
	Enter("SQR       ", Funktion, FloatS, 2);
	Enter("ODD       ", Funktion, Bools, 4);
	Enter("ASCII     ", Funktion, xChars, 5);
	Enter("ORD       ", Funktion, Ints, 6);
	Enter("SUCC      ", Funktion, xChars, 7);
	Enter("PRED      ", Funktion, xChars, 8);
	Enter("ROUND     ", Funktion, Ints, 9);
	Enter("TRUNC     ", Funktion, Ints, 10);
	Enter("SIN       ", Funktion, FloatS, 11);
	Enter("COS       ", Funktion, FloatS, 12);
	Enter("EXP       ", Funktion, FloatS, 13);
	Enter("LN        ", Funktion, FloatS, 14);
	Enter("SQRT      ", Funktion, FloatS, 15);
	Enter("ARCTAN    ", Funktion, FloatS, 16);
	Enter("EOF       ", Funktion, Bools, 17);
	Enter("EOLN      ", Funktion, Bools, 18);
	Enter("RANDOM    ", Funktion, Ints, 19); --{ Schoening }
	Enter("CLOCK     ", Funktion, FloatS, 100); --{ Cramer }
	--{ Niladic functions such as CLOCK will have   }
	--{ IdTab[].Adr >= 100 To differentiate them from }
	--{ functions with args.  See Parser.StandFct.  }
	Enter("GET       ", Prozedure, NOTYP, 1);
	Enter("GET_LINE  ", Prozedure, NOTYP, 2);
	Enter("PUT       ", Prozedure, NOTYP, 3);
	Enter("PUT_LINE  ", Prozedure, NOTYP, 4);
	Enter("NEW_LINE  ", Prozedure, NOTYP, 4); --{ Hathorn }
	Enter("WAIT      ", Prozedure, NOTYP, 5);
	Enter("SIGNAL    ", Prozedure, NOTYP, 6);
	Enter("RESET     ", Prozedure, NOTYP, 7); --{ Schoening }
	Enter("REWRITE   ", Prozedure, NOTYP, 8); --{ Schoening }
	Enter("CLOSE     ", Prozedure, NOTYP, 9); --{ Schoening }
	Enter("CURSORAT  ", Prozedure, NOTYP, 10); --{ Cramer }
	Enter("QUANTUM   ", Prozedure, NOTYP, 11); --{ Cramer }
	Enter("PRIORITY  ", Prozedure, NOTYP, 12); --{ Cramer }
	Enter("INHERITP  ", Prozedure, NOTYP, 13); --{ Cramer }
	Enter(ProgramID, Prozedure, NOTYP, 0);
    END EnterStdFcns;

  BEGIN -- Compile

    Init_Tables;
    cICompiler;

    IF ListingWasRequested THEN
      Create(Listing, name=> "compiler.lst");
      Put_Line(Listing, Header);
    END IF;

    if qDebug then
      Put_Line("Compiler: check for program heading");
    end if;

    CH:= ' ';
    InSymbol;
    IF Sy /= WithSy THEN   -- WITH SMALL_SP;
      Error(69);
    ELSE
      InSymbol;
      IF Sy /= IDent OR  Id /= "SMALL_SP  " THEN
        Error(69);
      ELSE
        InSymbol;
        IF  Sy /= Semicolon THEN Error(14); ELSE InSymbol; END IF;
      END IF;
    END IF;

    IF Sy /= UseSy THEN
      Error(70); -- USE SMALL_SP;
    ELSE
      InSymbol;
      IF  Sy /= IDent  OR  Id /= "SMALL_SP  " THEN
        Error(70);
      ELSE
        InSymbol;
        IF  Sy /= Semicolon THEN
          Error(14);
        ELSE
          InSymbol;
        END IF;
      END IF;
    END IF;

    if qDebug then
      Put_Line("Compiler: check for main procedure");
    end if;

    IF  Sy /= ProcSy THEN
      Error(3); -- PROCEDURE Name IS
    ELSE
      InSymbol;
      IF  Sy /= IDent THEN
        Error(2);
      ELSE
        ProgramID := Id;
        InSymbol;
      END IF;
    END IF;

    if qDebug then
      Put_Line("Compiler: main procedure is " & ProgramID);
    end if;

    EnterStdFcns; -- Enter Standard function ids and ProgramID

    BlockTab(0):= -- Block Table Entry for Standard [was Main, 1]
	(Id=> "Std Defns" & (10..Empty_Alfa'Length => ' '),
	 last=> T,
	 LastPar=> 1,
	 PSize=> 0,
	 VSize=> 0,
	 SrcFrom=> LineCount,
	 SrcTo=>   LineCount); -- ajout!
    Display(0):= 0; -- Added 7-Dec-2009

    TskDefTab(0) := T; --{ Task Table Entry }

    -- Start Compiling
    Block(BlockBegSyS + StatBegSys, False, 1, T);

    Emit(66); -- halt

    IF  Sy /= Semicolon THEN
      IF  qDebug THEN
        Put_Line("Compile terminated BEFORE FILE END");
      END IF;

      IF ListingWasRequested THEN
        Put_Line( "Compile terminated BEFORE FILE END");
      END IF;
    END IF;

    IF BlockTab(2).VSize > StMax - (STKINCR * TCount) THEN
      Error(49);
    END IF;
    BlockTab(1).SrcTo := LineCount;		--(* Manuel : terminate source *)
    IF  qDebug and Debug THEN
      PrintTables;
    END IF;
    IF  Errs /= Error_free THEN
          ErrorMsg;
		--{Close(ErrFile);}
		--{halt;}
    ELSIF  Map THEN
	  IF qDebug THEN
            New_Line;
	    Put_Line("  -* Symbol Table *-");
            New_Line;
	    Put_Line("  LOC  Name       scope");
            Put_Line("------------------------");
	    New_Line;
	    FOR  aTx  IN  BlockTab(1).last+1.. T LOOP
		Tx := aTx;
		IF  IdTab(Tx).Obj = Variable THEN
			IF  IdTab(Tx).TYP /= NOTYP THEN
				Put(IdTab(Tx).Adr, 4);
				Put(IdTab(Tx).Name, Alng + 3);
			END IF;
			IF  IdTab(Tx).LEV = 1 THEN
				Put(" Global(");
				Put( IdTab(Tx).LEV, 1);
				Put( ')');
				New_Line;
			ELSE
				Put(" Local (");
				Put( IdTab(Tx).LEV, 1);
				Put( ')');
				New_Line;
			END IF;
		END IF;
	    END LOOP;
	    New_Line;
	END IF;
    END IF;
	--{Close(ErrFile);}
  END Compile;

END HAC.Compiler;
