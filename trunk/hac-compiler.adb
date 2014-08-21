with HAC.Data;    use HAC.Data;
with HAC.UErrors; use HAC.UErrors;
with HAC.Parser;  use HAC.Parser;
with HAC.PCode;   use HAC.PCode;
with HAC.Scanner; use HAC.Scanner;

with Ada.Text_IO;

package body HAC.Compiler is

  use Ada.Text_IO;
  package IIO is new Integer_IO (Integer);
  use IIO;

  procedure Init_Tables is
  begin
    -- Arrays and blocks are clearly 1-based
    A := 0;
    B := 0;
    C2 := 0;
    -- Identifiers
    T := 0;
    -- Strings
    Sx := 0;
    -- Tasks, Entries
    TCount := 0;
    ECount := 0;
    -- Location Counter (in output code)
    LC := 0;
  end Init_Tables;

  procedure PrintTables is
  begin
    New_Line;
    Put (" Identifiers          Link  Obj  TYP  Ref  NRM  LEV  Adr");
    New_Line;
    for I in BlockTab (1).Last .. T loop
      declare
        r : TabEntry renames IdTab (I);
      begin
        Put (I, 4);
        Put (' ');
        Put (r.Name);
        Put (r.Link, 10);
        Put (aObject'Pos (r.Obj), 5);
        Put (Types'Pos (r.TYP), 5);
        Put (r.Ref, 5);
        Put (Boolean'Pos (r.Normal), 5);
        Put (r.LEV, 5);
        Put (r.Adr, 5);
        New_Line;
      end;
    end loop;

    New_Line;
    Put_Line (" Tasks       Block#");
    for I in 0 .. TCount loop
      Put (I, 4);
      Put (' ');
      Put (IdTab (TaskDefTab (I)).Name);
      Put ("  ");
      Put (IdTab (TaskDefTab (I)).Ref);
      New_Line;
    end loop;

    New_Line;

    if ECount > 0 then
      Put (" Entries ");
      New_Line;
      for I in 1 .. ECount loop
        Put (I, 4);
        Put (' ');
        Put (IdTab (EntryTAB (I)).Name);
        Put ("in Task ");
        Put (IdTab (TaskDefTab (IdTab (EntryTAB (I)).Adr)).Name);
        New_Line;
      end loop;
      New_Line;
    end if;

    Put_Line (" Blocks               last LPar PSze Vsze");
    -- There is a hidden block #0, "the Universe", with Standard
    for I in 1 .. B loop
      declare
        r : BTabEntry renames BlockTab (I);
      begin
        Put (I, 4);
        Put (' ');
        Put (r.Id);
        Put (r.Last, 10);
        Put (r.LastPar, 5);
        Put (r.PSize, 5);
        Put (r.VSize, 5);
        New_Line;
      end;
    end loop;

    New_Line;

    if A > 0 then
      Put_Line (" Arrays    Xtyp Etyp Eref  Low High ELSZ Size");
      for I in 1 .. A loop
        declare
          r : ATabEntry renames ArraysTab (I);
        begin
          Put (I, 4);
          Put (Types'Pos (r.Index_TYP), 10);
          Put (Types'Pos (r.Element_TYP), 5);
          Put (r.ELREF, 5);
          Put (r.Low, 5);
          Put (r.High, 5);
          Put (r.ELSize, 5);
          Put (r.Size, 5);
          New_Line;
        end;
      end loop;
    end if;

  end PrintTables;

  ---------------------------------------------------------------------------

  procedure Compile is

    procedure EnterStdFcns is

      procedure Enter
       (X0 : String;
        X1 : aObject;
        X2 : Types;
        X3 : Integer)
      is
        X0A : Alfa := Empty_Alfa;
      begin
        X0A (1 .. X0'Length) := X0;
        T                    := T + 1; -- Enter standard identifier
        IdTab (T)            :=
         (Name   => X0A,
          Link   => T - 1,
          Obj    => X1,
          TYP    => X2,
          Ref    => 0,
          Normal => True,
          LEV    => 0,
          Adr    => X3);
      end Enter;

    begin
      Enter ("",           Variable, NOTYP, 0);
      Enter ("FALSE",      Konstant, Bools, 0);
      Enter ("TRUE",       Konstant, Bools, 1);
      Enter ("FLOAT",      TypeMark, Floats, 1);
      Enter ("CHARACTER",  TypeMark, xChars, 1);
      Enter ("BOOLEAN",    TypeMark, Bools, 1);
      Enter ("INTEGER",    TypeMark, Ints, 1);
      Enter ("STRING",     TypeMark, Strings, 1);  --{ Hathorn }
      Enter ("SEMAPHORE",  TypeMark, Ints, 1);   --{ Hathorn }
      Enter ("TEXT",       TypeMark, Ints, 1);   --{ Schoening }
      Enter ("ABS",        Funktion, Floats, 0);
      Enter ("SQR",        Funktion, Floats, 2);
      Enter ("ODD",        Funktion, Bools, 4);
      Enter ("ASCII",      Funktion, xChars, 5);
      Enter ("ORD",        Funktion, Ints, 6);
      Enter ("SUCC",       Funktion, xChars, 7);
      Enter ("PRED",       Funktion, xChars, 8);
      Enter ("ROUND",      Funktion, Ints, 9);
      Enter ("TRUNC",      Funktion, Ints, 10);
      Enter ("SIN",        Funktion, Floats, 11);
      Enter ("COS",        Funktion, Floats, 12);
      Enter ("EXP",        Funktion, Floats, 13);
      Enter ("LN",         Funktion, Floats, 14);
      Enter ("SQRT",       Funktion, Floats, 15);
      Enter ("ARCTAN",     Funktion, Floats, 16);
      Enter ("EOF",        Funktion, Bools, 17);
      Enter ("EOLN",       Funktion, Bools, 18);
      Enter ("RANDOM",     Funktion, Ints, 19); --{ Schoening }
      Enter ("CLOCK",      Funktion, Floats, 100); --{ Cramer }
      --{ Niladic functions such as CLOCK will have   }
      --{ IdTab[].Adr >= 100 To differentiate them from }
      --{ functions with args.  See Parser.StandFct.  }
      Enter ("GET       ", Prozedure, NOTYP, 1);
      Enter ("GET_LINE  ", Prozedure, NOTYP, 2);
      Enter ("PUT       ", Prozedure, NOTYP, 3);
      Enter ("PUT_LINE  ", Prozedure, NOTYP, 4);
      Enter ("NEW_LINE  ", Prozedure, NOTYP, 4); --{ Hathorn }
      Enter ("WAIT      ", Prozedure, NOTYP, 5);
      Enter ("SIGNAL    ", Prozedure, NOTYP, 6);
      Enter ("RESET     ", Prozedure, NOTYP, 7); --{ Schoening }
      Enter ("REWRITE   ", Prozedure, NOTYP, 8); --{ Schoening }
      Enter ("CLOSE     ", Prozedure, NOTYP, 9); --{ Schoening }
      Enter ("CURSORAT  ", Prozedure, NOTYP, 10); --{ Cramer }
      Enter ("QUANTUM   ", Prozedure, NOTYP, 11); --{ Cramer }
      Enter ("PRIORITY  ", Prozedure, NOTYP, 12); --{ Cramer }
      Enter ("INHERITP  ", Prozedure, NOTYP, 13); --{ Cramer }
      Enter (ProgramID, Prozedure, NOTYP, 0);
    end EnterStdFcns;

  begin -- Compile

    -- !! As options
    ListingWasRequested:= False;
    Debug:= False;
    Map:= False;

    SkipFlag:= False;
    EofInput:= False;
    Errs:= error_free;

    Init_Tables;
    cICompiler;

    if ListingWasRequested then
      Create (Listing, Name => "compiler.lst");
      Put_Line (Listing, Header);
    end if;

    if qDebug then
      Create (Sym_dump, Name => "symbols.lst");
      Put_Line ("Compiler: check for program heading");
    end if;

    CH := ' ';
    InSymbol;
    if Sy /= WithSy then   -- WITH SMALL_SP;
      Error (err_WITH_Small_Sp, "");
    else
      InSymbol;
      if Sy /= IDent or Id(1..10) /= "SMALL_SP  " then
        Error (err_WITH_Small_Sp, "");
      else
        InSymbol;
        if Sy /= Semicolon then
          Error (err_SEMICOLON_missing, "");
        else
          InSymbol;
        end if;
      end if;
    end if;

    if Sy /= UseSy then
      Error (err_use_Small_Sp, ""); -- USE SMALL_SP;
    else
      InSymbol;
      if Sy /= IDent or Id(1..10) /= "SMALL_SP  " then
        Error (err_use_Small_Sp, "");
      else
        InSymbol;
        if Sy /= Semicolon then
          Error (err_SEMICOLON_missing, "");
        else
          InSymbol;
        end if;
      end if;
    end if;

    if qDebug then
      Put_Line ("Compiler: check for main procedure");
    end if;

    if Sy /= Procedure_Symbol then
      Error (err_missing_a_procedure_declaration, ""); -- PROCEDURE Name IS
    else
      InSymbol;
      if Sy /= IDent then
        Error (err_identifier_missing, "");
      else
        ProgramID := Id;
        InSymbol;
      end if;
    end if;

    if qDebug then
      Put_Line ("Compiler: main procedure is " & ProgramID);
    end if;

    EnterStdFcns; -- Enter Standard function ids and ProgramID

    BlockTab (0) := -- Block Table Entry for Standard [was Main, 1]
     (Id      => "Std Defns" & (10 .. Empty_Alfa'Length => ' '),
      Last    => T,
      LastPar => 1,
      PSize   => 0,
      VSize   => 0,
      SrcFrom => LineCount,
      SrcTo   => LineCount); -- ajout!
    Display (0)  := 0; -- Added 7-Dec-2009

    TaskDefTab (0) := T; --{ Task Table Entry }

    -- Start Compiling
    Block (Block_Begin_Symbol + Statement_Begin_Symbol, False, False, 1, T, IdTab(T).Name);
    -- Main procedure is parsed.
    Emit (k_Halt_Interpreter);

    if Sy /= Semicolon then
      if qDebug then
        Put_Line ("Compile terminated BEFORE FILE END");
      end if;
      if ListingWasRequested then
        Put_Line ("Compile terminated BEFORE FILE END");
      end if;
    end if;

    if BlockTab (1).VSize > StMax - (STKINCR * TCount) then
      Error (err_stack_size, "");
    end if;
    BlockTab (1).SrcTo := LineCount;  --(* Manuel : terminate source *)

    if ListingWasRequested then
      Close (Listing);
    end if;
    if qDebug then
      Close (Sym_dump);
    end if;

    if qDebug and Debug then
      PrintTables;
    end if;
    if Errs /= error_free then
      ErrorMsg;
    --{Close(ErrFile);}
    --{halt;}
    elsif Map then
      if qDebug then
        New_Line;
        Put_Line ("  -* Symbol Table *-");
        New_Line;
        Put_Line ("  LOC  Name       scope");
        Put_Line ("------------------------");
        New_Line;
        for aTx in BlockTab (1).Last + 1 .. T loop
          Tx := aTx;
          if IdTab (Tx).Obj = Variable then
            if IdTab (Tx).TYP /= NOTYP then
              Put (IdTab (Tx).Adr, 4);
              Put (IdTab (Tx).Name, Alng + 3);
            end if;
            if IdTab (Tx).LEV = 1 then
              Put (" Global(");
              Put (IdTab (Tx).LEV, 1);
              Put (')');
              New_Line;
            else
              Put (" Local (");
              Put (IdTab (Tx).LEV, 1);
              Put (')');
              New_Line;
            end if;
          end if;
        end loop;
        New_Line;
      end if;
    end if;
    --{Close(ErrFile);}
  end Compile;

end HAC.Compiler;
