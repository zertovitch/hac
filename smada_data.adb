with UErrors;

Package body SmAda_Data is

  function "+"(a,b: Set) return Set is
    c: Set(a'range);
    begin
     for i in a'range loop c(i):= a(i) or b(i); end loop;
     return c;
    end;

  function "+"(a,b: Symset) return Symset is
    c: Symset;
    begin
     for i in a'range loop c(i):= a(i) or b(i); end loop;
     return c;
    end;

  function "+"(a: Symset; b: KeyWSymbol) return Symset is
    c: Symset:= a;
    begin
      c(b):= True;
      return c;
    end;

  function "-"(a,b: Symset) return Symset is
    c: Symset;
    begin
     for i in a'range loop c(i):= a(i) and not b(i); end loop;
     return c;
    end;

  function "-"(a: Symset; b: KeyWSymbol) return Symset is
    c: Symset:= a;
    begin
      c(b):= False;
      return c;
    end;

 -----------------------------------------------------------ICompiler-----

PROCEDURE cICompiler is
 --  This procedure is executed once.
 --  So store here only the initialization steps
 --  required at the beginning of execution, and not at the beginning of
 --  compilation
  BEGIN
	-- (MRC) Total error count, from PC version
	Err_count := 0;

    --  Initialize character class lookup table
	FOR C in Character'val(OrdMinChar)..Character'val(OrdMaxChar) LOOP
		CASE C is
		  WHEN 'A'..'Z'=>	CharacterTypes(C) := Letter;
		  WHEN 'a'..'z'=>	CharacterTypes(C) := LowCase;
		  WHEN '0'..'9'=>	CharacterTypes(C) := Number;
		  WHEN '+'| '-'| '*'| '/'| '('| ')'| '$'| '='| ' '| ','| '.'|
		       ''' | '['| ']'| ':'| '^'| '_'| ';'|
		       '{' | '|' | '}' | '<'| '>'|
		       '"'=>	CharacterTypes(C) := Special;
		  WHEN c128=>		CharacterTypes(C) := Special;
		  WHEN OTHERS=>	CharacterTypes(C) := Illegal;
		END CASE;
	END LOOP;  --  for C

 END cICompiler;

	--  Initialize the keyword and keyword symbol Arrays

 procedure KeyWInit is
   begin
	AdaKeyW(1)  := "ABORT     "; AdaKeyWSy(1)  := USy;
	AdaKeyW(2)  := "ABS       "; AdaKeyWSy(2)  := USy;
	AdaKeyW(3)  := "ACCEPT    "; AdaKeyWSy(3)  := AcceptSy;
	AdaKeyW(4)  := "ACCESS    "; AdaKeyWSy(4)  := USy;
	AdaKeyW(5)  := "ALL       "; AdaKeyWSy(5)  := USy;
	AdaKeyW(6)  := "AND       "; AdaKeyWSy(6)  := AndSy;
	AdaKeyW(7)  := "ARRAY     "; AdaKeyWSy(7)  := ArraySy;
	AdaKeyW(8)  := "AT        "; AdaKeyWSy(8)  := ATSy;
	AdaKeyW(9)  := "BEGIN     "; AdaKeyWSy(9)  := BeginSy;
	AdaKeyW(10) := "BODY      "; AdaKeyWSy(10) := BodySy;
	AdaKeyW(11) := "CASE      "; AdaKeyWSy(11) := CaseSy;
	AdaKeyW(12) := "CONSTANT  "; AdaKeyWSy(12) := ConstSy;
	AdaKeyW(13) := "DECLARE   "; AdaKeyWSy(13) := DeclareSy;
	AdaKeyW(14) := "DELAY     "; AdaKeyWSy(14) := DelaySy;
	AdaKeyW(15) := "DELTA     "; AdaKeyWSy(15) := USy;
	AdaKeyW(16) := "DIGITS    "; AdaKeyWSy(16) := USy;
	AdaKeyW(17) := "DO        "; AdaKeyWSy(17) := doSy;
	AdaKeyW(18) := "ELSE      "; AdaKeyWSy(18) := ElseSy;
	AdaKeyW(19) := "ELSIF     "; AdaKeyWSy(19) := ElsIfSy;
	AdaKeyW(20) := "END       "; AdaKeyWSy(20) := EndSy;
	AdaKeyW(21) := "ENTRY     "; AdaKeyWSy(21) := EntrySy;
	AdaKeyW(22) := "EXCEPTION "; AdaKeyWSy(22) := USy;
	AdaKeyW(23) := "EXIT      "; AdaKeyWSy(23) := ExitSy;
	AdaKeyW(24) := "FOR       "; AdaKeyWSy(24) := ForSy;
	AdaKeyW(25) := "FUNCTION  "; AdaKeyWSy(25) := FuncSy;
	AdaKeyW(26) := "GENERIC   "; AdaKeyWSy(26) := USy;
	AdaKeyW(27) := "GOTO      "; AdaKeyWSy(27) := USy;
	AdaKeyW(28) := "IF        "; AdaKeyWSy(28) := IfSy;
	AdaKeyW(29) := "IN        "; AdaKeyWSy(29) := InSy;
	AdaKeyW(30) := "IS        "; AdaKeyWSy(30) := IsSy;
	AdaKeyW(31) := "LIMITED   "; AdaKeyWSy(31) := USy;
	AdaKeyW(32) := "LOOP      "; AdaKeyWSy(32) := LoopSy;
	AdaKeyW(33) := "MOD       "; AdaKeyWSy(33) := ModSy;
	AdaKeyW(34) := "NEW       "; AdaKeyWSy(34) := USy;
	AdaKeyW(35) := "NOT       "; AdaKeyWSy(35) := NOTSy;
	AdaKeyW(36) := "NULL      "; AdaKeyWSy(36) := NullSy;
	AdaKeyW(37) := "OF        "; AdaKeyWSy(37) := OFSy;
	AdaKeyW(38) := "OR        "; AdaKeyWSy(38) := OrSy;
	AdaKeyW(39) := "OTHERS    "; AdaKeyWSy(39) := OthersSy;
	AdaKeyW(40) := "OUT       "; AdaKeyWSy(40) := OutSy;
	AdaKeyW(41) := "PACKAGE   "; AdaKeyWSy(41) := USy;
	AdaKeyW(42) := "PRAGMA    "; AdaKeyWSy(42) := USy;
	AdaKeyW(43) := "PRIVATE   "; AdaKeyWSy(43) := USy;
	AdaKeyW(44) := "PROCEDURE "; AdaKeyWSy(44) := ProcSy;
	AdaKeyW(45) := "RAISE     "; AdaKeyWSy(45) := USy;
	AdaKeyW(46) := "RANGE     "; AdaKeyWSy(46) := RangeSy;
	AdaKeyW(47) := "RECORD    "; AdaKeyWSy(47) := RecordSy;
	AdaKeyW(48) := "REM       "; AdaKeyWSy(48) := USy;
	AdaKeyW(49) := "RENAMES   "; AdaKeyWSy(49) := USy;
	AdaKeyW(50) := "RETURN    "; AdaKeyWSy(50) := ReturnSy;
	AdaKeyW(51) := "REVERSE   "; AdaKeyWSy(51) := ReverseSy;
	AdaKeyW(52) := "SELECT    "; AdaKeyWSy(52) := SelectSy;
	AdaKeyW(53) := "SEPARATE  "; AdaKeyWSy(53) := USy;
	AdaKeyW(54) := "SUBTYPE   "; AdaKeyWSy(54) := SubTypeSy;
	AdaKeyW(55) := "TASK      "; AdaKeyWSy(55) := TaskSy;
	AdaKeyW(56) := "TERMINATE "; AdaKeyWSy(56) := TerminateSy;
	AdaKeyW(57) := "THEN      "; AdaKeyWSy(57) := ThenSy;
	AdaKeyW(58) := "TYPE      "; AdaKeyWSy(58) := TypeSy;
	AdaKeyW(59) := "USE       "; AdaKeyWSy(59) := UseSy;
	AdaKeyW(60) := "WHEN      "; AdaKeyWSy(60) := WhenSy;
	AdaKeyW(61) := "WHILE     "; AdaKeyWSy(61) := WhileSy;
	AdaKeyW(62) := "WITH      "; AdaKeyWSy(62) := WithSy;
	AdaKeyW(63) := "XOR       "; AdaKeyWSy(63) := USy;
	AdaKeyW(64) := "ZEND      "; AdaKeyWSy(64) := IDent;

   end KeyWInit;

  PROCEDURE cFeedback is
  BEGIN
	null; -- uiFeedback;
  END;

  current_compiler_stream: Stream_Access;

  procedure c_Set_Stream(s: Stream_Access) is
  begin
    current_compiler_stream:= s;
  end;


  PROCEDURE cGetNextLine(InpLine : out String; Last: out natural) is
    idx: Integer:= InpLine'First - 1;
    c: Character;
  BEGIN
    loop
      Character'Read(current_compiler_stream, c);
      exit when c = ASCII.CR or c = ASCII.LF;
      idx:= idx + 1;
      InpLine(idx):= c;
    end loop;
    Last:= idx;
    -- if qDebug then
    --   Put_Line("[::]" & InpLine(InpLine'First..Last));
    -- end if;
  END;

  FUNCTION  cEndOfSource return Boolean is
  BEGIN
	return false; -- return uiEndOfSource;
  END;

  PROCEDURE cFoundError(errCode, srcNumber, charStart, charEnd,
 	objNumber : Integer) is
  BEGIN
    if qDebug then
      Put_Line(
        " errCode=" & Integer'Image(errCode) &
        " (" & UErrors.ErrorString(errCode) & ") " &
        " srcNumber=" & Integer'Image(srcNumber) &
        " charStart=" & Integer'Image(charStart) &
        " charEnd=" & Integer'Image(charEnd) &
        " objNumber=" & Integer'Image(objNumber)
      );
    end if;
  END;

BEGIN
  KeyWInit;
END SmAda_Data;
