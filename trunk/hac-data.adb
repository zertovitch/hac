package body HAC.Data is

  function "+" (a, b : Set) return Set is
    c : Set (a'Range);
  begin
    for i in a'Range loop
      c (i) := a (i) or b (i);
    end loop;
    return c;
  end "+";

  function "+" (a, b : Symset) return Symset is
    c : Symset;
  begin
    for i in a'Range loop
      c (i) := a (i) or b (i);
    end loop;
    return c;
  end "+";

  function "+" (a : Symset; b : KeyWSymbol) return Symset is
    c : Symset := a;
  begin
    c (b) := True;
    return c;
  end "+";

  function "-" (a, b : Symset) return Symset is
    c : Symset;
  begin
    for i in a'Range loop
      c (i) := a (i) and not b (i);
    end loop;
    return c;
  end "-";

  function "-" (a : Symset; b : KeyWSymbol) return Symset is
    c : Symset := a;
  begin
    c (b) := False;
    return c;
  end "-";

  function Singleton(s: KeyWSymbol) return Symset is
    res: Symset:= Empty_Symset;
  begin
    res(s):= True;
    return res;
  end;

  -----------------------------------------------------------ICompiler-----

  procedure cICompiler is
  --  This procedure is executed once.
  --  So store here only the initialization steps
  --  required at the beginning of execution, and not at the beginning of
  --  compilation
  begin
    -- (MRC) Total error count, from PC version
    Err_Count := 0;

    --  Initialize character class lookup table
    for C in Character'Val (OrdMinChar) .. Character'Val (OrdMaxChar) loop
      case C is
        when 'A' .. 'Z' =>
          CharacterTypes (C) := Letter;
        when 'a' .. 'z' =>
          CharacterTypes (C) := LowCase;
        when '0' .. '9' =>
          CharacterTypes (C) := Number;
        when '+' | '-' | '*' | '/' |
             '(' | ')' |
             '$' |
             '=' |
             ' ' |
             ',' |
             '.' |
             ''' |
             '[' |
             ']' |
             ':' |
             '^' |
             '_' |
             ';' |
             '{' |
             '|' |
             '}' |
             '<' |
             '>' |
             '"' =>
          CharacterTypes (C) := Special;
        when c128 =>
          CharacterTypes (C) := Special;
        when others =>
          CharacterTypes (C) := Illegal;
      end case;
    end loop;  --  for C

  end cICompiler;

  --  Initialize the keyword and keyword symbol Arrays

  procedure KeyWInit is
  begin
    AdaKeyW:=
      (  1 => ("ABORT     ",  USy),
         2 => ("ABS       ",  USy),
         3 => ("ACCEPT    ", AcceptSy),
         4 => ("ACCESS    ",  USy),
         5 => ("ALL       ",  USy),
         6 => ("AND       ", And_Symbol),
         7 => ("ARRAY     ", ArraySy),
         8 => ("AT        ", ATSy),
         9 => ("BEGIN     ", BEGIN_Symbol),
        10 => ("BODY      ", BodySy),
        11 => ("CASE      ", CASE_Symbol),
        12 => ("CONSTANT  ", CONSTANT_Symbol),
        13 => ("DECLARE   ", DeclareSy),
        14 => ("DELAY     ", DelaySy),
        15 => ("DELTA     ",  USy),
        16 => ("DIGITS    ",  USy),
        17 => ("DO        ", doSy),
        18 => ("ELSE      ", ElseSy),
        19 => ("ELSIF     ", ElsIfSy),
        20 => ("END       ", END_Symbol),
        21 => ("ENTRY     ", EntrySy),
        22 => ("EXCEPTION ",  USy),
        23 => ("EXIT      ", EXIT_Symbol),
        24 => ("FOR       ", FOR_Symbol),
        25 => ("FUNCTION  ", Function_Symbol),
        26 => ("GENERIC   ",  USy),
        27 => ("GOTO      ",  USy),
        28 => ("IF        ", IF_Symbol),
        29 => ("IN        ", IN_Symbol),
        30 => ("IS        ", IS_Symbol),
        31 => ("LIMITED   ",  USy),
        32 => ("LOOP      ", Loop_Symbol),
        33 => ("MOD       ", ModSy),
        34 => ("NEW       ",  USy),
        35 => ("NOT       ", Not_Symbol),
        36 => ("NULL      ", NullSy),
        37 => ("OF        ", OFSy),
        38 => ("OR        ", Or_Symbol),
        39 => ("OTHERS    ", OthersSy),
        40 => ("OUT       ", OutSy),
        41 => ("PACKAGE   ",  USy),
        42 => ("PRAGMA    ",  USy),
        43 => ("PRIVATE   ",  USy),
        44 => ("PROCEDURE ", Procedure_Symbol),
        45 => ("RAISE     ",  USy),
        46 => ("RANGE     ", RangeSy),
        47 => ("RECORD    ", RecordSy),
        48 => ("REM       ",  USy),
        49 => ("RENAMES   ",  USy),
        50 => ("RETURN    ", RETURN_Symbol),
        51 => ("REVERSE   ", ReverseSy),
        52 => ("SELECT    ", SelectSy),
        53 => ("SEPARATE  ",  USy),
        54 => ("SUBTYPE   ", SUBTYPE_Symbol),
        55 => ("TASK      ", TaskSy),
        56 => ("TERMINATE ", TerminateSy),
        57 => ("THEN      ", THEN_Symbol),
        58 => ("TYPE      ", TYPE_Symbol),
        59 => ("USE       ", UseSy),
        60 => ("WHEN      ", WhenSy),
        61 => ("WHILE     ", WhileSy),
        62 => ("WITH      ", WithSy),
        63 => ("XOR       ",  USy),
        64 => ("ZEND      ", IDent)
       );

  end KeyWInit;

  procedure cFeedback is
  begin
    null; -- uiFeedback;
  end cFeedback;

  current_compiler_stream : Stream_Access;

  procedure c_Set_Stream (s : Stream_Access) is
  begin
    current_compiler_stream := s;
  end c_Set_Stream;

  procedure cGetNextLine (InpLine : out String; Last : out Natural) is
    idx : Integer := InpLine'First - 1;
    c   : Character;
  begin
    loop
      Character'Read (current_compiler_stream, c);
      exit when c = ASCII.LF;
      if c /= ASCII.CR then
        idx           := idx + 1;
        InpLine (idx) := c;
      end if;
    end loop;
    Last := idx;
    -- if qDebug then
    --   Put_Line("[::]" & InpLine(InpLine'First..Last));
    -- end if;
  end cGetNextLine;

  function cEndOfSource return Boolean is
  begin
    return False; -- return uiEndOfSource;
  end cEndOfSource;

procedure cFoundError (
  errCode: HAC.UErrors.Error_code;
  srcNumber, charStart, charEnd, objNumber : Integer
) is
  begin
    if qDebug then
      Put_Line
       (" errCode=" &
        HAC.UErrors.Error_code'Image (errCode) &
        " (" &
        HAC.UErrors.ErrorString (errCode, "") & -- !! hint
        ") " &
        " srcNumber=" &
        Integer'Image (srcNumber) &
        " charStart=" &
        Integer'Image (charStart) &
        " charEnd=" &
        Integer'Image (charEnd) &
        " objNumber=" &
        Integer'Image (objNumber));
    end if;
  end cFoundError;

begin
  KeyWInit;
end HAC.Data;
