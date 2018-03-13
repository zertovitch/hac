with Ada.Strings;
with Ada.Strings.Fixed;

package body HAC.Data is

  current_compiler_stream    : Stream_Access;

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

  function Alfa_to_String(a: Alfa) return String is
  begin
    return Ada.Strings.Fixed.Trim(a, Ada.Strings.Right);
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

  procedure cFeedback is
  begin
    null; -- uiFeedback;
  end cFeedback;

  procedure c_Set_Stream (
    s         : Stream_Access;
    file_name : String         --  Can be virtual (editor, zip entry)
  ) is
  begin
    current_compiler_stream := s;
    current_compiler_file_name := To_Unbounded_String (file_name);
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
     srcNumber, charStart, charEnd, objNumber : Integer;
     hint: String
  )
  is
  begin
    if qDebug then
      Put_Line
       (" errCode=" &
        HAC.UErrors.Error_code'Image (errCode) &
        " (" &
        HAC.UErrors.Error_String (errCode, hint) &
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

end HAC.Data;
