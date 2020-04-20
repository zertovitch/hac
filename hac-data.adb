with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body HAC.Data is

  current_compiler_stream : Stream_Access;        --  !!  Global variable alarm!
  current_compiler_file_name : Unbounded_String;  --  !!  Global variable alarm!

  function Get_Current_Source_Name return String is
  begin
    return To_String (current_compiler_file_name);
  end Get_Current_Source_Name;

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

  function Alfa_to_String (a: Alfa) return String is
  begin
    return Ada.Strings.Fixed.Trim (a, Ada.Strings.Right);
  end Alfa_to_String;

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
      --  !! NB: if HAC ever happens to consume large input files,
      --         the one-character-at-a-time stream input could become
      --         a performance bottleneck.  --> buffered input (cf Zip-Ada)
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
  exception
    when Ada.Text_IO.End_Error =>
      if idx >= InpLine'First then
        Last := idx;  --  Avoid trashing a non-empty line ending the stream.
      else
        raise;
      end if;
  end cGetNextLine;

end HAC.Data;
