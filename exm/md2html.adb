--  MD2HTML - a Markdown to HTML converter.
--
--  Useful for viewing Markdown files offline.

with HAT;

procedure MD2HTML is

  use HAT;

  f1, f2 : File_Type;

  type List_Descriptor is record
    indent  : Positive;
    ordered : Boolean;
  end record;

  list_top : Natural := 0;
  list_stack : array (1 .. 10) of List_Descriptor;

  bold, italic, underline, strikethrough : Boolean := False;

  procedure Process (s : VString; indent : Positive) is

    function Is_Separator (c : Character) return Boolean is
    begin
      case c is
        when ' ' | ',' | ';' | '.' =>
          return True;
        when others =>
          return False;
      end case;
    end Is_Separator;

    function Process_Content (ct : VString) return VString is  --  Assumption: ct is never empty.
      r : VString;
      c, cp1, cp2 : Character := ' ';
      item : Boolean := False;
      ordered : Boolean;
    begin
      for i in 1 .. Length (ct) loop
        c := Element (ct, i);
        case cp1 is
          when '*' | '_' | '~' | '-' =>
            if c = cp1 then
              --  "**, "__", "~~"
              case cp1 is
                when '*' =>
                  bold := not bold;
                  if bold then r := r & "<b>"; else r := r & "</b>"; end if;
                when '_' =>
                  underline := not underline;
                  if underline then r := r & "<u>"; else r := r & "</u>"; end if;
                when '~' =>
                  strikethrough := not strikethrough;
                  if strikethrough then r := r & "<s>"; else r := r & "</s>"; end if;
                when others =>
                  r := r & cp1;
              end case;
            elsif cp2 /= cp1 then
              if c = ' ' and cp2 = ' ' then
                if i = 2 and then (cp1 = '-' or cp1 = '*') then
                  --  " * item"
                  item := True;
                  ordered := False;
                else
                  r := r & cp1;
                end if;
              elsif Is_Separator (cp2) and then (cp1 = '*' or cp1 = '_') then
                --  " *x"
                if not italic then r := r & "<i>"; end if;
                italic := True;
              elsif Is_Separator (c) and then (cp1 = '*' or cp1 = '_') then
                --  "x* "
                if italic then r := r & "</i>"; end if;
                italic := False;
              else
                r := r & cp1;
              end if;
            elsif cp2 = cp1 then
              --  "**x": treated at previous step.
              null;
            else
              r := r & cp1;
            end if;
          when others =>
            r := r & cp1;
        end case;
        cp2 := cp1;
        cp1 := c;
      end loop;
      if item then
        r := "<li>" & r;
        if list_top = 0 or else indent > list_stack (list_top).indent then
          list_top := list_top + 1;
          list_stack (list_top).indent  := indent;
          list_stack (list_top).ordered := ordered;
          if ordered then
            r := "<ol>" & r;
          else
            r := "<ul>" & r;
          end if;
        end if;
      end if;
      return r;
    end Process_Content;

    hc : Integer := 0;
    i : Positive := 1;
    r : VString;
  begin
    if Length (s) = 0 then
      bold          := False;
      italic        := False;
      underline     := False;
      strikethrough := False;
      return;
    end if;

    --  Headings
    while Element (s, i) = '#' loop
      hc := hc + 1;
      i := i + 1;
    end loop;
    if hc > 0 then
      r := r & "<h" & hc & '>';
    end if;

    r := r & Process_Content (Slice (s, 1 + hc, Length (s)) & ' ');

    while list_top > 0 and then indent < list_stack (list_top).indent loop
      if list_stack (list_top).ordered then
        r := "</ol>" & r;
      else
        r := "</ul>" & r;
      end if;
      list_top := list_top - 1;
    end loop;

    if hc > 0 then
      r := r & "</h" & hc & '>';
    end if;
    Put_Line (f2, r);

  end Process;

  s : VString;

begin
  if Argument_Count = 0 then
    Open (f1, "../readme.md");
  else
    Open (f1, Argument (1));
  end if;
  Create (f2, "output.html");
  Put_Line (f2, "<html><body><font face=""Calibri, Arial, Geneva, Helvetica"">");
  while not End_Of_File (f1) loop
    Get_Line (f1, s);
    for indent in 1 .. Length (s) loop
      if Element (s, indent) /= ' ' then
        Process (Slice (s, indent, Length (s)), indent);
        exit;
      end if;
    end loop;
  end loop;
  Close (f1);
  Put_Line (f2, "</font></body></html>");
  Close (f2);
end MD2HTML;
