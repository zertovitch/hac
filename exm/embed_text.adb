--  This demo reads a text file and writes
--  an Ada package with its contents.
--
--  https://www.reddit.com/r/ada/comments/1l3i9wl/embedding_a_text_file_in_an_ada_exe/

with HAT;

procedure Embed_Text is
  use HAT;
  s, pkg : VString;
  f1, f2 : File_Type;
  lines : Natural := 0;
  c : Character;
begin
  if Argument_Count < 2 then
    Put_Line ("Usage: hac embed_text.adb [text_file] [ada_package_name]");
    Put_Line ("Produces: embedded.ada (to be split in 3, manually or by gnatchop)");
    return;
  end if;
  Open (f1, Argument (1));
  Create (f2, "embedded.ada");
  pkg := Argument (2);
  Put_Line (f2, "package body " & pkg & " is");
  Put_Line (f2, "  procedure Fill_Contents (text : out Embedded_Text) is");
  Put_Line (f2, "    use HAT;");
  Put_Line (f2, "  begin");
  while not End_Of_File (f1) loop
    lines := lines + 1;
    Get_Line (f1, s);
    Put (f2, +"    text (" & lines & ") := +""");
    for i in 1 .. Length (s) loop
      c := Element (s, i);
      if c = '"' then
        Put (f2, """""");
      else
        Put (f2, c);
      end if;
    end loop;
    Put_Line (f2, """;");
  end loop;
  Close (f1);
  Put_Line (f2,  "  end Fill_Contents;");
  Put_Line (f2,  "end " & pkg & ";");
  Put_Line (f2,  "with HAT;");
  Put_Line (f2,  "package " & pkg & " is");
  Put_Line (f2, +"  type Embedded_Text is array (1 .. " & lines & ") of HAT.VString;");
  Put_Line (f2,  "  procedure Fill_Contents (text : out Embedded_Text);");
  Put_Line (f2,  "end " & pkg & ";");
  Put_Line (f2,  "with HAT, " & pkg & ';');
  Put_Line (f2,  "procedure Demo_" & pkg & " is");
  Put_Line (f2,  "  e : " & pkg & ".Embedded_Text;");
  Put_Line (f2,  "begin");
  Put_Line (f2,  "  " & pkg & ".Fill_Contents (e);");
  Put_Line (f2,  "  for l in e'Range loop");
  Put_Line (f2,  "    HAT.Put_Line (e (l));");
  Put_Line (f2,  "  end loop;");
  Put_Line (f2,  "end Demo_" & pkg & ';');
  Close (f2);
end Embed_Text;
