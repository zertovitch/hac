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
  type Output_Mode is (hac, full_ada);
  output : Output_Mode := hac;
begin
  if Argument_Count < 2 then
    Put_Line ("Usage: hac embed_text.adb text_file ada_package_name [-h] [-f]");
    Put_Line ("   or:     embed_text     text_file ada_package_name [-h] [-f]");
    New_Line;
    Put_Line ("Produces: ada_package_name.ads, ada_package_name.adb, demo_ada_package_name.adb");
    New_Line;
    Put_Line ("Options: -h : produce sources for HAC, with HAT package (default)");
    Put_Line ("         -f : produce sources for ""full Ada"", with Ada.* packages");
    return;
  end if;
  Open (f1, Argument (1));
  pkg := Argument (2);
  for i in 3 .. Argument_Count loop
    if Argument (i) = "-h" then
      output := hac;
    elsif Argument (i) = "-f" then
      output := full_ada;
    end if;
  end loop;
  Create (f2, To_Lower (pkg) & ".adb");
  Put_Line (f2, "package body " & pkg & " is");
  Put_Line (f2, "  procedure Fill_Contents (text : out Embedded_Text) is");
  case output is
    when hac      =>
      Put_Line (f2, "    use HAT;");
    when full_ada => 
      Put_Line (f2, "    use Ada.Strings.Unbounded;");
      Put_Line (f2, "    function ""+"" (S : String) return Unbounded_String renames To_Unbounded_String;");
  end case;
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
  Close (f2);
  Create (f2, To_Lower (pkg) & ".ads");
  case output is
    when hac      =>
      Put_Line (f2,  "with HAT;");
    when full_ada =>
      Put_Line (f2,  "with Ada.Strings.Unbounded;");
  end case;
  Put_Line (f2,  "package " & pkg & " is");
  Put (f2, +"  type Embedded_Text is array (1 .. " & lines & ") of ");
  case output is
    when hac      =>
      Put_Line (f2,  "HAT.VString;");
    when full_ada =>
      Put_Line (f2,  "Ada.Strings.Unbounded.Unbounded_String;");
  end case;
  Put_Line (f2,  "  procedure Fill_Contents (text : out Embedded_Text);");
  Put_Line (f2,  "end " & pkg & ";");
  Close (f2);
  Create (f2, "demo_" & To_Lower (pkg) & ".adb");
  case output is
    when hac      =>
      Put_Line (f2,  "with HAT;");
    when full_ada =>
      Put_Line (f2,  "with Ada.Strings.Unbounded, Ada.Text_IO;");
  end case;
  Put_Line (f2,  "with " & pkg & ';');
  Put_Line (f2,  "procedure Demo_" & pkg & " is");
  Put_Line (f2,  "  e : " & pkg & ".Embedded_Text;");
  Put_Line (f2,  "begin");
  Put_Line (f2,  "  " & pkg & ".Fill_Contents (e);");
  Put_Line (f2,  "  for l in e'Range loop");
  case output is
    when hac      =>
      Put_Line (f2,  "    HAT.Put_Line (e (l));");
    when full_ada =>
      Put_Line (f2,  "    Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (e (l)));");
  end case;
  Put_Line (f2,  "  end loop;");
  Put_Line (f2,  "end Demo_" & pkg & ';');
  Close (f2);
end Embed_Text;
