with Ada.Directories,
     Ada.Text_IO.Text_Streams,
     Ada.Unchecked_Deallocation;

package body HAC_Sys.Files.Default is

  overriding function Exists (cat : File_Catalogue; name : String) return Boolean
  is
  begin
     return Ada.Directories.Exists (name);
  end Exists;

  overriding function Full_Source_Name (cat : File_Catalogue; name : String) return String
  is
  begin
     return Ada.Directories.Full_Name (name);
  end Full_Source_Name;

  overriding function Full_Spec_Source_Name (cat : File_Catalogue; name : String) return String
  is
    other_name : String := name;
  begin
    if name'Length > 0 then
      other_name (other_name'Last) := 's';  --  GNAT convention: .ads for spec.
    end if;
    return other_name;
  end Full_Spec_Source_Name;

  overriding function Full_Body_Source_Name (cat : File_Catalogue; name : String) return String
  is
    other_name : String := name;
  begin
    if name'Length > 0 then
      other_name (other_name'Last) := 'b';  --  GNAT convention: .adb for body.
    end if;
    return other_name;
  end Full_Body_Source_Name;

  overriding function Is_Open (cat : File_Catalogue; name : String) return Boolean
  is
   file : Text_File_Access;
  begin
    if cat.read_open_map.Contains (name) then
      file := cat.read_open_map.Element (name);
      if file /= null then
        return Ada.Text_IO.Is_Open (file.all);
      end if;
    end if;
    return False;
  end Is_Open;

  overriding procedure Source_Open
    (cat    : in out File_Catalogue;
     name   : in     String;
     stream :    out Root_Stream_Class_Access)
  is
    new_file : Text_File_Access;
  begin
    if cat.read_open_map.Contains (name) then
      raise Constraint_Error with "Attempt to re-open file named """ & name & '"';
    end if;
    new_file := new Ada.Text_IO.File_Type;
    cat.read_open_map.Insert (name, new_file);
    Ada.Text_IO.Open (new_file.all, Ada.Text_IO.In_File, name);
    stream := Root_Stream_Class_Access (Ada.Text_IO.Text_Streams.Stream (new_file.all));
  end Source_Open;

  overriding procedure Skip_Shebang
    (cat            : in out File_Catalogue;
     name           : in     String;
     shebang_offset :    out Natural)
  is
    file : Text_File_Access;
  begin
    shebang_offset := 0;
    if cat.read_open_map.Contains (name) then
      file := cat.read_open_map.Element (name);
      if file /= null
        and then Ada.Text_IO.Is_Open (file.all)
        and then not Ada.Text_IO.End_Of_File (file.all)
      then
        declare
          possible_shebang : constant String := Ada.Text_IO.Get_Line (file.all);
        begin
          if possible_shebang'Length >= 2
            and then
              possible_shebang
                (possible_shebang'First .. possible_shebang'First + 1) = "#!"
          then
            --  Ignore the first line, but count it.
            shebang_offset := 1;
          else
            --  Uh-oh: not a shebang. Then we need to reset the file.
            Ada.Text_IO.Reset (file.all);
          end if;
        end;
      end if;
    end if;
  end Skip_Shebang;

  overriding procedure Close (cat : in out File_Catalogue; name : String) is
    procedure Free is new Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type, Text_File_Access);
    file : Text_File_Access;
  begin
    --  Permissive implementation:
    --  OK if file is unknown to catalogue or not open.
    if cat.read_open_map.Contains (name) then
      file := cat.read_open_map.Element (name);
      if file /= null then
        if Ada.Text_IO.Is_Open (file.all) then
          Ada.Text_IO.Close (file.all);
        end if;
        Free (file);
      end if;
      cat.read_open_map.Delete (name);
    end if;
  end Close;

end HAC_Sys.Files.Default;
