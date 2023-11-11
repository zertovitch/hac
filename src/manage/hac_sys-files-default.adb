with Ada.Directories,
     Ada.Text_IO.Text_Streams,
     Ada.Unchecked_Deallocation;

package body HAC_Sys.Files.Default is

  overriding function Exists (cat : File_Catalogue; name : String) return Boolean
  is
  begin
     return Ada.Directories.Exists (name);
  end Exists;

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

  -----------
  -- Close --
  -----------

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
