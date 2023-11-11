with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash,
     Ada.Text_IO;

package HAC_Sys.Files.Default is

  --  This implementation deals with "physical" text files, without a search path.

  type File_Catalogue is limited new Files.Abstract_File_Catalogue with private;

  overriding function Exists (cat : File_Catalogue; name : String) return Boolean;

  overriding function Is_Open (cat : File_Catalogue; name : String) return Boolean;

  overriding procedure Source_Open
    (cat         : in out File_Catalogue;
     name        : in     String;
     stream      :    out Root_Stream_Class_Access);

  overriding procedure Close (cat : in out File_Catalogue; name : String);

private

  type Text_File_Access is access Ada.Text_IO.File_Type;

  package Default_File_Name_Mapping is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type        => String,
     Element_Type    => Text_File_Access,
     Hash            => Ada.Strings.Hash,
     Equivalent_Keys => "=");

  type File_Catalogue is limited new Files.Abstract_File_Catalogue with record
    read_open_map : Default_File_Name_Mapping.Map;
  end record;

end HAC_Sys.Files.Default;
