with Ada.Streams.Stream_IO;

package HAC_Sys.Files is

  --  This package deals with files in a broad sense.
  --
  --  A file can be, for instance:
  --    - in the operating system's file system, in current directory
  --    - in one of a set of directories (a search path)
  --    - a remote file accessed through the Internet
  --    - in one or more Zip archives (.har)
  --    - in a database
  --    - in a set of open editor windows, with or without
  --        a "physical" file (see the LEA project as an example).
  --
  --  In the last four examples, it could be that the "physical" file
  --  cannot be accessed via Ada.*_IO, or doesn't even exists.

  subtype Root_Stream_Class_Access is Ada.Streams.Stream_IO.Stream_Access;

  type Abstract_File_Catalogue is limited interface;

  type Abstract_File_Catalogue_Reference is access all Abstract_File_Catalogue'Class;

  function Exists (cat : Abstract_File_Catalogue; name : String) return Boolean
  is abstract;

  function Full_Source_Name (cat : Abstract_File_Catalogue; name : String) return String
  is abstract;

  function Full_Spec_Source_Name (cat : Abstract_File_Catalogue; name : String) return String
  is abstract;

  function Full_Body_Source_Name (cat : Abstract_File_Catalogue; name : String) return String
  is abstract;

  function Is_Open (cat : Abstract_File_Catalogue; name : String) return Boolean
  is abstract;

  --  A source file is a text and its search path
  --  may be specific to source files.
  --
  procedure Source_Open
    (cat    : in out Abstract_File_Catalogue;
     name   : in     String;
     stream :    out Root_Stream_Class_Access)
  is abstract;

  --  Skip a possible shebang line (such as "#!/usr/bin/env hac") that may
  --  exist as first line of a main unit's source file.
  --
  --    shebang_offset = 0 if no shebang was found,
  --    shebang_offset = 1 if a shebang was found.
  --
  procedure Skip_Shebang
    (cat            : in out Abstract_File_Catalogue;
     name           : in     String;
     shebang_offset :    out Natural)
  is abstract;

  procedure Close (cat : in out Abstract_File_Catalogue; name : String)
  is abstract;

  --  Inform the File Catalogue that it may have to search a supplemental
  --  directory.
  --  It happens for instance on the "--!hac_add_to_path <dir>" special comment
  --  within a HAC program.
  --
  procedure Add_to_Source_Path (cat : in out Abstract_File_Catalogue; new_dir : String)
  is abstract;

end HAC_Sys.Files;
