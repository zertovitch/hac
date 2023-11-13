with Ada.Streams;

package HAC_Sys.Files is

  --  This package deals with files in a broad sense.
  --  A file can be, for instance:
  --    - in the operating system's file system, in current directory
  --    - in one of a set of directories (a search path)
  --    - a remote file accessed through the Internet
  --    - in one or more Zip archives (.har)
  --    - in a database
  --    - in a set of open editor windows, with or without
  --        a "physical" file (see the LEA project as an example).

  --  In the last four examples, it could be that the "physical" file
  --  cannot be accessed via Ada.*_IO, or doesn't even exists.

  type Root_Stream_Class_Access is access all Ada.Streams.Root_Stream_Type'Class;

  type Abstract_File_Catalogue is limited interface;

  type Abstract_File_Catalogue_Reference is access all Abstract_File_Catalogue'Class;

  function Exists (cat : Abstract_File_Catalogue; name : String) return Boolean
  is abstract;

  function Full_Source_Name (cat : Abstract_File_Catalogue; name : String) return String
  is abstract;

  function Is_Open (cat : Abstract_File_Catalogue; name : String) return Boolean
  is abstract;

  --  A source file is a text and its search path
  --  may be specific to source files.

  procedure Source_Open
    (cat    : in out Abstract_File_Catalogue;
     name   : in     String;
     stream :    out Root_Stream_Class_Access)
  is abstract;

  procedure Close (cat : in out Abstract_File_Catalogue; name : String)
  is abstract;

end HAC_Sys.Files;
