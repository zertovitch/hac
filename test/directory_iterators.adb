with HAT;
with Testing_Utilities;

procedure Directory_Iterators is

  --  The package HAT provides search facilities that are semantically identical
  --  to the package Ada.Directories'.
  --  In a future version of HAC providing Ada.Directories, the package's specification
  --  and implementation will redirect to HAT for those facilities.
  --  will contain lines such as "type Search_Type is new HAT.Search_Type;"

  search          : HAT.Search_Type;
  directory_entry : HAT.Directory_Entry_Type;  --  Result of the search.
  filter          : HAT.Filter_Type;

  use HAT, Testing_Utilities;
begin

  null;  --  W.I.P. (will contain a regression test using test_directory_iterators.adb and directories known
         --          in the HAC project)

end Directory_Iterators;
