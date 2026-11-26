with HAT;
with Testing_Utilities;

procedure OS_Env is

  use HAT, Testing_Utilities;

  procedure Simple is
  begin
    --  Directories:
    Assert (Exists ("os_env.adb"),                +"Compiler bug [OS_Env.Simple, A]");
    Assert (not Exists ("njet_njet_njet.adb"),    +"Compiler bug [OS_Env.Simple, B]");

    --  Environment variables:
    Set_Env ("os_env_test", "bla_bla");
    Assert (Get_Env ("os_env_test") = "bla_bla",  +"Compiler bug [OS_Env.Simple, C]");
    Assert (not (Get_Env ("absurd") = "bla_bla"), +"Compiler bug [OS_Env.Simple, D]");
  end Simple;

  procedure Directory_Iterators is

    --  The package HAT provides search facilities that are semantically identical
    --  to the package Ada.Directories'.
    --  In a future version of HAC providing Ada.Directories, the package's specification
    --  and implementation will redirect to HAT for those facilities.
    --  It will contain lines such as "type Search_Type is new HAT.Search_Type;"

    search    : HAT.Search_Type;
    filter    : constant HAT.Filter_Type := (others => True);
    dir_entry : HAT.Directory_Entry_Type;  --  Result of the search.

    found_myself, found_other, found_absurd, found_subdir : Boolean := False;

  begin
    Start_Search (search, ".", "*", filter);
    while More_Entries (search) loop
      Get_Next_Entry (search, dir_entry);
      if Simple_Name (dir_entry) = "os_env.adb" then
        found_myself := True;
      elsif Simple_Name (dir_entry) = "case_statement.adb" then
        found_other  := True;
      elsif Simple_Name (dir_entry) = "njet_njet_njet.adb" then
        found_absurd := True;
      elsif Simple_Name (dir_entry) = "future" and then Kind (dir_entry) = Directory then
        found_subdir := True;
      end if;
    end loop;
    End_Search (search);

    Assert (found_myself,     +"Compiler bug [OS_Env.Directory_Iterators, A]");
    Assert (found_other,      +"Compiler bug [OS_Env.Directory_Iterators, B]");
    Assert (not found_absurd, +"Compiler bug [OS_Env.Directory_Iterators, C]");
    Assert (found_subdir,     +"Compiler bug [OS_Env.Directory_Iterators, D]");

  end Directory_Iterators;

begin
  Simple;
  Directory_Iterators;
end OS_Env;
