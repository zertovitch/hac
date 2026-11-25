with HAT;
with Interfaces;

procedure Dir is
  use HAT, Interfaces;
  search    : Search_Type;
  dir_entry : Directory_Entry_Type;
  filter    : Filter_Type := (others => False);
  k         : File_Kind;
begin
  filter (Ordinary_File) := True;
  Put_Line (" Directory of " & Current_Directory);
  Start_Search (search, ".", "*", filter);
  loop
     exit when not More_Entries (search);
     Get_Next_Entry (search, dir_entry);
     Put (Size (dir_entry));
     Put (' ');
     Put_Line (Simple_Name (dir_entry));
     --  k := HAT.Kind (dir_entry);
  end loop;
  End_Search (search);

end Dir;
