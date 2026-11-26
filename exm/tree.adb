with HAT;

procedure Tree is
  use HAT;

  procedure Contents (dir, prefix : VString) is
    search    : Search_Type;
    filter    : Filter_Type := (others => False);
    dir_entry : Directory_Entry_Type;  --  Result of the search.
    simple, new_dir : VString;
  begin
    filter (Directory) := True;
    Start_Search (search, dir, "*", filter);
    while More_Entries (search) loop
      Get_Next_Entry (search, dir_entry);
      simple := Simple_Name (dir_entry);
      if simple /= "." and then simple /= ".." then
        new_dir := dir & '/' & simple;
        if More_Entries (search) then
          Put_Line (prefix & "+---" & simple);
          Contents (new_dir, prefix & "|   ");
        else
          Put_Line (prefix & "\---" & simple);
          Contents (new_dir, prefix & "    ");
        end if;
      end if;
    end loop;
    End_Search (search);
  end Contents;

begin
  Contents (+".", +"");
end Tree;
