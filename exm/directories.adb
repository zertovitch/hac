--  This demo creates a few directories and then deletes them.

with HAT;

procedure Directories is
  use HAT;
begin
  Create_Directory ("dir_a");
  Create_Path ("dir_b/dir_c/dir_d");
  Put ("Check your current directory and spot ""dir_a"", ""dir_b"".");
  Skip_Line;
  Delete_Directory ("dir_a");
  Put_Line ("""dir_a"" has been deleted.");
  Delete_Directory ("dir_b/dir_c/dir_d");
  Put_Line ("""dir_b/dir_c/dir_d"" has been deleted.");
  Delete_Directory ("dir_b/dir_c");
  Put_Line ("""dir_b/dir_c"" has been deleted.");
  Delete_Directory ("dir_b");
  Put_Line ("""dir_b"" has been deleted.");
end Directories;
