--  This demo creates a few directories and deletes on of them.

with HAT;

procedure Directories is
  use HAT;
begin
  Create_Directory ("single");
  Create_Directory ("will_be_deleted");
  Create_Path ("dir_a/dir_b/dir_c");
  Put ("Check your current dir...");
  Skip_Line;
  Delete_Directory ("will_be_deleted");
end Directories;
