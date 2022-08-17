--  This script is useful for clearing the unused space of a drive.
--  Typically, you have deleted sensitive data files, but you would like to
--  prevent recovery of those file contents, but you would still like to
--  keep other files (so, you avoid a new formatting or wipeout of the drive).

with HAT; use HAT;

procedure Fill_Drive is
  f : File_Type;
begin
  Create (f, "garbage_delete_me.txt");
  loop
    Put_Line (f, "Bla bla bla bla bla bla and more bla bla bla bla bla!");
    --  TBD: randomized version.
  end loop;
end Fill_Drive;
