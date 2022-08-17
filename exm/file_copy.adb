--  This demo reads a text file (itself) and writes
--  its contents in another text file.
--
--  NB: for copying a file (of any kind) with a single command,
--  you can use Copy_File. See binary copy at the end of this demo.

with HAT; use HAT;

procedure File_Copy is
  s : VString;
  f1, f2 : File_Type;
begin
  Put_Line ("Line-by-line text copy");
  Open (f1, "file_copy.adb");
  Create (f2, "file_copy.txt");
  while not End_Of_File (f1) loop
    Get_Line (f1, s);
    Put_Line (f2, s);
  end loop;
  Close (f1);
  Close (f2);
  --
  Put_Line ("Binary copy");
  --  It's an opportunity to test some Ada.Directories-like subprograms.
  --
  Copy_File ("file_copy.adb", "file_copy_bin_$$.txt");
  if Exists ("file_copy_bin.txt") then
    Delete_File ("file_copy_bin.txt");
  end if;
  Rename ("file_copy_bin_$$.txt", "file_copy_bin.txt");
end File_Copy;
