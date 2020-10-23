--  This demo reads a text file (itself) and writes
--  its contents in another text file.
--  NB: for copying a file (of any kind) with a single command,
--  you can use Copy_File. See second copy at the end of this demo.

with HAC_Pack;  use HAC_Pack;

procedure File_Copy is
  s : VString;
  f1, f2 : File_Type;
begin
  Open (f1, "file_copy.adb");
  Create (f2, "file_copy.txt");
  while not End_Of_File (f1) loop
    Get_Line (f1, s);
    Put_Line (f2, s);
  end loop;
  Close (f1);
  Close (f2);
  --
  Copy_File ("file_copy.adb", "file_copy_2.txt");  --  Binary copy.
end File_Copy;
