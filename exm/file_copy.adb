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
end File_Copy;
