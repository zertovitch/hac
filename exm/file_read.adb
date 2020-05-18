with HAC_Pack;  use HAC_Pack;

procedure File_Read is
  s : VString;
  f : File_Type;
begin
  Open (f, "file_read.adb");
  while not End_Of_File (f) loop
    Get_Line (f, s);
    Put_Line (s);
  end loop;
  Close (f);
end File_Read;
