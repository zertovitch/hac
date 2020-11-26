--  This demo reads a text file (itself) and displays
--  its contents on the console.

with HAC_Pack;  use HAC_Pack;

procedure File_Append is
  f : File_Type;
  n : VString := To_VString ("file_append.txt");
begin
  Create (f, n);
  Put_Line (f, "0");
  Close (f);
  --
  for i in 1 .. 9 loop
    Append (f, n);
    Put_Line (f, i, 0);
    Close (f); 
  end loop;
end File_Append;
