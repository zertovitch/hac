--  Turn a list of integers (one per row), like
--      ...
--      791
--      690
--      960
--      939
--      549
--      ...
--  into a packed format with 10 items per line, separated by commas, like:
--      ...
--      791,  690,  960,  939,  549,  855,  911,  933,  911,  945,  --   40 ..  49
--      974,  755,  846,  762,  761,  571,  677,  763,  760,  759,  --   50 ..  59
--      754,  494,  552,  537,  577,  692,  786,  788,  788,  790,  --   60 ..  69
--      ...

with HAT;

procedure Pack_List is
  use HAT;
  s : VString;
  f, g : File_Type;
  i : Natural := 0;
  x : Natural;
begin
  Open   (f, "list.txt");
  Create (g, "packed_list.txt");
  while not End_Of_File (f) loop
    Get (f, x);
    Put (g, x, 4);
    Put (g, ", ");
    i := i + 1;
    if i mod 10 = 0 then
      Put (g, " --  ");
      Put (g, i - 10, 3);
      Put (g, " .. ");
      Put (g, i - 1, 3);
      New_Line (g);
    end if; 
  end loop;
  Close (f);
  Close (g);
end Pack_List;
