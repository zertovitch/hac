with HAT;

procedure Dir is
  use HAT;

  function Nice_Date (t1 : Time; with_intraday : Boolean) return VString is
    day_secs, day_mins : Integer;
    just_day : VString;
    --
    function Two_Digits (x : Integer) return VString is
    begin
      if x < 10 then
        return "0" & Image (x);
      else
        return Image (x);
      end if;
    end Two_Digits;
    --
  begin
    day_secs := Integer (Seconds (t1));
    day_mins := day_secs / 60;
    just_day := +"" &  --  VString concatenation
      Year (t1)  & '-' &
      Two_Digits (Month (t1)) & '-' &
      Two_Digits (Day (t1));
    if with_intraday then
      return just_day & "  " &
      Two_Digits (day_mins / 60) & ':' &
      Two_Digits (day_mins mod 60) & ':' &
      Two_Digits (day_secs mod 60);
    else
      return just_day;
    end if;
  end Nice_Date;

  search    : Search_Type;
  filter    : constant Filter_Type := (others => True);
  dir_entry : Directory_Entry_Type;  --  Result of the search.

begin
  Put_Line (" Directory of " & Current_Directory);
  Start_Search (search, ".", "*", filter);
  while More_Entries (search) loop
    Get_Next_Entry (search, dir_entry);

    Put (Nice_Date (Modification_Time (dir_entry), True) & "   ");

    case Kind (dir_entry) is
      when Directory     => Put ("      <DIR>           ");
      when Ordinary_File => Put (Size (dir_entry)); Put ("  ");
      when Special_File  => Put (Size (dir_entry)); Put (" *");
    end case;

    Put_Line (Simple_Name (dir_entry));
  end loop;
  End_Search (search);

end Dir;
