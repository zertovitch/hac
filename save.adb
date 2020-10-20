--  #!/usr/bin/env hax

--  This script works both with HAC (command: hax save.adb)
--  and a full Ada compiler like GNAT, on Linux and Windows at least.

with HAC_Pack;  use HAC_Pack;

procedure Save is

  function Nice_Date (with_intraday : Boolean) return VString is
    t1 : constant Time := Clock;
    day_secs, day_mins : Integer;
    just_day : VString;
    --
    function Two_Digits (x : Integer) return VString is
    begin
      if x < 10 then
        return "0" & Image(x);
      else
        return Image (x);
      end if;
    end Two_Digits;
  begin
    day_secs := Integer (Seconds (t1));
    day_mins := day_secs / 60;
    just_day := +"" &  --  VString concatenation
      Year (t1)  & '-' &
      Two_Digits (Month (t1)) & '-' &
      Two_Digits (Day (t1));
    if with_intraday then
      return just_day & '-' &
      Two_Digits (day_mins / 60) & '-' &
      Two_Digits (day_mins mod 60) & '-' &
      Two_Digits (day_secs mod 60);
    else
      return just_day;
    end if;
  end Nice_Date;

  root, examples, files, tests : VString;
  r : Integer;

begin
  Put_Line ("Save: " & Nice_Date (True));

  root := +".";  --  +"hac";  !!
  r := Shell_Execute ("cd ..");  --  Ineffective (neither HAC nor GNAT): curr. dir. restored right after call!!

  examples := root & "/exm/*.ad* "  & root & "/exm/*.gpr  " & root & "/exm/e.cmd "  & root & "/exm/not_working/*.ad*";

  tests    := root & "/test/*.ad* " & root & "/test/*.gpr " & root & "/test/t.cmd " & root & "/test/*.aru " & root & "/test/future/*.ad*";

  files := root & "/src/*.ad* " & root & "/src/compile/*.ad* " & root & "/src/execute/*.ad* " &
           root & "/*.gpr " & root & "/*.xls " & 
           root & "/save.cmd " & root & "/save.adb " &
           root & "/build.cmd " & root & "/*.txt " &
           root & "/debug.pra " &
           root & "/obj/debug/create_dir.txt " & root & "/obj/fast/create_dir.txt";

  files := files & ' ' & examples & ' ' & tests;

  r := Shell_Execute ("zipada -ep2 " & root & "/hac-" & Nice_Date (True) & "- " & files);
  r := Shell_Execute ("cd " & root);
end;
