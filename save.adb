--  Local backup Ada shell script for the HAC project.
--
--  This script works both with HAC (command: hac save.adb)
--  and a full Ada compiler like GNAT, and that on different
--  Operating Systems: Linux and Windows at least.
--
--  A "shebang" for Unix/Linux, such as "#!/usr/bin/env hac" can be
--  added on the top line of this file.
--  HAC will ignore it, but GNAT won't like it.

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
      return just_day & "--" &
      Two_Digits (day_mins / 60) & '-' &
      Two_Digits (day_mins mod 60) & '-' &
      Two_Digits (day_secs mod 60);
    else
      return just_day;
    end if;
  end Nice_Date;

  root, examples, files, tests : VString;
  zip_res : Integer;

begin
  Put_Line ("Save date: " & Nice_Date (True));
  Put_Line ("Current directory: " & Current_Directory);
  Put_Line ("-----");

  root := +"hac";
  Set_Directory ("..");

  examples := root & "/exm/*.ad* "  & root & "/exm/*.gpr  " &
              root & "/exm/e.cmd "  & root & "/exm/not_working/*.ad*" &
              root & "/exm/aoc/2020/aoc*";

  tests    := root & "/test/*.ad* " & root & "/test/*.gpr " &
              root & "/test/t.cmd " & root & "/test/*.aru " &
              root & "/test/future/*.ad*";

  files := root & "/src/*.ad* " &
           root & "/src/compile/*.ad* " &
           root & "/src/execute/*.ad* " &
           root & "/*.gpr " &
           root & "/save.adb " &
           root & "/build.cmd " &
           root & "/fast.cmd " &
           root & "/*.txt " &
           root & "/doc/*.txt " &
           root & "/doc/*.xls " &
           root & "/doc/*.pdf " &
           root & "/debug.pra " &
           root & "/obj/debug/create_dir.txt " &
           root & "/obj/fast/create_dir.txt";

  files := files & ' ' & examples & ' ' & tests;

  --  The ZipAda command-line tool can be built or downloaded
  --  from the project Zip-Ada @
  --    https://unzip-ada.sourceforge.io/ ,
  --    https://github.com/zertovitch/zip-ada
  --  or from ALIRE (Ada LIbrary REpository) @ https://alire.ada.dev/
  --
  Shell_Execute ("zipada -ep2 " & root & "/hac-" & Nice_Date (True) & "- " & files, zip_res);
  if zip_res = 0 then
    Put_Line ("Zip archive creation successful");
  else
    Put_Line ("Zip archive creation failed");
  end if;

  Set_Directory (root);

end Save;
