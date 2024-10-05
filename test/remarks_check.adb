--  Check for regression in HAC's production of remarks (warnings or notes).

with HAT;
with Testing_Utilities;

procedure Remarks_Check is

  use HAT;

  --  Primitive file comparison for text files that should be identical.
  procedure File_Comp (name_1, name_2 : VString; ok : out Boolean) is 
    f1, f2 : File_Type;
    l1, l2 : VString;
  begin
    ok := True;
    Open (f1, name_1);
    Open (f2, name_2);
    while not (End_Of_File (f1) or End_Of_File (f2)) loop
      Get_Line (f1, l1);
      Get_Line (f2, l2);
      if l1 /= l2 then
        Put_Line ("*** Difference found:");
        New_Line;
        Put_Line (name_1 & ':');
        Put_Line (l1);
        New_Line;
        Put_Line (name_2 & ':');
        Put_Line (l2);
        ok := False;
        exit;
      end if;
    end loop;
    if ok then
      if not End_Of_File (f1) then
        Put_Line (name_1 & " is longer");
        ok := False;
      elsif not End_Of_File (f2) then
        Put_Line (name_2 & " is longer");
        ok := False;
      end if;
    end if;
    Close (f1);
    Close (f2);
  end File_Comp;  

  procedure Check (letter : Character) is 
    prefix   : constant VString := +"remarks_" & letter & '_';
    name_ok  : constant VString := prefix & "ok.txt";
    name_new : constant VString := prefix & "new.txt";
    ok : Boolean;
  begin
    --  -r0 disables all remarks
    Shell_Execute
      (+".." & Directory_Separator & "hac -c -r0 -r" & letter & " ../exm/remarks.adb 2>" & name_new);
    File_Comp (name_ok, name_new, ok);
    if not ok then
      Testing_Utilities.Failure
        (+"Remarks_Check: possible regression in HAC's remarks (warnings or notes)");
    end if;
  end Check;
  
begin
  Check ('k');
  Check ('r');
  Check ('u');
  Check ('v');
end Remarks_Check;
