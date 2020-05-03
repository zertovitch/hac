with HAC_Pack;  use HAC_Pack;

procedure Strings is
  s1, s2, s4 : VString;
begin
  s4 := +"abc" & 'd' & "ef";
  if s4 /= +"abcdef" then
    Put_Line ("Compiler bug [Comp. VString to VString, or conv. Literal String to VString]");
  end if;
  if Length (s4) /= 6 then
    Put_Line ("Compiler bug [VString Length]");
  end if;
end Strings;
