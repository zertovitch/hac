with HAC_Pack;  use HAC_Pack;

procedure Strings_demo is
  s1, s2, s4 : VString;
  s3 : constant VString := +" world";

begin
  s2 := +"Hello";             --  Copy from literal
  s1 := s2;                   --  Copy VString to VString
  s4 := s1 & s3;              --  Concatenation
  for i in 1 .. 4 loop Put (s1 & ' '); end loop; New_Line;
  for i in 1 .. 4 loop Put ('.' & s1); end loop; New_Line;
  for i in 1 .. 4 loop Put ('"' & s1 & """    "); end loop; New_Line;
  s4 := "---> """ & s4 & '"';
  Put_Line (s4);
--  Put_Line (">> " & s4 & " <<");  -- Put & Put_Line borks with "x" & ...
end;
