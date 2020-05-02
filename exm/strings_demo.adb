with HAC_Pack;  use HAC_Pack;

procedure Strings_demo is

  s1 : VString;
  garbage : Integer := 123;
  s2 : VString;

begin
  s2 := +"Hello";             --  Copy from literal
  s1 := s2;                   --  Copy VString to VString
  Put (s1);
  Put (s1);
  Put (s1);
  New_Line;
  Put_Line (s1);
  Put_Line (s1);
end;
