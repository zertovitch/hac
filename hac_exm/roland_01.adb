--  Submitted by Roland Coghetto - thanks
--
--  ** HAC Bug as of 21-Mar-2019: the BEGIN .. END block
--     below (even without DECLARE) seems to corrupt the stack.
--  See comments @ procedure Block_statement in HAC.Parser.

with HAC_Pack; use HAC_Pack;

procedure Roland_01 is
  a:integer:= 1234;
begin
  put("a = ");
  put(a);
  new_line;
  put("[p1]");
  --  declare
  --    aa: integer; -- := 1;
  --  begin
  --    put("[p2]");
  --    new_line;
  --    put_line("BEGIN BLOC 1");
  --    put("aa = ");
  --    --  put(aa);  --  BUG
  --    new_line;
  --    put_line("END BLOC 1");
  --  end;
  new_line;
  put_line("OUT 1");
  put("a = ");
  put(a);
  new_line;
  put_line("OUT 2");
end Roland_01;
