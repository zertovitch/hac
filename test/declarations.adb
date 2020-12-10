--  Declarations in Ada can be in any order.
--  In Pascal it is: constants, types, variables, subprograms.
--  SmallAda (and early versions of HAC) have some remnants of Pascal...

with HAC_Pack; use HAC_Pack;

procedure Declarations is

  type ee is (r, r2);

  procedure Test (I: Integer) is
  begin
    null;
  end Test;

  procedure Not_Yet_Done (a : Integer) is null;  --  Ada 2005 null procedure.

  --  HAC 0.01: a type right after a subprogram borked the parser!
  type rec is record x, y: Integer; end record;
  a, b: rec;

begin
  a.x := 5;
  a.y := 7;
  b.x := 3;
  b.y := 4;
  if a.x + a.y - b.x * b.y /= 000 then
    Put_Line ("Compiler bug [A]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  Not_Yet_Done (123);
end Declarations;
