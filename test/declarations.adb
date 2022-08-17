--  Declarations in Ada can be in any order.
--  In Pascal it is: constants, types, variables, subprograms.
--  SmallAda (and early versions of HAC) has some remnants of Pascal and funny other bugs...

with HAT; use HAT;

procedure Declarations is

  Called_Mimi_Me : Boolean := False;
  Called_Micro_Me : Boolean := False;

  procedure Declarations is
    procedure Declarations is
    begin
      Called_Micro_Me := True;
      return;
      Put ("Noooo wayyyy (inner level 2)");
    end;
  begin
    Declarations;
    Called_Mimi_Me := True;
    return;  --  SmallAda & HAC < rev. 331: sees CD.Main_Program_ID and triggers "ILLEGAL RETURN STATEMENT FROM MAIN" !...
    Put ("Noooo wayyyy (inner level 1)");
    Set_Exit_Status (1);  --  Compiler test failed.
  end
  ;  --  SmallAda & HAC < rev. 332: sees CD.Main_Program_ID and triggers "Incorrectly used symbol" !...

  type ee is (r, r2);

  procedure Test (I : Integer) is
  begin
    null;
  end Test;

  procedure Not_Yet_Done (a : Integer) is null;  --  Ada 2005 null procedure.

  --  HAC 0.01: a type right after a subprogram borked the parser!
  type rec is record x, y : Integer; end record;
  a, b : rec;

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
  Declarations;  --  Subprogram with the same name.
  if not (Called_Mimi_Me and Called_Micro_Me) then
    Set_Exit_Status (1);  --  Compiler test failed.
    return;  --  HAC >= rev. 331: return from main emits a k_Halt_Interpreter
  end if;
  return;  --  HAC >= rev. 331: return from main emits a k_Halt_Interpreter
  Put ("Noooo wayyyy (inner level 0)");
  Set_Exit_Status (1);  --  Compiler test failed.
end Declarations;
