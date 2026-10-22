--  Named parameter associations ("Formal => Actual", RM 6.4),
--  in the formal parameters' declaration order, for procedure and
--  function calls, alone or mixed with positional associations.

--  Output should be empty if the compiler is correct.

with HAT;
with Testing_Utilities;

procedure Named_Associations is
  use HAT, Testing_Utilities;

  --  The result encodes each actual's placement (Origin in the
  --  hundreds, Value in the tens, Color in the units), so any
  --  crossed or misplaced parameter is detected:
  procedure Triple_Draw
    (Origin   : in Integer;
     Value    : in Integer;
     Color    : in Integer;
     out_copy : out Integer)
  is
  begin
    out_copy := Origin * 100 + Value * 10 + Color;
  end Triple_Draw;

  function Mix (A : in Integer; B : in Integer) return Integer is
  begin
    return A * 10 + B;
  end Mix;

  i : Integer := 7;
  total : Integer;

begin
  --  Pure positional:
  Triple_Draw (1, 2, 3, total);
  Assert (total = 123, +"Compiler bug [D]");

  --  Pure named, in the formals' order:
  Triple_Draw (Origin => 1, Value => 2, Color => 3, out_copy => total);
  Assert (total = 123, +"Compiler bug [E]");

  --  Mixed: positional first, then named:
  Triple_Draw (1, Value => 2, Color => 3, out_copy => total);
  Assert (total = 123, +"Compiler bug [F]");

  --  Named with expressions, variables and nested calls:
  Triple_Draw
    (Origin   => i + 1,
     Value    => Mix (A => 3, B => 4),
     Color    => -5,
     out_copy => total);
  Assert (total = 1135, +"Compiler bug [G]");

  --  Function called with named associations, in an expression:
  Assert (Mix (A => 1, B => 2) = 12, +"Compiler bug [H]");

  --  Positional actuals starting with a plain identifier:
  Triple_Draw (i, i * 2, i - 4, total);
  Assert (total = 843, +"Compiler bug [I]");
end Named_Associations;
