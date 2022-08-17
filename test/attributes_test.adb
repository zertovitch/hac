with HAT;
with Testing_Utilities;

procedure Attributes_Test is
  use HAT, Testing_Utilities;

  type Enum is (aa, bb, cc, dd);
  subtype Sub_Enum is Enum range bb .. cc;

  dummy_e : Enum;
  dummy_i, sum : Integer;

  type A is array (Sub_Enum) of HAT.Real;
  type M is array (-5 .. -2, bb .. dd) of Integer;
  mm : M;

  type R is record
    x : Real;
    y : M;
  end record;

  mmm : array (10 .. 20) of R;

begin
  Assert (Enum'Image (bb) < Enum'Image (cc), +"""<"" on values Strings_as_VStrings, Image");

  Assert (Enum'First = aa,     +"S'First");
  Assert (Enum'Last = dd,      +"S'Last");
  Assert (Enum'Pred (dd) = cc, +"S'Pred");
  Assert (Enum'Succ (bb) = cc, +"S'Succ");

  Assert (bb = A'First, +"A'First");
  Assert (cc = A'Last,  +"A'Last");
  Assert (2 = A'Length, +"A'Length");

  Assert (M'First (1) = -5, +"M'First (1)");
  Assert (M'Last (1) = -2,  +"M'Last (1)");
  Assert (bb = M'First (2), +"M'First (2)");
  Assert (dd = M'Last (2),  +"M'Last (2)");

  for i in M'Range (1) loop
    for j in M'Range (2) loop
      mm (i, j) := i * Enum'Pos (j);
    end loop;
  end loop;

  sum := 0;
  for j in M'Range (2) loop
    for i in M'Range (1) loop
      sum := sum + mm (i, j);
    end loop;
  end loop;
  Assert (sum = -84, +"M'Range (N)");

  Assert (-2 = mmm (10).y'Last (1),  +"Obj.Last (1)");
  Assert (bb = mmm (15).y'First (2), +"Obj.First (2)");

end Attributes_Test;
