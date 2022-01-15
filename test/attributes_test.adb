with HAL;

procedure Attributes_test is
  use HAL;

  procedure Failure (Msg : VString) is
  begin
    Put_Line (+"Failure in test: [" & Msg & ']');
    Set_Exit_Status (1);  --  Compiler test failed.
  end Failure;

  procedure Assert (Msg : VString; Check : in Boolean) is
  --  Similar to RM 11.4.2 but without raising an exception.
  begin
    if not Check then Failure (Msg & ", assertion"); end if;
  end Assert;

  type Enum is (aa, bb, cc, dd);
  subtype Sub_Enum is Enum range bb .. cc;

  dummy_e : Enum;
  dummy_i, sum : Integer;

  type A is array (Sub_Enum) of HAL.Real;
  type M is array (-5 .. -2, bb .. dd) of Integer;
  mm : M;

  type R is record
    x : Real;
    y : M;
  end record;

  mmm : array (10 .. 20) of R;

begin
  Assert (+"""<"" on values Strings_as_VStrings, Image", Enum'Image (bb) < Enum'Image (cc));

  Assert (+"S'First",  Enum'First = aa);
  Assert (+"S'Last",   Enum'Last = dd);
  Assert (+"S'Pred",   Enum'Pred (dd) = cc);
  Assert (+"S'Succ",   Enum'Succ (bb) = cc);

  Assert (+"A'First",  bb = A'First);
  Assert (+"A'Last",   cc = A'Last);
  Assert (+"A'Length", 2 = A'Length);

  Assert (+"M'First (1)", M'First (1) = -5);
  Assert (+"M'Last (1)",  M'Last (1) = -2);
  Assert (+"M'First (2)", bb = M'First (2));
  Assert (+"M'Last (2)",  dd = M'Last (2));

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
  Assert (+"M'Range (N)", sum = -84);

  Assert (+"Obj.Last (1)", -2 = mmm (1).y'Last (1));
  Assert (+"Obj.First (2)", bb = mmm (15).y'First (2));

end Attributes_test;
