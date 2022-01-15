with HAL;

procedure Attributes is

  d : constant Duration := 123.456;

  subtype Some_Range is Integer range -123 .. 456;
  subtype A_to_Z is Character range 'A' .. 'Z';

  type Enum is (aa, bb, cc, dd);
  subtype Sub_Enum is Enum range bb .. cc;

  dummy_e : Enum;
  dummy_i, sum : Integer;

  type A is array (Sub_Enum) of HAL.Real;

  type M is array (-5 .. -2, bb .. dd) of Integer;

  mm : M;

  use HAL;

begin
  ---------------------------------------------------
  --  If you uncomment any of the following lines  --
  --  you'll get a Constraint_Error on run time:   --
  ---------------------------------------------------

  --  dummy_i := Some_Range'Succ (Some_Range'Last);
  --  dummy_i := Some_Range'Pred (Some_Range'First);
  --  dummy_e := Enum'Value ("ff");

  --

  Put_Line ("Attributes      Standard subtypes are indicated with '*'");
  Put_Line ("==========           HAL subtypes are indicated with '#'");
  New_Line;
  Put_Line ("S'First and S'Last attributes for scalar subtype S:");
  New_Line;
  Put_Line (+"  * Integer's bounds    : " & Integer'First               & " .. " & Integer'Last);
  Put_Line (+"  * Natural's bounds    : " & Natural'First               & " .. " & Natural'Last);
  Put_Line (+"  * Positive's bounds   : " & Positive'First              & " .. " & Positive'Last);
  Put_Line (+"    Some_Range's bounds : " & Some_Range'First            & " .. " & Some_Range'Last);
  Put_Line (+"  * Boolean's bounds    : " & Boolean'First               & " .. " & Boolean'Last);
  Put_Line (+"    A_to_Z's bounds     : " & A_to_Z'First                & " .. " & A_to_Z'Last);
  Put_Line (+"  # Real's bounds       : " & Real'First                  & " .. " & Real'Last);
  Put_Line (+"    Enum's bounds       : " & Enum'Image (Enum'First)     & " .. " & Enum'Image (Enum'Last));
  Put_Line (+"    Sub_Enum's bounds   : " & Enum'Image (Sub_Enum'First) & " .. " & Enum'Image (Sub_Enum'Last));
  New_Line;
  Put_Line ("S'Pred and S'Succ attributes:");
  New_Line;
  Put_Line (+"  * Integer'Succ (100)                 : " & Integer'Succ (100));
  Put_Line (+"  * Integer'Pred (100)                 : " & Integer'Pred (100));
  Put_Line (+"    Some_Range'Succ (Some_Range'First) : " & Some_Range'Succ (Some_Range'First));
  New_Line;
  Put_Line ("S'Pos and S'Val attributes:");
  New_Line;
  Put_Line (+"  * Boolean'Pos (True)  : " & Boolean'Pos (True));
  Put_Line (+"  * Boolean'Val (0)     : " & Boolean'Val (0));
  Put_Line (+"  * Character'Pos (' ') : " & Character'Pos (' '));
  Put_Line (+"  * Character'Val (65)  : " & Character'Val (65));
  Put_Line (+"    Enum'Pos (aa)       : " & Enum'Pos (aa));
  Put_Line (+"    Enum'Val (3)        : " & Enum'Image (Enum'Val (3)));
  New_Line;
  Put_Line ("S'Image attribute:");
  New_Line;
  Put_Line ("  * Integer'Image (123)      : [" & Integer'Image (123) & ']');
  Put_Line ("  # Real'Image (Pi)          : [" & Real'Image (Pi) & ']');
  Put_Line ("  * Boolean'Image (True)     : [" &
    Boolean'Image (
      --  Here we test "<" on values of the internal type Strings_as_VStrings.
      Enum'Image (bb) < Enum'Image (cc)
    ) &
    ']'
  );
  Put_Line ("  * Character'Image ('x')    : [" & Character'Image ('x') & "] (purely academic!)");
  Put_Line ("  * Duration'Image (123.456) : [" & Duration'Image (d) & ']');
  Put_Line ("    Enum'Image (bb)          : [" & Enum'Image (bb) & ']');
  New_Line;
  Put_Line ("S'Value attribute, re-displayed via S'Image for Enum and,");
  Put_Line (" otherwise, via ""nice"" HAL.Image and VString concatenation");
  New_Line;
  Put_Line (+"  * Integer'Value (""1e3"")     : [" & Integer'Value ("1e3") & ']');
  Put_Line (+"  # Real'Value (""1e3"")        : [" & Real'Value ("1e3") & ']');
  Put_Line (+"  * Boolean'Value (""True"")    : [" & Boolean'Value ("True") & ']');
  Put_Line (+"  * Character'Value (""'x'"")   : [" & Character'Value ("'x'") & ']');
  Put_Line (+"  * Duration'Value (""543.21"") : [" & Duration'Value ("543.21") & ']');
  Put_Line (+"    Enum'Value (""dd"")         : [" & Enum'Image (Enum'Value ("dd")) & ']');

  New_Line;
  --  Test Strings_as_VStrings on some operators and HAL overloaded subprograms.
  for x in Enum loop
    Set_Env (Enum'Image (x), +"Set_to_" & Enum'Image (x));
  end loop;
  --
  for x in Enum'First .. Enum'Last loop  --  Identical to `for x in Enum loop`
    Put_Line (+"Env. variable   " & Enum'Image (x) & "   has the value   " & Get_Env (Enum'Image (x)));
  end loop;
  New_Line;
  for x in Enum'Range loop  --  `Enum'Range` is a shortcut for `Enum'First .. Enum'Last`
    Put_Line (+"Env. variable   " & Enum'Image (x) & "   has the value   " & Get_Env (Enum'Image (x)));
  end loop;

  New_Line;
  Put_Line ("A'First, A'Last, A'Range, A'Length attributes for array type A:");
  New_Line;
  Put_Line (+"  * A'First   (should be BB of type Enum) : " & Enum'Image (A'First));
  Put_Line (+"  * A'Last    (should be CC of type Enum) : " & Enum'Image (A'Last));
  Put_Line (+"  * A'Length  (should be 2) : " & A'Length);

  for x in A'Range loop
    Put_Line (+"Env. variable   " & Enum'Image (x) & "   has the value   " & Get_Env (Enum'Image (x)));
  end loop;

  New_Line;
  Put_Line ("M'First (N), M'Last (N), M'Range (N), M'Length (N) attributes");
  Put_Line ("---  M = Multidimensional array type, mm = object of type M:");
  New_Line;
  Put_Line (+"  * M'First (1)  (should be -5) : " & M'First (1));
  Put_Line (+"  * mm'Last (1)  (should be -2) : " & mm'Last (1));
  Put_Line (+"  * M'First (2)  (should be BB) : " & Enum'Image (M'First (2)));
  Put_Line (+"  * mm'Last (2)  (should be DD) : " & Enum'Image (mm'Last (2)));

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
  Put_Line (+"Sum of elements in the matrix: " & sum);

end Attributes;
