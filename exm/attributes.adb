with HAL;

procedure Attributes is

  subtype Some_Range is Integer range -123 .. 456;
  subtype A_to_Z is Character range 'A' .. 'Z';

  type Enum is (aa, bb, cc, dd);
  subtype Sub_Enum is Enum range bb .. cc;

  d : constant Duration := 123.456;

  use HAL;

begin
  Put_Line ("Attributes. Standard subtypes are indicated with '*'");
  Put_Line ("                 HAL subtypes are indicated with '#'");
  New_Line;
  Put_Line ("S'First and S'Last attributes");
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
  Put_Line ("S'Pred and S'Succ attributes");
  New_Line;
  Put_Line (+"  * Integer'Succ (100)                : " & Integer'Succ (100));
  Put_Line (+"  * Integer'Pred (100)                : " & Integer'Pred (100));
  Put_Line (+"    Some_Range'Succ (Some_Range'First): " & Some_Range'Succ (Some_Range'First));
  New_Line;
  Put_Line ("S'Pos and S'Val attributes");
  New_Line;
  Put_Line (+"  * Boolean'Pos (True) : " & Boolean'Pos (True));
  Put_Line (+"  * Boolean'Val (0)    : " & Boolean'Val (0));
  Put_Line (+"  * Character'Pos (' '): " & Character'Pos (' '));
  Put_Line (+"  * Character'Val (65) : " & Character'Val (65));
  Put_Line (+"    Enum'Pos (aa)      : " & Enum'Pos (aa));
  Put_Line (+"    Enum'Val (3)       : " & Enum'Image (Enum'Val (3)));
  New_Line;
  Put_Line ("S'Image attribute");
  New_Line;
  Put_Line (+"  * Integer'Image (123)      : [" & Integer'Image (123) & ']');
  Put_Line (+"  # Real'Image (Pi)          : [" & Real'Image (Pi) & ']');
  Put_Line (+"  * Boolean'Image (True)     : [" & 
    Boolean'Image (
      --  Here we test "<" on values of the internal type Strings_as_VStrings.
      Enum'Image (bb) < Enum'Image (cc)
    ) &
    ']'
  );
  Put_Line (+"  * Character'Image ('x')    : [" & Character'Image ('x') & ']');
  Put_Line (+"  * Duration'Image (123.456) : [" & Duration'Image (d) & ']');
  Put_Line (+"    Enum'Image (bb)          : [" & Enum'Image (bb) & ']');

  New_Line;
  --  Test Strings_as_VStrings on some operators and HAL overloaded subprograms.
  for x in Enum loop
    Set_Env (Enum'Image (x), +"Set_to_" & Enum'Image (x));
  end loop;
  --
  for x in Enum loop
    Put_Line (Get_Env (Enum'Image (x)));
  end loop;

  ----------------------------------------------------------------------------------
  --  If you uncomment any of the following lines you'll get a Constraint_Error:  --
  ----------------------------------------------------------------------------------

  --  Put_Line (+"  Some_Range'Succ (Some_Range'Last)  : " & Some_Range'Succ (Some_Range'Last));
  --  Put_Line (+"  Some_Range'Pred (Some_Range'First) : " & Some_Range'Pred (Some_Range'First));
end Attributes;
