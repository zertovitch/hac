with HAL;

procedure Attributes is

  subtype Some_Range is Integer range -123 .. 456;
  
  type Enum is (a, b, c, d);
  subtype Sub_Enum is Enum range b .. c;

  --  VImage will be added soon as attribute...
  function VImage (e : Enum) return HAL.VString is
  begin
    case e is
      when a => return +"A";
      when b => return +"B";
      when c => return +"C";
      when d => return +"D";
    end case;
  end VImage;

  use HAL;

begin
  Put_Line ("Standard subtypes are indicated with '*' .");
  Put_Line ("HAL subtypes are indicated with '#' .");
  New_Line;
  Put_Line ("S'First and S'Last attributes");
  New_Line;
  Put_Line (+"* Integer's bounds    : " & Integer'First           & " .. " & Integer'Last);
  Put_Line (+"* Natural's bounds    : " & Natural'First           & " .. " & Natural'Last);
  Put_Line (+"* Positive's bounds   : " & Positive'First          & " .. " & Positive'Last);
  Put_Line (+"  Some_Range's bounds : " & Some_Range'First        & " .. " & Some_Range'Last);
  Put_Line (+"* Boolean's bounds    : " & Boolean'First           & " .. " & Boolean'Last);
  Put_Line (+"# Real's bounds       : " & Real'First              & " .. " & Real'Last);
  Put_Line (+"  Enum's bounds       : " & VImage (Enum'First)     & " .. " & VImage (Enum'Last));
  Put_Line (+"  Sub_Enum's bounds   : " & VImage (Sub_Enum'First) & " .. " & VImage (Sub_Enum'Last));
  New_Line;
  Put_Line ("S'Pred and S'Succ attributes");
  New_Line;
  Put_Line (+"* Integer'Succ (100): " & Integer'Succ (100));
  Put_Line (+"* Integer'Pred (100): " & Integer'Succ (99));
  Put_Line (+"  Some_Range'Succ (Some_Range'First)  : " & Some_Range'Succ (Some_Range'First));
  
  ----------------------------------------------------------------------------------
  --  If you uncomment any of the following lines you'll get a Constraint_Error:  --
  ----------------------------------------------------------------------------------
  
  --  Put_Line (+"  Some_Range'Succ (Some_Range'Last)  : " & Some_Range'Succ (Some_Range'Last));
  --  Put_Line (+"  Some_Range'Pred (Some_Range'First) : " & Some_Range'Pred (Some_Range'First));
end Attributes;
