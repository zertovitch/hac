--  https://rosettacode.org/wiki/100_doors#Ada

with HAC_Pack; use HAC_Pack;

procedure Doors is
  type Door_State is (Closed, Open);
  last_door : constant := 100; 
  type Door_List is array(1..last_door) of Door_State;
  the_doors : Door_List;  --  := (others => Closed);
begin
  for i in 1..last_door loop
    the_doors (i) := Closed;
  end loop;
  --
  for i in 1..last_door loop
    for j in 1..last_door loop
      if j mod i = 0 then
        if the_doors(j) = Closed then
          the_doors(j) := Open;
        else
          the_doors(j) := Closed;
        end if;
      end if;
    end loop;
  end loop;
  --
  for i in 1..last_door loop
    Put ("Door "); Put (i);
    Put (" is ");  Put (the_doors(i));  --  <- implicit 'Pos (temp HAC hack)
    New_Line;
  end loop;
end Doors;