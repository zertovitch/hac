--  https://rosettacode.org/wiki/100_doors#Ada

--  There are 100 doors in a row that are all initially closed.
--  You make 100 passes by the doors.
--  The first time through, visit every door and toggle the door (if the
--    door is closed, open it;   if it is open,  close it).
--  The second time, only visit every 2nd door   (door #2, #4, #6, ...),
--    and toggle it.
--  The third time, visit every 3rd door (door #3, #6, #9, ...), etc,
--    until you only visit the 100th door.

with HAC_Pack; use HAC_Pack;

procedure Doors is
  type Door_State is (Closed, Open);
  image : array (Door_State) of VString;
  last_door : constant := 100;
  subtype Door_Range is Integer range 1 .. last_door;
  type Door_List is array (Door_Range) of Door_State;
  the_doors : Door_List;  --  := (others => Closed);
begin
  image (Open)   := +"open";
  image (Closed) := +"closed";
  for i in Door_Range loop
    the_doors (i) := Closed;
  end loop;
  --
  for i in Door_Range loop
    for j in Door_Range loop
      if j mod i = 0 then
        if the_doors (j) = Closed then
          the_doors (j) := Open;
        else
          the_doors (j) := Closed;
        end if;
      end if;
    end loop;
  end loop;
  --
  for i in Door_Range loop
    Put ("Door "); Put (i, 4); Put_Line (+" is " & image ((the_doors (i))));
  end loop;
end Doors;
