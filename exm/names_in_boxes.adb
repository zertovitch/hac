--  https://stackoverflow.com/questions/61719343/storing-and-using-strings-of-varying-length-ada

with HAT; use HAT;

procedure Names_in_Boxes is

  Max : constant := 100;
  type Names_List is array (1 .. Max) of VString;

  procedure Object_Catcha (N : out Integer) is
  begin
    Put ("Enter amount of objects: ");
    Get (N);
    --  Console vs. GUI input:
    if Get_Needs_Skip_Line then Skip_Line; else New_Line; end if;
  end Object_Catcha;

  procedure Names_Catcha (Names : out Names_List; N : in Integer) is
  begin
    for I in 1 .. N loop
      Put (+"Object " & I & ": ");
      Get_Line (Names (I));
    end loop;
  end Names_Catcha;

  procedure Space_Box (Names : in Names_List; N : in Integer) is
  begin
    Put_Line (N * "+-----------+     ");
    for I in 1 .. N loop
      Put ("| " & Names (I) & (10 - Length (Names (I))) * ' ' & '|');
      if I < N then
        Put ("<>---");
      end if;
    end loop;
    New_Line;
    Put_Line (N * "+-----------+     ");
  end Space_Box;

  --  "Global" variables, unknown to
  --  Object_Catcha, Names_Catcha, Space_Box:
  N : Integer;
  Names : Names_List;

begin
  Object_Catcha (N);
  if N > Max then
    Put_Line (+"Too many objects! Maximum is " & Max);
  else
    Put_Line ("Enter the name of the objects: ");
    Names_Catcha (Names, N);
    Space_Box (Names, N);
  end if;
end Names_in_Boxes;
