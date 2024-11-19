--  Show all permutations of a certain range.

with HAT;

procedure Permutations is

  subtype Some_Range is Integer range 1 .. 4;

  x : array (Some_Range) of Some_Range;

  procedure Fill (from : Some_Range) is
    ok : Boolean;
    use HAT;
  begin
    for value in Some_Range loop
      ok := True;
      for i in 1 .. from - 1 loop
        ok := ok and then not (x (i) = value);
      end loop;
      if ok then
        x (from) := value; 
        if from = Some_Range'Last then
          for i in Some_Range loop
            Put (Image (x (i)));
          end loop;
          New_Line;
        else
          Fill (from + 1);
        end if;
      end if;
    end loop;
  end;

begin
  Fill (1);
end Permutations;
