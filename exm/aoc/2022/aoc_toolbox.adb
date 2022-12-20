package body AoC_Toolbox is

  function Dist_L1 (a, b : Point) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y);
  end Dist_L1;

  function Dist_Max (a, b : Point) return Natural is
  begin
    return
      HAT.Max (abs (a.x - b.x), abs (a.y - b.y));
  end Dist_Max;

  procedure Skip_till_Space (f : in out HAT.File_Type; times : Positive) is
    c : Character;
    use HAT;
  begin
    for repeat in 1 .. times loop
      while not End_Of_File (f) loop
        Get (f, c);
        exit when c = ' ';
      end loop;
      exit when End_Of_File (f);
    end loop;
  end Skip_till_Space;

end AoC_Toolbox;
