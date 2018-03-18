with HAC_Pack; use HAC_Pack;

procedure Merge_Sort is

  -- STIME : Float;
  -- CH    : Character;

  procedure Merge is

   -- SUBTYPE VECTOR IS STRING(1..26); -- !! Hangs HAC
   -- TYPE VECTOR IS NEW STRING(1..26); -- !! Unknown to HAC
   type Vector is array (1..26) of Character;
   v                : Vector;
   temp_array       : Vector;
   max              : Integer;
   cur_length       : Integer;
   m                : Integer;
   left, top_left   : Integer;
   right, top_right : Integer;

   c2        : String (1..3);

  begin
    v := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
    c2:= " rl";
    max := 26;
    Put_Line ("Merge_Sort.");
    New_Line;
    Put_Line ("Unsorted string:");
    Put_Line ("----------------");
    for k in 1..26 loop
      Put(v(k));
    end loop;
    New_Line;
    New_Line;
    -- delay STIME - CLOCK;
    cur_length := 1;
    while cur_length < max loop -- NEW PHASE
      temp_array := v;
      for k in 1..26 loop
        Put(temp_array(k));
        Put(c2(1));
      end loop;
      New_Line;

      left := 1;
      m := 1;
      while left <= max loop -- FIND PAIR OF SUBARRAYS
        right := left + cur_length;
        top_left := right;
        if top_left > max then
          top_left := max + 1;
        end if;
        top_right := right + cur_length;
        if top_right > max then
          top_right := max + 1;
        end if;
        -- MERGE SUBARRAYS
        -- GO UNTIL ONE SUBARRAY RUNS OUT
        while (left < top_left) and (right < top_right) loop
          if temp_array(left) <= temp_array(right) then
            v(m)  := temp_array(left);
            Put(v(m));
            Put(c2(2));
            left  := left + 1;
          else
            v(m)  := temp_array(right);
            Put(v(m));
            Put(c2(3));
            right := right + 1;
          end if;
          m := m + 1;
        end loop;
        -- NOW "COPY TAIL" OF WHICHEVER SUBARRAY REMAINS
        while left < top_left loop
          v(m) := temp_array(left);
          Put(v(m));
          Put(c2(2));
          m    := m + 1;
          left := left + 1;
        end loop;
        while right < top_right loop
          v(m)  := temp_array(right);
          Put(v(m));
          Put(c2(3));
          right := right + 1;
          m     := m + 1;
        end loop;
        left := top_right;
      end loop;
      New_Line;
      -- NOW DOUBLE SIZE OF SUBARRAYS AND GO BACK FOR NEXT PHASE
      cur_length := cur_length * 2;
    end loop;
    New_Line;
    Put_Line("Result of Merge_Sort:");
    Put_Line("---------------------");
    for k in 1..26 loop
      Put(v(k));
    end loop;
    New_Line;
  end Merge;

begin
  -- STIME := CLOCK + 10.0;

  Merge;

  -- PUT("PRESS ENTER TO RETURN TO THE EDITOR...");
  -- GET_LINE(CH);
end Merge_Sort;
