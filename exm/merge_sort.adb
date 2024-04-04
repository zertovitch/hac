with HAT;

procedure Merge_Sort is

  procedure Merge is

    subtype Vector is String (1 .. 26);
    v                : Vector;
    temp_array       : Vector;
    max              : Integer;
    cur_length       : Integer;
    m                : Integer;
    left, top_left   : Integer;
    right, top_right : Integer;

    c2 : String (1 .. 3);

    use HAT;

  begin
    v := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
    c2 := " rl";
    max := 26;
    Put_Line ("Merge Sort");
    New_Line;
    Put_Line ("String at start:");
    Put_Line ("----------------");
    Put_Line (v);
    New_Line;
    --
    cur_length := 1;
    while cur_length < max loop  --  New phase
      temp_array := v;
      for k in 1 .. 26 loop
        Put (temp_array (k));
        Put (c2 (1));
      end loop;
      New_Line;
      left := 1;
      m := 1;
      while left <= max loop  --  Find pair of subarrays
        right := left + cur_length;
        top_left := right;
        if top_left > max then
          top_left := max + 1;
        end if;
        top_right := right + cur_length;
        if top_right > max then
          top_right := max + 1;
        end if;
        --  Merge subarrays
        --  Go until one subarray runs out
        while left < top_left and right < top_right loop
          if temp_array (left) <= temp_array (right) then
            v (m)  := temp_array (left);
            Put (v (m));
            Put (c2 (2));
            left  := left + 1;
          else
            v (m)  := temp_array (right);
            Put (v (m));
            Put (c2 (3));
            right := right + 1;
          end if;
          m := m + 1;
        end loop;
        --  Now "copy tail" of whichever subarray remains
        while left < top_left loop
          v (m) := temp_array (left);
          Put (v (m));
          Put (c2 (2));
          m    := m + 1;
          left := left + 1;
        end loop;
        while right < top_right loop
          v (m)  := temp_array (right);
          Put (v (m));
          Put (c2 (3));
          right := right + 1;
          m     := m + 1;
        end loop;
        left := top_right;
      end loop;
      New_Line;
      --  Now double size of subarrays and go back for next phase
      cur_length := cur_length * 2;
    end loop;
    New_Line;
    Put_Line ("Result of Merge Sort:");
    Put_Line ("---------------------");
    Put_Line (v);
  end Merge;

begin
  Merge;
end Merge_Sort;
