--  Silent versions of merge_sort.adb and shell_sort.adb.
--  We check the result. No output <=> compiler is correct.

with HAT;
with Testing_Utilities;

procedure Sorting_Tests is
  use HAT;

  expected_result : constant String (1 .. 26) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  procedure Merge is

    type Vector is array (1 .. 26) of Character;
    v                : Vector;
    temp_array       : Vector;
    max              : Integer;
    cur_length       : Integer;
    m                : Integer;
    left, top_left   : Integer;
    right, top_right : Integer;

  begin
    v := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
    max := 26;
    --
    cur_length := 1;
    while cur_length < max loop  --  New phase
      temp_array := v;
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
        while (left < top_left) and (right < top_right) loop
          if temp_array (left) <= temp_array (right) then
            v (m)  := temp_array (left);
            left  := left + 1;
          else
            v (m)  := temp_array (right);
            right := right + 1;
          end if;
          m := m + 1;
        end loop;
        --  Now "copy tail" of whichever subarray remains
        while left < top_left loop
          v (m) := temp_array (left);
          m    := m + 1;
          left := left + 1;
        end loop;
        while right < top_right loop
          v (m)  := temp_array (right);
          right := right + 1;
          m     := m + 1;
        end loop;
        left := top_right;
      end loop;
      --  Now double size of subarrays and go back for next phase
      cur_length := cur_length * 2;
    end loop;
    for k in 1 .. 26 loop
      Testing_Utilities.Assert
        (v (k) = expected_result (k), +"Wrong result in Merge Sort");
    end loop;
  end Merge;

  procedure Shell is
    b : String (1 .. 26);
    i, j, step : Integer;
    step_size : array (1 .. 4) of Integer;
    stop : Boolean;
    temp : Character;
  begin
    b := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
    --  'steps' contains decreasing increments for each
    --  pass. The last pass has increment 1.
    step_size (4) := 1;
    for pass in reverse 1 .. 3 loop
      step_size (pass) := 2 * step_size (pass + 1);
    end loop;
    for pass in 1 .. 4 loop
      step := step_size (pass);
      --  Do a straight insertion sort with 'step' as
      --  an increment instead of 1.
      i := step + 1;
      while i <= 26 loop
        temp := b (i);
        j := i;
        stop := False;
        while (j > step) and not stop loop
          j := j - step;
          if b (j) > temp then
            b (j + step) := b (j);
          else
            b (j + step) := temp;
            stop := True;
          end if;
        end loop;
        if not stop then
          b (1) := temp;
        end if;
        i := i + step;
      end loop;
    end loop; -- for pass in 1..npass
    for k in 1 .. 26 loop
      Testing_Utilities.Assert
        (b (k) = expected_result (k), +"Wrong result in Shell Sort");
    end loop;
  end Shell;

begin
  Merge;
  Shell;
end Sorting_Tests;
