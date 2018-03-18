with HAC_Pack; use HAC_Pack;

procedure Merge_Sort is

   STIME : Float;
   CH    : Character;

   procedure Merge is

    -- SUBTYPE VECTOR IS STRING(1..26); -- !! Hangs HAC
    -- TYPE VECTOR IS NEW STRING(1..26); -- !! Unknown to HAC
    type Vector is array(1..26) of Character;
    V         : Vector;
    TEMPARRAY : Vector;
    MAX       : Integer;
    CURLENGTH : Integer;
    M         : Integer;
    LEFT      : Integer;
    TOPLEFT   : Integer;
    RIGHT     : Integer;
    TOPRIGHT  : Integer;

    C2        : String(1..3);

    TEMP      : Character;
    I,J       : Integer;
    ROW       : Integer;

   begin
       V := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       C2:= " rl";
       MAX := 26;
       ROW := 3;
       Put_Line ("Merge_Sort.");
       New_Line;
       Put_Line ("Unsorted string:");
       Put_Line ("----------------");
       for k in 1..26 loop
         Put(V(k));
       end loop;
       New_Line;
       New_Line;
       -- delay STIME - CLOCK;
       CURLENGTH := 1;
       while CURLENGTH < MAX loop -- NEW PHASE
            TEMPARRAY := V;
            for k in 1..26 loop
              Put(TEMPARRAY(k));
              Put(C2(1));
            end loop;
            New_Line;

            LEFT := 1;
            M := 1;
            while LEFT <= MAX loop -- FIND PAIR OF SUBARRAYS
                RIGHT := LEFT + CURLENGTH;
                TOPLEFT := RIGHT;
                if TOPLEFT > MAX then
                        TOPLEFT := MAX + 1;
                end if;
                TOPRIGHT := RIGHT + CURLENGTH;
                if TOPRIGHT > MAX then
                        TOPRIGHT := MAX + 1;
                end if;
                -- MERGE SUBARRAYS
                -- GO UNTIL ONE SUBARRAY RUNS OUT
                while (LEFT < TOPLEFT) and (RIGHT < TOPRIGHT) loop
                        if TEMPARRAY(LEFT) <= TEMPARRAY(RIGHT) then
                                V(M)  := TEMPARRAY(LEFT);
                                Put(V(M));
                                Put(C2(2));
                                LEFT  := LEFT + 1;
                        else
                                V(M)  := TEMPARRAY(RIGHT);
                                Put(V(M));
                                Put(C2(3));
                                RIGHT := RIGHT + 1;
                        end if;
                        M := M + 1;
                end loop;
                -- NOW "COPY TAIL" OF WHICHEVER SUBARRAY REMAINS
                while LEFT < TOPLEFT loop
                        V(M) := TEMPARRAY(LEFT);
                        Put(V(M));
                        Put(C2(2));
                        M    := M + 1;
                        LEFT := LEFT + 1;
                end loop;
                while RIGHT < TOPRIGHT loop
                        V(M)  := TEMPARRAY(RIGHT);
                        Put(V(M));
                        Put(C2(3));
                        RIGHT := RIGHT + 1;
                        M     := M + 1;
                end loop;
                LEFT := TOPRIGHT;
            end loop;
            New_Line;
            -- NOW DOUBLE SIZE OF SUBARRAYS AND GO BACK FOR NEXT PHASE

            CURLENGTH := CURLENGTH * 2;
       end loop;
       New_Line;
       Put_Line("Result:");
       Put_Line("-------");
       for k in 1..26 loop
         Put(V(k));
       end loop;
       New_Line;
   end Merge;

begin
  -- STIME := CLOCK + 10.0;

  Merge;

  -- PUT("PRESS ENTER TO RETURN TO THE EDITOR...");
  -- GET_LINE(CH);
end Merge_Sort;
