with HAC_Pack; use HAC_Pack;

procedure Merge_Sort is

   STIME : FLOAT;
   CH    : CHARACTER;

   procedure SCREEN(X,Y : in INTEGER; C, C2 : in CHARACTER) is
   begin
            CURSORAT(X-1,Y);
            PUT(C2);
            CURSORAT(X,Y);
            PUT(C);
            -- CURSORAT(X-1,Y);
            -- PUT(" ");
   end SCREEN;

   procedure MERGE is

    -- SUBTYPE VECTOR IS STRING(1..26); -- !! Hangs HAC
    -- TYPE VECTOR IS NEW STRING(1..26); -- !! Unknown to HAC
    TYPE VECTOR IS array(1..26) of Character;
    V         : VECTOR;
    TEMPARRAY : VECTOR;
    MAX       : INTEGER;
    CURLENGTH : INTEGER;
    M         : INTEGER;
    LEFT      : INTEGER;
    TOPLEFT   : INTEGER;
    RIGHT     : INTEGER;
    TOPRIGHT  : INTEGER;

    C2        : STRING(1..3);

    TEMP      : CHARACTER;
    I,J       : INTEGER;
    ROW       : INTEGER;

   begin
       V := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       C2:= " rl";
       MAX := 26;
       ROW := 3;
       for k in 1..26 loop
         Put(V(k));
         Put(C2(1));
       end loop;
       New_Line;
       -- delay STIME - CLOCK;
       CURLENGTH := 1;
       WHILE CURLENGTH < MAX LOOP -- NEW PHASE
            -- CURSORAT(ROW,55); PUT("CURLENGTH:"); PUT(CURLENGTH);
            TEMPARRAY := V;
            -- CURSORAT(ROW+3,1); PUT("Temp: ");
            for k in 1..26 loop
              --  SCREEN(ROW+3,14+k,TEMPARRAY(K),C2(0));
              Put(TEMPARRAY(K));
              Put(C2(1));
            end loop;
            New_Line;

            LEFT := 1;
            M := 1;
            WHILE LEFT <= MAX LOOP -- FIND PAIR OF SUBARRAYS
                RIGHT := LEFT + CURLENGTH;
                TOPLEFT := RIGHT;
                IF TOPLEFT > MAX THEN
                        TOPLEFT := MAX + 1;
                END IF;
                TOPRIGHT := RIGHT + CURLENGTH;
                IF TOPRIGHT > MAX THEN
                        TOPRIGHT := MAX + 1;
                END IF;
                -- MERGE SUBARRAYS
                -- GO UNTIL ONE SUBARRAY RUNS OUT
                WHILE (LEFT < TOPLEFT) AND (RIGHT < TOPRIGHT) LOOP
                        IF TEMPARRAY(LEFT) <= TEMPARRAY(RIGHT) THEN
                                V(M)  := TEMPARRAY(LEFT);
                                -- SCREEN(3,14+M,V(M),C2(1));
                                Put(V(M));
                                Put(C2(2));
                                LEFT  := LEFT + 1;
                        ELSE
                                V(M)  := TEMPARRAY(RIGHT);
                                -- SCREEN(3,14+M,V(M),C2(2));
                                Put(V(M));
                                Put(C2(3));
                                RIGHT := RIGHT + 1;
                        END IF;
                        M := M + 1;
                END LOOP;
                -- NOW "COPY TAIL" OF WHICHEVER SUBARRAY REMAINS
                WHILE LEFT < TOPLEFT LOOP
                        V(M) := TEMPARRAY(LEFT);
                        Put(V(M));
                        Put(C2(2));
                        -- SCREEN(3,14+M,V(M),C2(1));
                        M    := M + 1;
                        LEFT := LEFT + 1;
                END LOOP;
                WHILE RIGHT < TOPRIGHT LOOP
                        V(M)  := TEMPARRAY(RIGHT);
                        -- SCREEN(3,14+M,V(M),C2(2));
                        Put(V(M));
                        Put(C2(3));
                        RIGHT := RIGHT + 1;
                        M     := M + 1;
                END LOOP;
                LEFT := TOPRIGHT;
            END LOOP;
            New_Line;
            -- NOW DOUBLE SIZE OF SUBARRAYS AND GO BACK FOR NEXT PHASE

            CURLENGTH := CURLENGTH * 2;
       END LOOP;
       Put_Line("Result:");
       for k in 1..26 loop
         Put(V(k));
       end loop;
       New_Line;
   end MERGE;

begin
  -- CURSORAT(3,1);
  Put_Line("Merge:");

  -- STIME := CLOCK + 10.0;

  MERGE;

  -- CURSORAT(15,1);
  -- PUT("PRESS ENTER TO RETURN TO THE EDITOR...");
  -- GET_LINE(CH);
end Merge_Sort;
