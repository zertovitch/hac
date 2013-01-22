with SMALL_SP; use SMALL_SP;
procedure MERGE is

   STIME : FLOAT;
   CH    : CHARACTER;

   procedure SCREEN(X,Y : in INTEGER; C, C2 : in CHARACTER) is
   begin
            CURSORAT(X-1,Y);
            PUT(C2);
            CURSORAT(X,Y);
            PUT(C);
            CURSORAT(X-1,Y);
            PUT(" ");
   end SCREEN;

   procedure MERGE_SORT is

    TYPE VECTOR IS STRING(0..25);
    V         : VECTOR;
    TEMPARRAY : VECTOR;
    MAX       : INTEGER;
    CURLENGTH : INTEGER;
    M         : INTEGER;
    LEFT      : INTEGER;
    TOPLEFT   : INTEGER;
    RIGHT     : INTEGER;
    TOPRIGHT  : INTEGER;

    C2        : STRING(0..2);

    TEMP      : CHARACTER;
    I,J       : INTEGER;
    ROW       : INTEGER;

   begin
       V := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       C2:= " RL";
       MAX := 25;
       ROW := 3;
       for k in 0..25 loop
        SCREEN(ROW,14+k,V(k),C2(0));
       end loop;
       delay STIME - CLOCK;
       CURLENGTH := 1;
       WHILE CURLENGTH < MAX LOOP -- NEW PHASE
            CURSORAT(ROW,55); PUT("CURLENGTH:"); PUT(CURLENGTH);
            TEMPARRAY := V;
            CURSORAT(ROW+3,1); PUT("Temp: ");
            for k in 0..25 loop
                SCREEN(ROW+3,14+k,TEMPARRAY(K),C2(0));
            end loop;

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
                                SCREEN(3,14+M,V(M),C2(1));
                                LEFT  := LEFT + 1;
                        ELSE
                                V(M)  := TEMPARRAY(RIGHT);
                                SCREEN(3,14+M,V(M),C2(2));
                                RIGHT := RIGHT + 1;
                        END IF;
                        M := M + 1;
                END LOOP;
                -- NOW "COPY TAIL" OF WHICHEVER SUBARRAY REMAINS
                WHILE LEFT < TOPLEFT LOOP
                        V(M) := TEMPARRAY(LEFT);
                        SCREEN(3,14+M,V(M),C2(1));
                        M    := M + 1;
                        LEFT := LEFT + 1;
                END LOOP;
                WHILE RIGHT < TOPRIGHT LOOP
                        V(M)  := TEMPARRAY(RIGHT);
                        SCREEN(3,14+M,V(M),C2(2));
                        RIGHT := RIGHT + 1;
                        M     := M + 1;
                END LOOP;
                LEFT := TOPRIGHT;
            END LOOP;
            -- NOW DOUBLE SIZE OF SUBARRAYS
            -- AND GO BACK FOR NEXT PHASE

            CURLENGTH := CURLENGTH * 2;
       END LOOP;
   end MERGE_SORT;


begin

  CURSORAT(3,1); PUT("Merge:");

  STIME := CLOCK + 10.0;

  MERGE_SORT;

  CURSORAT(15,1); PUT("PRESS ENTER TO RETURN TO THE EDITOR...");
  GET_LINE(CH);

end MERGE;
