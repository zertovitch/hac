with HAT; use HAT;   -- CS159-10 Instructor: Arthur Vargas Lopes
                     -- Fall 1990
procedure newrace is
task SCREEN is
        entry WAITME;
        ENTRY SIGNALME;
end SCREEN;
task BUBBLE_SORT is
end BUBBLE_SORT;
task SELECT_S_SORT is
end SELECT_S_SORT;
task body SCREEN is
  n : integer;
begin
  n := 0;
  loop
     select
       when n = 0 =>
          accept WAITME do
            n := 1;
          end WAITME;
     or
       when n = 1 =>
          accept SIGNALME do
            n := n - 1;
          end SIGNALME;
     or
        terminate;
     end select;
   end loop;
end SCREEN;
task body SELECT_S_SORT is
    B : STRING(0..25);
    I,J : INTEGER;
    procedure SWAP(X, Y : in INTEGER) is
        temp : character;
    begin
                                        SCREEN.WAITME; CURSORAT(2,X+14);
                                        PUT("X"); CURSORAT(2,Y+14);
                                        PUT("Y");
                                        SCREEN.SIGNALME;
        temp := b(x);
        b(x) := b(y);
        b(y) := temp;                   SCREEN.WAITME; CURSORAT(3,X+14);
                                        PUT(b(x)); CURSORAT(3,Y+14);
                                        PUT(b(y));
                                        CURSORAT(2,X+14); PUT(" ");
                                        CURSORAT(2,Y+14); PUT(" ");
                                        SCREEN.SIGNALME;
    end SWAP;
 begin
       SCREEN.WAITME;CURSORAT(3,1); PUT("Selection:");SCREEN.SIGNALME;
       B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       for k in 0..25 loop
           CURSORAT(3,k+14);
           put(b(k));
       end loop;
       for i in 0..24 loop       SCREEN.WAITME; CURSORAT(2,50);
                                 PUT("I: "); PUT(I);  SCREEN.SIGNALME;
         for j in (i+1)..25 loop    SCREEN.WAITME; CURSORAT(3,50);
                                    PUT("J: "); PUT(J);  SCREEN.SIGNALME;
           if B(I) > B(J) then
             SWAP(I,J);
           end if;
         end loop;
       end loop;
end SELECT_S_SORT;

task body BUBBLE_SORT is
    B : STRING(0..25);
    I,J : INTEGER;
    procedure SWAP(X, Y : in INTEGER) is
        temp : character;
    begin
        SCREEN.WAITME; CURSORAT(6,X+14); PUT("X"); CURSORAT(6,Y+14);
        PUT("Y"); SCREEN.SIGNALME;
        temp := b(x);
        b(x) := b(y);
        b(y) := temp;
        SCREEN.WAITME; CURSORAT(7,X+14); PUT(b(x)); CURSORAT(7,Y+14);
        PUT(b(y)); CURSORAT(6,X+14);
        PUT(" "); CURSORAT(6,Y+14); PUT(" "); SCREEN.SIGNALME;
    end SWAP;
begin
       SCREEN.WAITME;CURSORAT(7,1); PUT("Bubble:"); SCREEN.SIGNALME;
       B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       for k in 0..25 loop
           SCREEN.WAITME;CURSORAT(7,k+14);
           put(b(k));    SCREEN.SIGNALME;
       end loop;
       I:= 25;
       while (I > 0) loop                CURSORAT(6,50); PUT("I: "); PUT(I);
         J:= 0;
         while (J < I) loop              CURSORAT(7,50); PUT("J: "); PUT(J);
           if B(J) > B(J+1) then
             SWAP(J,J+1);
           end if;
           J:= J+1;
         end loop;
         I:= I-1;
       end loop;
end BUBBLE_SORT;

begin
end newrace;
