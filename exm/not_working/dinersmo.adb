with HAT; use HAT;

procedure dinersmo is

    SCREEN: SEMAPHORE := 1;

    -- task SCREEN is
    --    entry WAIT;
    --    entry SIGNAL;
    -- end SCREEN;

    task DIJKSTRA is
      entry GIVE_BIRTH;
    end DIJKSTRA;

    task HOPPER is
      entry GIVE_BIRTH;
    end HOPPER;

    task SAMMET is
      entry GIVE_BIRTH;
    end SAMMET;

    task GEHANI is
      entry GIVE_BIRTH;
    end GEHANI;

    task CHOPSTICK1 is
      entry PICKUP;
      entry PUTDOWN;
    end CHOPSTICK1;

    task CHOPSTICK2 is
      entry PICKUP;
      entry PUTDOWN;
    end CHOPSTICK2;

    task CHOPSTICK3 is
      entry PICKUP;
      entry PUTDOWN;
    end CHOPSTICK3;

    task CHOPSTICK4 is
      entry PICKUP;
      entry PUTDOWN;
    end CHOPSTICK4;



  NO_MEALS : integer := 5;



  task body DIJKSTRA is
       LENGTH_OF_MEAL: FLOAT;
    begin

        accept GIVE_BIRTH do
          WAIT(SCREEN);
          PUT("Eddy Dijkstra living and breathing");
          NEW_LINE;
          SIGNAL(SCREEN);
        end GIVE_BIRTH;

      for x in 1..NO_MEALS loop

        CHOPSTICK1.PICKUP;
        CHOPSTICK2.PICKUP;

        LENGTH_OF_MEAL := RANDOM(10);

        WAIT(SCREEN);
        PUT("Eddy Dijkstra eating meal"); PUT(x, 2);
        PUT(" with chopsticks 1 and 2");
        PUT(" for"); PUT(LENGTH_OF_MEAL, 2); PUT(" seconds");
        NEW_LINE;
        SIGNAL(SCREEN);

        delay LENGTH_OF_MEAL;

        WAIT(SCREEN);
        PUT("Eddy Dijkstra done");
        NEW_LINE;
        SIGNAL(SCREEN);

        CHOPSTICK1.PUTDOWN;
        CHOPSTICK2.PUTDOWN;
        delay 5.0;
      end loop;

      WAIT(SCREEN);
      PUT("Eddy Dijkstra burp");
      NEW_LINE;
      SIGNAL(SCREEN);

    end DIJKSTRA;

   task body HOPPER is
        LENGTH_OF_MEAL: FLOAT;
    begin
        accept GIVE_BIRTH do
          WAIT(SCREEN);
          PUT(" Gracie Hopper living and breathing");
          NEW_LINE;
          SIGNAL(SCREEN);
        end GIVE_BIRTH;

      for x in 1..NO_MEALS loop
        CHOPSTICK2.PICKUP;
        CHOPSTICK3.PICKUP;

        LENGTH_OF_MEAL := RANDOM(10);

        WAIT(SCREEN);
        PUT(" Gracie Hopper eating meal"); PUT(x, 2);
        PUT(" with chopsticks 2 and 3");
        PUT(" for"); PUT(LENGTH_OF_MEAL, 2); PUT(" seconds");
        NEW_LINE;
        SIGNAL(SCREEN);

        delay LENGTH_OF_MEAL;

        WAIT(SCREEN);
        PUT(" Gracie Hopper done");
        NEW_LINE;
        SIGNAL(SCREEN);

        CHOPSTICK2.PUTDOWN;
        CHOPSTICK3.PUTDOWN;
        delay 5.0;
      end loop;
      WAIT(SCREEN);
      PUT(" Gracie Hopper burp");
      NEW_LINE;
      SIGNAL(SCREEN);
    end HOPPER;

    task body SAMMET is
        LENGTH_OF_MEAL: FLOAT;
    begin
        accept GIVE_BIRTH do
          WAIT(SCREEN);
          PUT("  Jeannie Sammet living and breathing");
          NEW_LINE;
          SIGNAL(SCREEN);
        end GIVE_BIRTH;

      for x in 1..NO_MEALS loop
        CHOPSTICK3.PICKUP;
        CHOPSTICK4.PICKUP;

        LENGTH_OF_MEAL := RANDOM(10);

        WAIT(SCREEN);
        PUT("  Jeannie Sammet eating meal"); PUT(x, 2);
        PUT(" with chopsticks 3 and 4");
        PUT(" for"); PUT(LENGTH_OF_MEAL, 2); PUT(" seconds");
        NEW_LINE;
        SIGNAL(SCREEN);

        delay LENGTH_OF_MEAL;

        WAIT(SCREEN);
        PUT("  Jeannie Sammet done");
        NEW_LINE;
        SIGNAL(SCREEN);

        CHOPSTICK3.PUTDOWN;
        CHOPSTICK4.PUTDOWN;
        delay 5.0;
      end loop;
      WAIT(SCREEN);
      PUT("  Jeannie Sammet burp");
      NEW_LINE;
      SIGNAL(SCREEN);
    end SAMMET;

  task body GEHANI is
        LENGTH_OF_MEAL : FLOAT;
    begin
        accept GIVE_BIRTH do
          WAIT(SCREEN);
          PUT("   Narain Gehani living and breathing");
          NEW_LINE;
          SIGNAL(SCREEN);
        end GIVE_BIRTH;

      for x in 1..NO_MEALS loop
        CHOPSTICK1.PICKUP;
        CHOPSTICK4.PICKUP;

        LENGTH_OF_MEAL := RANDOM(10);

        WAIT(SCREEN);
        PUT("   Narain Gehani eating meal"); PUT(x:2);
        PUT(" with chopsticks 1 and 4");
        PUT(" for"); PUT(LENGTH_OF_MEAL:2); PUT(" seconds");
        NEW_LINE;
        SIGNAL(SCREEN);

        delay LENGTH_OF_MEAL;

        WAIT(SCREEN);
        PUT("   Narain Gehani done");
        NEW_LINE;
        SIGNAL(SCREEN);

        CHOPSTICK1.PUTDOWN;
        CHOPSTICK4.PUTDOWN;
        delay 5.0;
      end loop;
      WAIT(SCREEN);
      PUT("   Narain Gehani burp");
      NEW_LINE;
      SIGNAL(SCREEN);
    end GEHANI;

 -- task body SCREEN is
 --   begin
 --     loop
 --       select
 --         accept WAIT;
 --       or
 --         terminate;
 --       end select;
 --       accept SIGNAL;
 --     end loop;
 --   end SCREEN;

  task body CHOPSTICK1 is
    begin
      loop
        select
          accept PICKUP;
        or
          terminate;
        end select;
        accept PUTDOWN;
      end loop;
    end CHOPSTICK1;

  task body CHOPSTICK2 is
    begin
      loop
        select
          accept PICKUP;
        or
          terminate;
        end select;
        accept PUTDOWN;
      end loop;
    end CHOPSTICK2;

  task body CHOPSTICK3 is
    begin
      loop
        select
          accept PICKUP;
        or
          terminate;
        end select;
        accept PUTDOWN;
      end loop;
    end CHOPSTICK3;

  task body CHOPSTICK4 is
    begin
      loop
        select
          accept PICKUP;
        or
          terminate;
        end select;
        accept PUTDOWN;
      end loop;
    end CHOPSTICK4;

begin
  DIJKSTRA.GIVE_BIRTH;
  HOPPER.GIVE_BIRTH;
  SAMMET.GIVE_BIRTH;
  GEHANI.GIVE_BIRTH;
end dinersmo;
