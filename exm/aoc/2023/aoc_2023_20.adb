--  Solution to Advent of Code 2023, Day 20
-------------------------------------------
--  Pulse Propagation
--
--  https://adventofcode.com/2023/day/20
--  Copy of questions in: aoc_2023_20_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

with Interfaces;

procedure AoC_2023_20 is

  use AoC_Toolbox, HAT, Interfaces;

  subtype Alpha is Character range 'a' .. 'z';

  subtype Link_Range is Integer range 1 .. 10;

  type Destination_Array is array (Link_Range) of Integer;

  type Pulse is (low, high);

  type Input_Info is record
    source : Natural;
    mem    : Pulse;
    cycle  : Natural;  --  Concerns only the pre_final node's inputs.
  end record;

  type Input_Array is array (Link_Range) of Input_Info;

  type Module_Kind is (broadcaster, flip_flop, conjunction, final);

  type Flip_Flop_State is (off, on);

  type Module_Type is record
    kind  : Module_Kind;
    dest  : Destination_Array;  --  0-terminated (a bad idea...)
    name  : VString;            --  Only for display.
    state : Flip_Flop_State;    --  Concerns only flip-flop
    input : Input_Array;        --  Concerns only conjunction; 0-terminated.
  end record;

  map : array (1 .. 60) of Module_Type;
  top : Natural := 0;

  function Name (i : Integer) return VString is
  begin
    if i = 0 then
      return +"Button";
    elsif i < 0 then
      return +"Unknown " & i'Image;  --  Notably, the mysterious "rx" node...
    else
      return map (i).name;
    end if;
  end Name;

  procedure Clear_Machine is
  begin
    for i in map'Range loop
      --  Set the 0-terminators.
      map (i).dest (1) := 0;
      map (i).input (1).source := 0;
    end loop;
  end Clear_Machine;

  procedure Reset_Inputs is
    dest : Integer;
  begin
    for src in 1 .. top loop
      map (src).state := off;
      for d_lnk in Link_Range loop
        dest := map (src).dest (d_lnk);
        exit when dest = 0;
        if dest > 0 and then map (dest).kind = conjunction then
          --  For conjunctions, we need to know the pulse state
          --  of each source, because some sources may have not been
          --  yet activated and have an assumed "low" state.
          for s_lnk in Link_Range loop
            if map (dest).input (s_lnk).source = 0 then
              map (dest).input (s_lnk).source := src;
              map (dest).input (s_lnk).mem    := low;
              map (dest).input (s_lnk).cycle  := 0;  --  Only useful for pre_final.
              --  0-terminate:
              map (dest).input (s_lnk + 1).source := 0;
              exit;
            end if;
          end loop;
          --
        end if;
      end loop;
    end loop;
  end Reset_Inputs;

  broadcaster_idx : Positive;

  --  input_name : constant VString := +"mini_1";
  input_name : constant VString := +"aoc_2023_20";
  --
  procedure Read_Data is
    use Hash_Maps;
    hm : Hash_Map_Type;
    prefix, c : Character;
    key : VString;
    arrow : String (1 .. 3);
    row, i, dest : Integer;
    f : File_Type;

    procedure Get_Key is
    begin
      key := Null_VString;
      while not End_Of_Line (f) loop
        Get (f, c);
        exit when c not in Alpha;
        key := key & c;
      end loop;
    end Get_Key;

  begin
    Clear_Machine;
    Clear (hm);
    top := top + 1;
    key := +"rx";
    map (top).name := key;
    map (top).kind := final;
    Insert (hm, key, top);
  Passes :
    for pass in 1 .. 2 loop
      Open (f, input_name & ".txt");
      while not End_Of_File (f) loop
        Get (f, prefix);
        Get_Key;
        case pass is
          when 1 =>
            --  In pass #1 we just fill the vector `map` with names
            --  and feed the hash map.
            top := top + 1;
            map (top).name := key;
            Insert (hm, key, top);
            Skip_Line (f);
          when 2 =>
            Find (hm, key, row, 0);
            case prefix is
              when 'b' =>
                map (row).kind := broadcaster;
                broadcaster_idx := row;
                map (row).name := 'b' & map (row).name;
              when '%' =>
                map (row).kind := flip_flop;
              when '&' =>
                map (row).kind := conjunction;
              when others =>
                Put_Line ("Bad data!");
            end case;
            Get (f, arrow);
            i := 0;
            loop
              Get_Key;
              Find (hm, key, dest, -1);
              i := i + 1;
              map (row).dest (i) := dest;
              exit when End_Of_Line (f);
              Get (f, c);  --  Space after ','
            end loop;
            --  0-terminate:
            map (row).dest (i + 1) := 0;
        end case;
      end loop;
      Close (f);
    end loop Passes;
  end Read_Data;

  verbosity : constant := 0;

  --  We make a FIFO buffer

  type To_Do_Type is record
    sent     : Pulse;
    from, to : Integer;
  end record;

  buffer : array (0 .. 49) of To_Do_Type;

  read_idx  : Natural;
  write_idx : Natural;

  procedure FIFO_Reset is
  begin
    read_idx  := 0;
    write_idx := 0;
  end FIFO_Reset;

  function FIFO_Is_Full return Boolean is
  begin
    return (write_idx + 1) mod buffer'Length = read_idx;
  end FIFO_Is_Full;

  function FIFO_Is_Empty return Boolean is
  begin
    return write_idx = read_idx;
  end FIFO_Is_Empty;

  procedure Put_To_Do (e : in To_Do_Type) is
  begin
    if FIFO_Is_Full then
      Put_Line (+"FIFO is full! w:" & write_idx & " r:" & read_idx);
      Skip_Line;
    else
      if verbosity >= 2 then
        Put_Line (Name (e.from) & " -" & e.sent'Image & " -> " & Name (e.to));
      end if;
      buffer (write_idx) := e;
      write_idx          := (write_idx + 1) mod buffer'Length;
    end if;
  end Put_To_Do;

  procedure Get_To_Do (e : out To_Do_Type) is
  begin
    if FIFO_Is_Empty then
      Put_Line ("FIFO is empty!");
    else
      e        := buffer (read_idx);
      read_idx := (read_idx + 1) mod buffer'Length;
    end if;
  end Get_To_Do;

  r : array (Part_Type) of Integer_64;

  procedure Simulate is
    e_cur, e : To_Do_Type;
    row : Integer;
    dest : Integer;
    all_cycles : Boolean;
    out_pulse : Pulse;
    ignore : Boolean;
    high_pulses_for_all_inputs : Boolean;
    counter : array (Pulse) of Natural;
    pre_final_idx : Natural := 0;

    procedure Put_Job is
    begin
      counter (e.sent) := counter (e.sent) + 1;
      Put_To_Do (e);
    end Put_Job;

    button_press_count : Natural := 0;

  begin
    Reset_Inputs;
    counter (low)  := 0;
    counter (high) := 0;
  Button_Presses :
    loop
      button_press_count := button_press_count + 1;
      if verbosity >= 2 then
        Put_Line (+"===================  Round number " & button_press_count);
      end if;
      FIFO_Reset;
      e.sent := low;
      e.from := 0;
      e.to   := broadcaster_idx;
      Put_Job;
      while not FIFO_Is_Empty loop
        Get_To_Do (e_cur);
        row := e_cur.to;

        if row > 0 then
          out_pulse := e_cur.sent;
          ignore := False;

          case map (row).kind is

            when broadcaster =>
              null;

            when flip_flop =>
              case e_cur.sent is
                when high =>
                  ignore := True;
                when low =>
                  if map (row).state = off then
                    map (row).state := on;
                    out_pulse := high;
                  else
                    map (row).state := off;
                    out_pulse := low;
                  end if;
              end case;

            when conjunction =>
              if verbosity >= 3 then
                Put_Line ("  For " & Name (row) & ", checking inputs");
              end if;
              high_pulses_for_all_inputs := True;
              for s_lnk in Link_Range loop
                exit when map (row).input (s_lnk).source = 0;
                if verbosity >= 3 then
                  Put_Line
                    ("    " &
                     Name (map (row).input (s_lnk).source) & ": " &
                     map (row).input (s_lnk).mem'Image);
                end if;
                high_pulses_for_all_inputs :=
                  high_pulses_for_all_inputs and
                    map (row).input (s_lnk).mem = high;
              end loop;
              if high_pulses_for_all_inputs then
                out_pulse := low;
              else
                out_pulse := high;
              end if;

            when final =>
              null;

          end case;

          if not ignore then
            --  Broadcasting
            if verbosity >= 2 then
              Put_Line ("  Broadcasting from " & Name (row));
            end if;
            for id in map (row).dest'Range loop
              dest := map (row).dest (id);
              exit when dest = 0;
              e.sent := out_pulse;
              e.from := row;
              e.to   := dest;
              Put_Job;
              if dest > 0 then
                case map (dest).kind is
                  when conjunction =>
                    if verbosity >= 2 then
                      Put_Line ("  Destination is a conjunction: " & Name (dest));
                    end if;
                    for s_lnk in Link_Range loop
                      exit when map (dest).input (s_lnk).source = 0;
                      if map (dest).input (s_lnk).source = row then
                        map (dest).input (s_lnk).mem := out_pulse;
                        if verbosity >= 2 then
                          Put_Line ("  Updated pulse from input " & Name (row));
                        end if;
                        if dest = pre_final_idx
                          and then out_pulse = high
                          and then map (dest).input (s_lnk).cycle = 0
                        then
                          if verbosity >= 1 then
                            Put_Line
                              ("New cycle on the " & Name (row) & " -> " & Name (dest) &
                               " broadcast; button press so far: " & button_press_count);
                          end if;
                          map (dest).input (s_lnk).cycle := button_press_count;
                        end if;
                        exit;
                      end if;
                    end loop;
                    if dest = pre_final_idx then
                      all_cycles := True;
                      for s_lnk in Link_Range loop
                        exit when map (dest).input (s_lnk).source = 0;
                        all_cycles := all_cycles and map (dest).input (s_lnk).cycle > 0;
                      end loop;
                      exit Button_Presses when all_cycles;
                    end if;

                  when final =>
                    --  Put_Line
                    --    (+"FINAL " & count & ", pulse: " &
                    --     out_pulse'Image & ", from " & Name (row));
                    pre_final_idx := row;
                    --  In our data (and seemingly other data as well)
                    --  The final node (rx) has a single predecessor
                    --  (in our case, called "xn"), which is a conjunction
                    --  node. Let's call that node "pre_final".
                    --  The pre_final node will emit a low pulse only
                    --  after all its inputs are high.
                  when others =>
                    null;
                end case;
              end if;
            end loop;
          end if;
        end if;
      end loop;
      if button_press_count = 1000 then
        r (part_1) := Integer_64 (counter (low) * counter (high));
      end if;
    end loop Button_Presses;
    r (part_2) := 1;
    for s_lnk in Link_Range loop
      exit when map (pre_final_idx).input (s_lnk).source = 0;
      r (part_2) :=
        LCM_64
          (r (part_2),
           Integer_64 (map (pre_final_idx).input (s_lnk).cycle));
    end loop;
  end Simulate;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin

  Read_Data;
  Simulate;

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1)'Image);
    Put_Line (+"Part 2: " & r (part_2)'Image);
    --  Part 1: validated by AoC: 806332748
    --  Part 2: validated by AoC: 228060006554227
  end if;
end AoC_2023_20;
