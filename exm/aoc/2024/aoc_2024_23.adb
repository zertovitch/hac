--  Solution to Advent of Code 2024, Day 23
-------------------------------------------
--  LAN Party
--
--  https://adventofcode.com/2024/day/23
--  Copy of questions in: aoc_2024_23_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

with Interfaces;

procedure AoC_2024_23 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_23";

  r : array (Part_Type) of VString;

  dic : Hash_Maps.Hash_Map_Type;

  subtype Id_Range is Integer range 1 .. 1000;
  subtype Computer_Name is String (1 .. 2);

  historian : array (Id_Range) of Boolean;
  connected : array (Id_Range, Id_Range) of Boolean;
  name_of   : array (Id_Range) of Computer_Name;

  last : Id_Range;

  procedure Read_Data is
    use Hash_Maps;
    dummy_separator : Character;
    name : Computer_Name;
    f : File_Type;
    last_64, v1, v2 : Integer_64;

    procedure Register (val : out Integer_64) is
      dummy : Integer_64;
      unknown : constant := 0;
    begin
      Find (dic, +name, unknown, val);
      if val = unknown then
        last_64 := last_64 + 1;
        Insert (dic, +name, last_64, False, dummy);
        val := last_64;
        if name (1) = 't' then
          historian (Id_Range (last_64)) := True;
        end if;
        name_of (Id_Range (last_64)) := name;
      end if;
    end Register;

  begin

    for i in Id_Range loop
      historian (i) := False;
      for j in Id_Range loop
        connected (i, j) := False;
      end loop;
    end loop;
    Clear (dic);
    last_64 := 0;

    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, name);
      Register (v1);
      Get (f, dummy_separator);
      Get (f, name);
      Register (v2);
      connected (Id_Range (v1), Id_Range (v2)) := True;
      connected (Id_Range (v2), Id_Range (v1)) := True;
    end loop;
    Close (f);

    last := Id_Range (last_64);
  end Read_Data;

  verbosity_level : constant := 0;

  procedure Do_Part_1 is
    count_with_historian : Natural := 0;
  begin
    for i in 1 .. last loop
      for j in i + 1 .. last loop
        if connected (i, j) then
          if verbosity_level > 1 then
            Put_Line (+"Connected: " & name_of (i) & ',' & name_of (j));
          end if;
          for k in j + 1 .. last loop
            --  "[...] each computer in the set is connected to the other two computers."
            if connected (j, k)
              and then connected (i, k)
              and then (historian (i) or else historian (j) or else historian (k))
            then
              if verbosity_level > 0 then
                Put_Line (+"Historian in triplet: " & name_of (i) & ',' & name_of (j) & ',' & name_of (k));
              end if;
              count_with_historian := count_with_historian + 1;
            end if;
          end loop;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & count_with_historian;
  end Do_Part_1;

  --  Sorting is copy-pasted-adapted from the BWT example for HAC.
  --  Note the Ada standard has a generic sorting in its library.

  type Table is array (Id_Range) of Computer_Name;

  procedure Shell_Sort (b : in out Table; n : Natural) is
    i, j, step : Integer;
    step_size : array (1 .. 4) of Integer;
    stop : Boolean;
    temp : Computer_Name;
  begin
    --  'steps' contains decreasing increments for each
    --  pass. The last pass has increment 1.
    step_size (4) := 1;
    for pass in reverse 1 .. 3 loop
      step_size (pass) := 2 * step_size (pass + 1);
    end loop;
  Passes :
    for pass in 1 .. 4 loop
      step := step_size (pass);
      --  Do a straight insertion sort with 'step' as
      --  an increment instead of 1.
      i := step + 1;
      while i <= n loop
        temp := b (i);
        j := i;
        stop := False;
        while j > step and not stop loop
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
    end loop Passes;
  end Shell_Sort;

  procedure Do_Part_2 is
    elem_set : array (Id_Range) of Natural;
    set_size, set_last, best, name_count : Natural;
    valid : Boolean;
    t : Table;
    type Pass_Type is (find_largest_set_size, pick_largest_set);
  begin
    best := 0;

    Passes :
    for pass in Pass_Type loop
      Main_Vertex_Loop :
      for i in 1 .. last loop
        if verbosity_level > 1 then
          Put (+"From " & name_of (i) & ": ");
        end if;

        --  Create a group around computer i:
        set_last := 1;
        elem_set (set_last) := i;

        --  We gather all computers connected to i:
        for j in 1 .. last loop
          if connected (i, j) then
            set_last := set_last + 1;
            elem_set (set_last) := j;
            if verbosity_level > 1 then
              Put (name_of (j) & ' ');
            end if;
          end if;
        end loop;
        if verbosity_level > 1 then
          New_Line;
        end if;

        --  Now we have *the* set of all computers connected to i.

        --  Check mutual connections in the set:
        set_size := set_last;
        Check_Connections :
        for elem_i in 1 .. set_last loop
          valid := True;
          for elem_j in elem_i + 1 .. set_last loop
            valid := valid and then connected (elem_set (elem_i), elem_set (elem_j));
            if not valid then
              if verbosity_level > 1 then
                Put_Line
                  (+"  Eliminated: " & name_of (elem_set (elem_i)) &
                    " not connected to " & name_of (elem_set (elem_j)));
              end if;
              exit;
            end if;
          end loop;
          if not valid then
            set_size := set_size - 1;
            elem_set (elem_i) := 0;  --  Cancel the element (no packing)
          end if;
        end loop Check_Connections;

        --  Now we have *a* set of computers connected to i, where computers
        --  are directly connected to each others.
        --  Note that vertex i can belong to multiple such sets.

        if verbosity_level > 0 then
          Put (+"Set size: " & set_size & ": ");
          for elem_i in 1 .. set_last loop
            if elem_set (elem_i) > 0 then
              Put (name_of (elem_set (elem_i)) & ' ');
            end if;
          end loop;
          New_Line;
        end if;

        case pass is

          when find_largest_set_size =>
            best := Max (best, set_size);

          when pick_largest_set =>

            if set_size = best then
              --  We found a set with the maximum size (hope this is the expected one!):
              name_count := 0;
              for elem_i in 1 .. set_last loop
                if elem_set (elem_i) > 0 then
                  name_count := name_count + 1;
                  t (name_count) := name_of (elem_set (elem_i));
                end if;
              end loop;
              Shell_Sort  (t, name_count);
              r (part_2) := +"";
              for i in 1 .. name_count loop
                r (part_2) := r (part_2) & t (i);
                if i < name_count then
                  r (part_2) := r (part_2) & ',';
                end if;
              end loop;
              exit Main_Vertex_Loop;
            end if;

        end case;
      end loop Main_Vertex_Loop;
    end loop Passes;

  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 1227
    --  Part 2: validated by AoC: cl,df,ft,ir,iy,ny,qp,rb,sh,sl,sw,wm,wy
  end if;
end AoC_2024_23;
