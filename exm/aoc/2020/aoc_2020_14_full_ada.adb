--  Solution to Advent of Code 2020, Day 14
-------------------------------------------
--
--  https://adventofcode.com/2020/day/14
--
--  Full Ada version only.
--  This problem is too complicated for HAC 0.08x...
--
with Ada.Calendar,
     Ada.Containers.Hashed_Maps,
     Ada.Text_IO,
     Ada.Integer_Text_IO;

with Interfaces;

procedure AoC_2020_14_full_Ada is

  --  Based on algorithm at https://gist.github.com/badboy/6267743
  --
  function Hash_64_32_shift (key : in Interfaces.Unsigned_64) return Ada.Containers.Hash_Type
  is
    use Interfaces;
    k : Unsigned_64 := key;
  begin
    k := (not k) + Shift_Left (k, 18);     -- key = (key << 18) - key - 1;
    k := k xor Shift_Right (k, 31);
    k := k * 21;                           -- key = (key + (key << 2)) + (key << 4);
    k := k xor Shift_Right (k, 11);
    k := k +   Shift_Left  (k,  6);
    k := k xor Shift_Right (k, 22);

    return Ada.Containers.Hash_Type (k and (2**32 - 1));
  end Hash_64_32_shift;

  procedure Process_Data (
    file_name : String;
    verbose   : Boolean := False;
    exit_part : Positive := 2
  )
  is

    use Ada.Calendar, Ada.Text_IO, Ada.Integer_Text_IO, Interfaces;

    package Addr_Map_Pkg is new
      Ada.Containers.Hashed_Maps (Unsigned_64, Unsigned_64, Hash_64_32_shift, "=");

    package MIO is new Modular_IO (Unsigned_64); use MIO;
    package IIO is new Integer_IO (Integer_64);  use IIO;

    addr, addr_f, addr_base, b, m_or, m_float, m_and, sum, v : Unsigned_64;

    mame_s : String := "ma";
    mask_s : String := "sk = ";
    mem_s  : String := "m[";
    mem_s2 : String := "] = ";

    c : Character;
    f : File_Type;
    mem : Addr_Map_Pkg.Map;
    use Addr_Map_Pkg;
    cu : Cursor;
    single_bit_mask : array (1 .. 36) of Unsigned_64;
    last_X_bit : Natural;
    --  Some post-game statistics:
    written_addresses : Integer_64;
    T1, T2 : Time;
  begin
    Put_Line ("Processing data file: " & file_name);
    T1 := Clock;
    for part in 1 .. 2 loop
      mem.Clear;
      Open (f, In_File, file_name);
      while not End_Of_File (f) loop
        Get (f, mame_s);
        if mame_s (2) = 'a' then  --  "mask"
          Get (f, mask_s);
          m_or := 0;
          m_and := 0;
          b := 2 ** 35;
          if verbose then
            Put ("  Masks: ------[");
          end if;
          last_X_bit := 0;
          for i in 1 .. 36 loop
            Get (f, c);
            if verbose then
              Put (c);
            end if;
            case c is
              when 'X'  =>
                last_X_bit := last_X_bit + 1;
                single_bit_mask (last_X_bit) := b;
                m_and := m_and + b;
              when '1'  => m_or := m_or + b;
              when others => null;
            end case;
            b := b / 2;
          end loop;
          if verbose then
            Put (']');
            New_Line;
            Put ("  Masks: and: "); Put (m_and, 39, Base => 2); New_Line;
            Put ("  Masks:  or: ");  Put (m_or, 39, Base => 2); New_Line;
            if part = 2 then
              Put_Line ("   Single ""X"" bit mask list:");
              for i in 1 .. last_X_bit loop
                Put ("            ");  Put (i, 0);
                Put (" "); Put (single_bit_mask (i),  39, Base => 2); New_Line;
              end loop;
            end if;
          end if;
        else  --  "mem"
          Get (f, mem_s);
          Get (f, addr);
          Get (f, mem_s2);
          Get (f, v);
          if part = 1 then
            --  Value is changed.
            v := (v and m_and) or m_or;
            mem.Include (addr, v);
          else
            addr_base := (addr or m_or) and not m_and;
            if verbose then
              Put ("  Base address : "); Put (addr_base, 0); New_Line;
            end if;
            --  Bit flipping
            m_float := 0;
            --  Get as much combinations of 0's and 1's as there are 'X'es:
            for i in Unsigned_64'(0) .. 2 ** last_X_bit - 1 loop
              m_float := 0;
              for j in 1 .. last_X_bit loop
                if (Shift_Left (Unsigned_64'(1), j - 1) and i) /= 0 then
                  --  Flip the bit of the corresponding 'X':
                  m_float := m_float + single_bit_mask (j);
                end if;
              end loop;
              if verbose then
                Put ("  Compact ""Floating"" representation: ");  Put (i, 0, Base => 2);
                Put (", expanded : ");  Put (m_float, 0, Base => 2);
              end if;
              addr_f := addr_base or m_float;
              if verbose then
                Put ("  Setting @ "); Put (addr_f, 0); Put (" value "); Put (v, 0);  New_Line;
              end if;
              mem.Include (addr_f, v);
            end loop;
          end if;
        end if;
      end loop;
      Close (f);
      sum := 0;
      written_addresses := 0;
      if verbose then
        Put_Line ("----- Summary of memory dump (overwritten addresses only) -----");
      end if;
      cu := mem.First;
      while Has_Element (cu) loop
        sum := sum + Element (cu);
        written_addresses := written_addresses + 1;
        if verbose or Key (cu) = 0 then
          Put (Key (cu)); Put (":"); Put (Element (cu)); New_Line;
        end if;
        Next (cu);
      end loop;
      Put ("    Part "); Put (part, 0);
      Put (" rules.  Sum of all values left in memory: "); Put (sum, 15);
      T2 := Clock;
      Put (". Total of memory addresses written: "); Put (written_addresses, 5);
      Put_Line (". Computation time: " & Duration'Image (T2 - T1) & '.');
      exit when part = exit_part;
    end loop;
    New_Line;
  end Process_Data;
begin
  Process_Data ("aoc_2020_14_exm_1.txt", True, 1);
  Process_Data ("aoc_2020_14_exm_2.txt", True);
  Process_Data ("aoc_2020_14.txt");
  Process_Data ("aoc_2020_14_jc.txt");
  --
  --  aoc_2020_14.txt :
  --    Sum of all values left in memory:  10717676595607
  --    Sum of all values left in memory:   3974538275659
end AoC_2020_14_full_Ada;
