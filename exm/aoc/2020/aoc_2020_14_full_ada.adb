--  Solution to Advent of Code 2020, Day 14
-------------------------------------------
--
--  https://adventofcode.com/2020/day/14
--
--  Full Ada version only.
--  This problem is too complicated for HAC 0.08x...
--
with Ada.Containers.Hashed_Maps,
     Ada.Text_IO,
     Ada.Integer_Text_IO;

with Interfaces;

procedure aoc_2020_14_full_ada is
  use Ada.Text_IO, Ada.Integer_Text_IO, Interfaces;

   --  Based on algorithm at https://gist.github.com/badboy/6267743
   --
   function Hash_64_32_shift (key : in Interfaces.Unsigned_64) return Ada.Containers.Hash_Type
   is
      k : Interfaces.Unsigned_64 := key;
   begin
      k := (not k) + Shift_Left (k, 18);     -- key = (key << 18) - key - 1;
      k := k xor Shift_Right (k, 31);
      k := k * 21;                           -- key = (key + (key << 2)) + (key << 4);
      k := k xor Shift_Right (k, 11);
      k := k +   Shift_Left  (k,  6);
      k := k xor Shift_Right (k, 22);

      return Ada.Containers.Hash_Type (k and (2**32 - 1));
   end Hash_64_32_shift;

  package Addr_Map_Pkg is new
    Ada.Containers.Hashed_Maps (Unsigned_64, Unsigned_64, Hash_64_32_shift, "=");

  package MIO is new Modular_IO (Unsigned_64); use MIO;

  addr, addr_f, b, m_or, m_float, m_and, sum, v : Unsigned_64;

  mame_s : String := "ma";
  mask_s : String := "sk = ";
  mem_s  : String := "m[";
  mem_s2 : String := "] = ";

  c : Character;
  f : File_Type;
  verbose : constant Boolean := False;
  mem : Addr_Map_Pkg.Map;
  use Addr_Map_Pkg;
  cu : Cursor;
  bit_list : array (1 .. 36) of Unsigned_64;
  last_bit : Natural;
begin
  for part in 1 .. 2 loop
    mem.Clear;
    Open (f, In_File, "aoc_2020_14.txt");
    while not End_Of_File (f) loop
      Get (f, mame_s);
      if mame_s (2) = 'a' then  --  "mask"
        Get (f, mask_s);
        m_or := 0;
        m_and := 0;
        b := 2 ** 35;
        if verbose then
          Put ("Masks:       ");
        end if;
        last_bit := 0;
        for i in 1 .. 36 loop
          Get (f, c);
          if verbose then
            Put (c);
          end if;
          case c is
            when 'X'  =>
              last_bit := last_bit + 1;
              bit_list (last_bit) := b;
              m_and := m_and + b;
            when '1'  => m_or := m_or + b;
            when others => null;
          end case;
          b := b / 2;
        end loop;
        if verbose then
          New_Line;
          Put ("Masks: and "); Put (m_and, 39, Base => 2); New_Line;
          Put ("Masks:  or ");  Put (m_or, 39, Base => 2); New_Line;
          Put_Line ("Bit list:");
          for i in 1 .. last_bit loop
            Put ("  ");  Put (i, 0);
            Put ("        "); Put (bit_list (i),  39, Base => 2); New_Line;
          end loop;
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
          --  Bit flip
          m_float := 0;
          for i in Unsigned_64'(0) .. 2 ** last_bit - 1 loop  --  Get all combinations of 0's and 1's
            m_float := 0;
            for j in 1 .. last_bit loop
              if (Shift_Left (Unsigned_64'(1), j - 1) and i) /= 0 then
                m_float := m_float + bit_list (j);
              end if;
            end loop;
            if verbose then
              Put ("Compact: ");  Put (i,  10, Base => 2);
              Put ("  Floating: ");  Put (m_float,  39, Base => 2); New_Line;
            end if;
            addr_f := ((addr or m_or) and (not m_and)) or m_float;
            if verbose then
              Put ("Setting @ "); Put (addr_f); Put ("  value "); Put (v);  New_Line;
            end if;
            mem.Include (addr_f, v);
          end loop;
        end if;
      end if;
    end loop;
    Close (f);
    sum := 0;
    cu := mem.First;
    while Has_Element (cu) loop
      if Key (cu) /= 0 then
        sum := sum + Element (cu);
        if verbose then
          Put (Key (cu)); Put (":"); Put (Element (cu)); New_Line;
        end if;
        Next (cu);
      end if;
    end loop;
    Put ("Sum of all values left in memory: "); Put (sum, 0); New_Line;
  end loop;
  --  Sum of all values left in memory: 10717676595607
  --  Sum of all values left in memory: 3974538275659
end aoc_2020_14_full_ada;
