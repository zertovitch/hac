--  Solution to Advent of Code 2021, Day 16
-------------------------------------------
--  Packet Decoder
--
--  https://adventofcode.com/2021/day/16
--  Copy of questions in: aoc_2021_16_questions.txt
--
with HAL;  --  For a build with "full Ada": files hal*.ad* are in ../../../src

procedure AoC_2021_16 is
  use HAL;
  subtype Bit is Integer range 0 .. 1;
  --
  msg_max : constant := 32000;
  msg : array (1 .. msg_max) of Bit;
  bits : Natural := 0;
  --
  procedure Read_Data is
    input : constant VString := +"aoc_2021_16.txt";
    f : File_Type;
    s : VString;
    digit : Natural;
  begin
    --  Examples:
    --  s := +"D2FE28";                          --  Literal: 2021.
    --  s := +"38006F45291200";                  --  Operator with 2 subpackets: 10, 20.
    --  s := +"EE00D40C823060";                  --  Operator with 3 subpackets: 1, 2, 3.
    --  s := +"8A004A801A8002F478";              --  Sum of versions = 16.
    --  s := +"A0016C880162017C3686B18A3D4780";  --  Sum of versions = 31.
    --  s := +"C200B40A82";                      --  "1 + 2" -> 3.
    --  s := +"9C0141080250320F1802104A08";      --  "1 + 3 = 2 * 2" -> 1.
    --
    Open (f, input); Get_Line (f, s); Close (f);
    --
    for i in 1 ..  Length (s) loop
      digit := Integer_Value (+"16#" &  Element (s, i) & '#');
      for j in reverse 1 .. 4 loop
        msg (bits + j) := digit mod 2;
        digit := digit / 2;
      end loop;
      bits := bits + 4;
    end loop;
  end Read_Data;
  --
  b : Positive := 1;
  vsum : Natural := 0;
  --
  function Process return Natural is
    procedure Decode (var : out Natural; bits : Positive) is
    begin
      var := 0;
      for i in 1 .. bits loop
        var := var * 2 + msg (b);
        b := b + 1;
      end loop;
    end Decode;
    version, id, end_mark, lit, length_id, sub_length, sub_num : Natural;
    res : Integer;
    --
    id_sum     : constant := 0;
    id_product : constant := 1;
    id_minimum : constant := 2;
    id_maximum : constant := 3;
    id_literal : constant := 4;
    id_greater : constant := 5;
    id_less    : constant := 6;
    id_equal   : constant := 7;
    --
    procedure Cumulate (first : Boolean) is
      term : constant Integer := Process;  --  Here is the recursion :-) .
    begin
      if first then
        res := term;
      else
        case id is
          when id_sum     => res := res + term;
          when id_product => res := res * term;
          when id_minimum => res := Min (res, term);
          when id_maximum => res := Max (res, term);
          when id_greater => if res > term then res := 1; else res := 0; end if;
          when id_less    => if res < term then res := 1; else res := 0; end if;
          when id_equal   => if res = term then res := 1; else res := 0; end if;
          when others     => null;
        end case;
      end if;
    end Cumulate;
    b_mem : Positive;
  begin
    Decode (version, 3);
    vsum := vsum + version;
    Decode (id, 3);
    case id is
      when id_literal =>
        res := 0;
        loop
          Decode (end_mark, 1);
          Decode (lit, 4);
          res := res * 16 + lit;
          exit when end_mark = 0;
        end loop;
      when others =>
        Decode (length_id, 1);
        if length_id = 0 then
          Decode (sub_length, 15);
          b_mem := b;
          loop
            Cumulate (b = b_mem);
            exit when b = b_mem + sub_length;
          end loop;
        else
           Decode (sub_num, 11);
          for i in 1 .. sub_num loop
            Cumulate (i = 1);
          end loop;
        end if;
    end case;
    return res;
  end Process;
  --
  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  r (2) := Process;
  r (1) := vsum;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: sum of version numbers: " & r (1));
    Put_Line (+"Part 2: value of expression:    " & r (2));
    --  Part 1: validated by AoC: 927
    --  Part 2: validated by AoC: 1725277876501
  end if;
end AoC_2021_16;
