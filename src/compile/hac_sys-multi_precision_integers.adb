-----------------------------------------------------------------------------
--  File: hac_sys-multi_precision_integers.ads
-----------------------------------------------------------------------------
--    Aug-2007: - No more generics (Long_Block_type,
--                Block_type,... always the largest possible idea: J.C.)
--              - Fixed Basic(...) (based on J.C.'s remarks)
--    Nov-2006: - Multiply_internal with/without copy of result (automatic
--                detection of when it is needed)
--              - Explicit Multiply_internal for Multi_int * Basic_int
--              - Multiply(multi,basic,multi) available as procedure
--              - useless zeroing of quotient removed
--              - useless zeroing of blocks removed for indices
--                above last possible used in *
--  24-Feb-2002: Div_Rem(u, v, v, r) also possible
--  23-Feb-2002: DEBUG: +: multiplications are verified by dividing the result
--                     +: divisions are verified by comparing i2*q+r and i1
--  15-Feb-2002: "zero" and 1st index in Divide_absolute_normalized
--                 bugs fixed by Duncan Sands (D.S.)

--  To-do/bug symbol: !!

pragma Warnings (".P");
pragma Warnings (".I");
pragma Warnings ("C");

with Ada.Unchecked_Deallocation;

package body HAC_Sys.Multi_Precision_Integers is
  use type Basic_Int;

  function Shift_Left
    (Value  : Block_type;
     Amount : Natural) return Block_type is
  begin
    return Value * (2 ** Amount);
  end Shift_Left;

  function Shift_Right
    (Value  : Block_type;
     Amount : Natural) return Block_type is
  begin
    return Value / (2 ** Amount);
  end Shift_Right;

  function Shift_Left
    (Value  : Long_Block_type;
     Amount : Natural) return Long_Block_type is
  begin
    return Value * (2 ** Amount);
  end Shift_Left;

  function Shift_Right
    (Value  : Long_Block_type;
     Amount : Natural) return Long_Block_type is
  begin
    return Value / (2 ** Amount);
  end Shift_Right;

  --  Internal_error: exception;
  --  Not_done: exception;

  type compar is (smaller, equal, greater);

  function Min (a, b : Index_Int) return Index_Int is
  begin
    return (if a < b then a else b);
  end Min;

  function Max (a, b : Index_Int) return Index_Int is
  begin
    return (if a > b then a else b);
  end Max;

  procedure Reduce_last_nonzero (n : in out Multi_int) is
    old_last : constant Index_Int := n.last_used;
  begin
    if n.zero then -- We avoid de-zeroing accidentally
      return;      -- and returning a false non-zero with rubbish :-)
    end if;

    n.zero := True;
    for i in 0 .. old_last loop -- after old_last it *is* rubbish anyway.
      if n.blk (i) /= 0 then
        n.zero := False;
        n.last_used := i;
      end if;
    end loop;
  end Reduce_last_nonzero;

  function Compare_absolute (i1, i2 : Multi_int) return compar is
    l1, l2 : Index_Int;
  begin
    --  On ne compare que ABS(i1) et ABS(i2)
    l1 := i1.last_used;
    l2 := i2.last_used;
    if l1 > l2 then         -- i1 a plus de blocs non nuls
      return greater;
    elsif l1 < l2 then      -- i1 a moins de blocs non nuls
      return smaller;
    else                       -- i1 et i2 ont le meme nb de blocs
      for i in reverse 0 .. l1 loop -- on parcourt du + signifiant au -
        if    i1.blk (i) > i2.blk (i) then -- <<chiffre>> de i1 plus grand
          return greater;
        elsif i1.blk (i) < i2.blk (i) then -- <<chiffre>> de i1 plus petit
          return smaller;
        end if;
        --  M\^emes chiffres -> au suivant!
      end loop;
      --  Bon, les 2 nombres sont egaux!
      return equal;
    end if;
  end Compare_absolute;

  ----- Informations, conversions

  function Multi (small : Basic_Int) return Multi_int is
    long                           : Long_Block_type_signed;
    abs_long, heading              : Long_Block_type;
    small_enough, case_first_value : Boolean;
    negs                           : constant Boolean := small < 0;
    Conversion_overflow : exception;
  begin
    abs_long := 0;
    long := Long_Block_type_signed (small);
    case_first_value := long = Long_Block_type_signed'First;
    if case_first_value then
      --  `long` is equal to the lowest value for Long_Block_type_signed.
      --  Then, (abs long) will automatically overflow.
      --  For example, for a 64-bit integer, -2**63 is ok, but 2**63 is
      --  larger than the maximum 64-bit integer, 2**63 - 1.
      --  So we have to have a special case for that value.
      small_enough := False;
    else
      abs_long := Long_Block_type (abs long);
      small_enough := abs_long <= Long_Block_type (maxblock);
    end if;
    --
    if small_enough then
      return Multi_int'
             (n =>         0,                            --  One block is enough.
              blk =>      (0 => Block_type (abs_long)),  --  The block contains the number.
              neg =>       negs,
              zero =>      small = 0,
              last_used => 0
             );
    elsif case_first_value then
      heading := Shift_Right
        (Long_Block_type (-(Long_Block_type_signed'First / 2)),  --  E.g. for 64-bit: 2**62.
         Block_type_bits - 1);                                   --  First shift is done by the / 2.
      return Multi_int'
             (n =>         1,                                   --  Two blocks are needed.
              blk =>      (0 => 0,                              --  Block #0
                           1 => Block_type (heading)),          --  Block #1
              neg =>       True,
              zero =>      False,
              last_used => 1
             );
    else
      heading := Shift_Right (abs_long, Block_type_bits);
      if heading <= Long_Block_type (maxblock) then
        return (n =>         1,                                    --  Two blocks are needed.
                blk =>      (0 => Block_type (abs_long and maxblock),  --  Block #0
                             1 => Block_type (heading)),           --  Block #1
                neg =>       negs,
                zero =>      False,
                last_used => 1
               );
      else
        if Shift_Right (heading, Block_type_bits) > Long_Block_type (maxblock) then
           raise Conversion_overflow;
        end if;

        return (n =>     2,   --  Three blocks are needed. (e.g. 31 bits: 15+15+1)
                blk =>  (0 => Block_type (abs_long and maxblock),                      --  Block #0
                         1 => Block_type (heading and maxblock),                   --  Block #1
                         2 => Block_type (Shift_Right (heading, Block_type_bits))  --  Block #2
                         ),
                 neg =>   negs,
                 zero =>  False,
                 last_used => 2
               );
      end if;
    end if;
  end Multi;

  zero : constant Multi_int := Multi (0);
  one : constant Multi_int := Multi (1);

  Blocks_Per_Basic : constant Positive :=
     (Basic_Int'Size + Block_type'Size - 1) / Block_type'Size;

  --  Convert Multi_int to Basic_int (when possible, else: Cannot_fit raised)
  --  2007:
  --  - correct code for block sizes smaller than Basic_int
  --  - fixed usage of negative flag
  function Basic (large : Multi_int) return Basic_Int is
    type Same_as_Basic_natural is mod 2 ** Basic_Int'Size;
    function Shift_Left
      (Value  : Same_as_Basic_natural;
       Amount : Natural) return Same_as_Basic_natural is
    begin
      return Value * (2 ** Amount);
    end Shift_Left;
    result : Same_as_Basic_natural;
    block_value : Block_type;
    type Huge_int is mod System.Max_Binary_Modulus;
    last_bit : Natural;
  begin
    if large.zero then -- <- 17-Feb-2002
      return 0;
    end if;
    --  Case: too many blocks (whatever sizes)
    if 1 + large.last_used > Blocks_Per_Basic then
      raise Cannot_fit;
    end if;
    --  Case: block size and contents larger than basic
    block_value := large.blk (large.last_used);
    if Huge_int (block_value) > Huge_int (Basic_Int'Last) then
      raise Cannot_fit;
    end if;
    declare
      tmp : Block_type := block_value;
    begin
      last_bit := 0;
      while tmp /= 0 loop
        tmp := tmp / 2;
        last_bit := last_bit + 1;
      end loop;
    end;
    result := Same_as_Basic_natural (block_value);
    --  If the following loop was on all blocks,
    --  the shift by Block_type_bits in the loop could do meaningless
    --  things the case Basic_int'Size <= Block_Type'Size
    for b in reverse 0 .. large.last_used - 1 loop
      result := Shift_Left (result, Block_type_bits);
      --  An overflow is not detected by shifting (it's the way we want it!)
      --  so we need to detect the overall overflow by locating the
      --  last bit.
      last_bit := last_bit + Block_type_bits;
      if last_bit > Basic_Int'Size - 1 then
        --  ^ "- 1" because of sign bit in Basic_int
        raise Cannot_fit;
      end if;
      result := result + Same_as_Basic_natural (large.blk (b));
    end loop;
    if large.neg then
      return -Basic_Int (result);
    else
      return Basic_Int (result);
    end if;
  end Basic;

  --  14-Feb-2002: "zero" bug fixed by Duncan Sands
  procedure Fill (what : out Multi_int; with_smaller : Multi_int) is
    l : constant Index_Int := with_smaller.last_used;
  begin
    what.zero := with_smaller.zero;

    if with_smaller.zero then
      return;
    end if;

    if what.n < l then
      raise Array_too_small;   -- contenant trop petit
    end if;

    what.blk (0 .. l) := with_smaller.blk (0 .. l); -- copy contents
    what.neg := with_smaller.neg;
    what.last_used := l;
  end Fill;

  procedure Fill (what : out Multi_int; with_basic : Basic_Int) is
  begin
    Fill (what, Multi (with_basic));
  end Fill;

  function Bits_per_block return Positive is
  begin
    return Block_type_bits;
  end Bits_per_block;

  ---------------------------
  ----- Unary operators -----
  ---------------------------

  function "+" (i : Multi_int) return Multi_int is begin return i; end "+";

  procedure Opp (i : in out Multi_int) is
  begin
    i.neg := not i.neg; -- -0 possible, anyway i.zero = True in such a case
  end Opp;

  function "-" (i : Multi_int) return Multi_int is
    res : Multi_int (i.n) := i; -- copy + stack :-(
  begin
    Opp (res);
    return res;
  end "-";

  procedure Abso (i : in out Multi_int) is
  begin
    i.neg := False;
  end Abso;

  function "Abs" (i : Multi_int) return Multi_int is
    abs_i : Multi_int (i.n) := i; -- copy + stack :-(
  begin
    abs_i.neg := False;
    return abs_i;
  end "Abs";

  function Sign (i : Multi_int) return Basic_Int is
  begin
    if    i.zero then return  0;
    elsif i.neg  then return -1;
    else              return +1;
    end if;
  end Sign;

  function Even (i : Multi_int) return Boolean is
  begin
    return i.zero or else i.blk (0) mod 2 = 0;
  end Even;

  function Odd (i : Multi_int) return Boolean is
  begin
    return (not i.zero) and then i.blk (0) mod 2 = 1;
  end Odd;

  ----------------------------
  ----- Binary operators -----
  ----------------------------

  --  Internal algorithm to add two numbers AS POSITIVE ( > 0 ) !

  procedure Add_absolute (i1, i2 : in Multi_int; i3 : out Multi_int) is
    l1 : constant Index_Int := i1.last_used;
    l2 : constant Index_Int := i2.last_used;
    min_ind : constant Index_Int := Min (l1, l2);
    max_ind : constant Index_Int := Max (l1, l2);
    s : Long_Block_type := 0;
    retenue_finale : Block_type;
  begin

    if max_ind > i3.n then
      raise Result_undersized;
    end if; -- 17-Feb-2002

    --  (1) On additionne sur le <<support commun>>
    for ind in 0 .. min_ind loop
      s := Long_Block_type (i1.blk (ind)) + Long_Block_type (i2.blk (ind)) +
              Shift_Right (s, Block_type_bits); --  (retenue)
      i3.blk (ind) := Block_type (s and maxblock);
      --  NB: dans un cas de Add(a,b,a) ou Add(a,b,b),
      --  i1.blk(ind) ou i2.blk(ind) est modifie en meme temps!
    end loop;

    --  (2) On poursuit au besoin si i1 a plus de blocs...
    if l1 > min_ind then
      for ind in min_ind + 1 .. max_ind loop
        s := Long_Block_type (i1.blk (ind)) +
              Shift_Right (s, Block_type_bits); --  (retenue)
        i3.blk (ind) := Block_type (s and maxblock);
      end loop;
    --  ... ou bien si i2 en a plus.
    elsif l2 > min_ind then
      for ind in min_ind + 1 .. max_ind loop
        s := Long_Block_type (i2.blk (ind)) +
              Shift_Right (s, Block_type_bits); --  (retenue)
        i3.blk (ind) := Block_type (s and maxblock);
      end loop;
    end if;

    --  (3) Il peut rester une retenue
    retenue_finale := Block_type (Shift_Right (s, Block_type_bits));
    if retenue_finale /= 0 then
      if max_ind + 1 > i3.n then
        raise Result_undersized;
      end if; -- 17-Feb-2002
      i3.blk (max_ind + 1) := retenue_finale;
      i3.last_used := max_ind + 1;
    else
      i3.last_used := max_ind;
    end if;

    --  (4) i3 = i1+i2 > 0
    i3.neg := False;
    i3.zero := False;

  end Add_absolute;

  --  Internal algorithm to subtract two numbers AS POSITIVE ( > 0 ) !

  procedure Sub_absolute (i1, i2 : in Multi_int; i3 : in out Multi_int;
                         sgn : out Boolean) is
    l1 : constant Index_Int := i1.last_used;
    l2 : constant Index_Int := i2.last_used;
    max_ind : constant Index_Int := Max (l1, l2);
    ai, bi : Long_Block_type;
    s : Block_type;
    retenue_finale : Long_Block_type;
  begin

    if max_ind > i3.n then raise Result_undersized; end if; -- 17-Feb-2002

    i3.last_used := 0;
    i3.zero := True;
    s := 0;

    --  (1) Soustraction avec retenue
    for ind in 0 .. max_ind loop
      if ind <= l1 then
        ai := Long_Block_type (i1.blk (ind));
      else
        ai := 0;
      end if;
      if ind <= l2 then
        bi := Long_Block_type (i2.blk (ind)) + Long_Block_type (s);
      else
        bi := Long_Block_type (s);
      end if;

      if ai < bi then
        ai := ai + cardblock;
        s := 1;
      else
        s := 0;
      end if;

      i3.blk (ind) := Block_type (ai - bi);
      --  NB: dans un cas de Sub(a,b,a) ou Sub(a,b,b),
      --  i1.blk(ind) ou i2.blk(ind) est modifie en meme temps!

      if i3.blk (ind) /= 0 then -- au passage, on corrige .last_used et .zero
        i3.last_used := ind;
        i3.zero := False;
      end if;
    end loop;

    --  (2) Traitement de la derni\`ere retenue
    if s = 0 then
      i3.neg := False;
      sgn    := False;
    else
      i3.neg := True;
      sgn    := True;
      i3.last_used := 0;
      retenue_finale := 1; -- on fait "9-chaque chiffre" et on ajoute 1 au tout
      for i in 0 .. max_ind loop
        retenue_finale :=
          Long_Block_type (maxblock) -
          Long_Block_type (i3.blk (i)) + retenue_finale;
        i3.blk (i) := Block_type (retenue_finale and maxblock);
        if i3.blk (i) /= 0 then
          i3.last_used := i;
        end if;
        retenue_finale := Shift_Right (retenue_finale, Block_type_bits);
      end loop;
    end if;

  end Sub_absolute;

  procedure Add (i1, i2 : in Multi_int; i3 : in out Multi_int) is
    sgn : Boolean;
  begin
    --  (1) Les cas o\`u i1 ou i2 = 0
    if i1.zero and i2.zero then
      i3.zero := True;
    elsif i1.zero then
      Fill (i3, i2);
    elsif i2.zero then
      Fill (i3, i1);
    --  (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes
    --  (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Add_absolute (i1, i2, i3); -- On fait comme si i1>0 et i2>0
      i3.neg := i1.neg;           -- et on met le bon signe
    --  (2.2) i1 < 0, i2 > 0, donc i3 = i2 - abs(i1)
    elsif i1.neg and not i2.neg then
      Sub_absolute (i2, i1, i3, sgn);
    --  (2.3) i1 > 0, i2 < 0, donc i3 = i1 - abs(i2)
    elsif i2.neg and not i1.neg then
      Sub_absolute (i1, i2, i3, sgn);
    end if;
  end Add;

  function "+" (i1, i2 : Multi_int) return Multi_int is
    somme : Multi_int (Max (i1.n, i2.n) + 1);
  begin
    Add (i1, i2, somme);
    return somme;
  end "+";

  procedure Sub (i1, i2 : in Multi_int; i3 : in out Multi_int) is
    sgn : Boolean;
  begin
    --  (1) Les cas o\`u i1 ou i2 = 0
    if    i1.zero and i2.zero then i3.zero := True;
    elsif i1.zero then Fill (i3, i2); i3.neg := not i2.neg;
    elsif i2.zero then Fill (i3, i1);

    --  (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes

    --  (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Sub_absolute (i1, i2, i3, sgn); -- On fait comme si i1>0 et i2>0
                                      --  et on met le bon signe
    i3.neg := i1.neg xor sgn;
    --  26-Mar-2002: equivalent a:
    --      if i1.neg then
    --        i3.neg:= NOT sgn;
    --      else
    --        i3.neg:= sgn;
    --      end if;

    --  (2.2) i1 < 0, i2 > 0, donc i3 = i1-i2 = - (abs(i1) + abs(i2))
    elsif i1.neg and not i2.neg then
      Add_absolute (i1, i2, i3);
      i3.neg := True;

    --  (2.3) i1 > 0, i2 < 0, donc i3 = i1-i2 = i1 + (-i2) = i1 + abs(i2)
    elsif i2.neg and not i1.neg then
      Add_absolute (i1, i2, i3);
    end if;

  end Sub;

  function "-" (i1, i2 : Multi_int) return Multi_int is
    diff : Multi_int (Max (i1.n, i2.n) + 1); -- +1: retenue possible (add_abs.)
  begin
    Sub (i1, i2, diff);
    return diff;
  end "-";

  function "+" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
  begin return i1 + Multi (i2); end "+";

  function "+" (i1 : Basic_Int; i2 : Multi_int) return Multi_int is
  begin return Multi (i1) + i2; end "+";

  function "-" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
  begin return i1 - Multi (i2); end "-";

  function "-" (i1 : Basic_Int; i2 : Multi_int) return Multi_int is
  begin return Multi (i1) - i2; end "-";

  ----- Begin of MULTIPLICATION part -----

  --  Added 2006: choice to copy result into i3 or write directly into i3
  generic
    copy : Boolean;
  procedure Multiply_internal_m_m (i1, i2 : in Multi_int; i3 : in out Multi_int);

  type p_Block_array is access Block_array;
  procedure Dispose is new Ada.Unchecked_Deallocation (Block_array, p_Block_array);

  -------------------
  -- Multi * Multi --
  -------------------

  --  To do: implement a faster algorithm.
  --  1) Karatsuba's algorithm
  --  Ada code for string arithm exists !!
  --  http://www.csc.liv.ac.uk/~ped/teachadmin/algor/karatsuba.ada
  --  2) Better: Schönhage-Strassen algorithm (no Ada code)

  procedure Multiply_internal_m_m (i1, i2 : in Multi_int; i3 : in out Multi_int) is
    l1 : constant Index_Int := i1.last_used;
    l2 : constant Index_Int := i2.last_used;
    last_max : constant Index_Int := l1 + l2 + 2;
    prod, sum_carry, rk, i1j : Long_Block_type;
    i, k : Index_Int;
    res : p_Block_array;
    --  res: buffer used in the "copy" variant to avoid
    --  problems with Multiply(i,j,i) or Multiply(j,i,i)
  begin
    if i1.zero or i2.zero then
      i3.zero := True;
      return;
    end if;

    if last_max > i3.n then
      raise Result_undersized;
    end if;

    if copy then
      res := new Block_array (0 .. last_max);
      for k in res'Range loop res (k) := 0; end loop;
      --  Seems slower :-( :  res:= new Block_array'( 0..last_max => 0);
    else
      for k in 0 .. last_max loop i3.blk (k) := 0; end loop;
      --  Slower :-( :  i3.blk(0..last_max):= (others => 0);
    end if;

    i3.zero := False;
    i3.last_used := last_max;
    --  NB: va changer i1.last_used ou i2.last_used si
    --  i1 ou i2 et i3 sont les memes

    for j in 0 .. l1 loop
      i1j := Long_Block_type (i1.blk (j));
      sum_carry := 0;
      i := 0;
      k := j;
      loop
        if i <= l2 then
          prod := i1j * Long_Block_type (i2.blk (i));
        else
          exit when sum_carry = 0; -- nothing more to add
          prod := 0;
        end if;
        if copy then
          rk := Long_Block_type (res (k));
        else
          rk := Long_Block_type (i3.blk (k));
        end if;
        sum_carry := rk + prod + sum_carry;
        if copy then
          res (k) := Block_type (sum_carry and maxblock); -- somme
        else
          i3.blk (k) := Block_type (sum_carry and maxblock); -- somme
        end if;
        sum_carry := Shift_Right (sum_carry, Block_type_bits); -- retenue
        i := i + 1;
        k := k + 1;
      end loop;
    end loop;

    if copy then
      i3.blk (res'Range) := res.all;
      Dispose (res);
    end if;

    Reduce_last_nonzero (i3);

    i3.neg := i1.neg /= i2.neg;

  end Multiply_internal_m_m;

  procedure Multiply_internal_copy is
    new Multiply_internal_m_m (copy => True);
  procedure Multiply_internal_copy_export (i1, i2 : in Multi_int; i3 : in out Multi_int)
    renames Multiply_internal_copy;
  --  ^ At least GNAT <= GPL 2006 requires the trick with renames...
  --   ObjectAda 7.2.2 too -> there must be a good reason...

  procedure Multiply_internal_no_copy is
    new Multiply_internal_m_m (copy => False);

  -------------------
  -- Multi * Basic --
  -- added 2006    --
  -------------------

  generic
    copy : Boolean;
  procedure Multiply_internal_m_b (i1 : in Multi_int; i2 : Basic_Int; i3 : in out Multi_int);

  procedure Multiply_internal_m_b (i1 : in Multi_int; i2 : Basic_Int; i3 : in out Multi_int) is
    l1 : constant Index_Int := i1.last_used;
    last_max : constant Index_Int := l1 + 2;
    prod, sum_carry, rk, i2a : Long_Block_type;
    k : Index_Int;
    res : p_Block_array;
    --  res: buffer used in the "copy" variant to avoid
    --  problems with Multiply(i,j,i) or Multiply(j,i,i)
  begin
    if i1.zero or i2 = 0 then
      i3.zero := True;
      return;
    end if;

    if last_max > i3.n then
      raise Result_undersized;
    end if;

    if copy then
      res := new Block_array (0 .. last_max);
      for k in res'Range loop res (k) := 0; end loop;
      --  Seems slower :-( :  res:= new Block_array'( 0..last_max => 0);
    else
      for k in 0 .. last_max loop i3.blk (k) := 0; end loop;
      --  Slower :-( :  i3.blk(0..last_max):= (others => 0);
    end if;

    i3.zero := False;
    i3.last_used := last_max;
    --  NB: va changer i1.last_used ou i2.last_used si i1 ou i2 et i3 sont les memes
    i2a := Long_Block_type (abs i2);

    for j in 0 .. l1 loop
      k := j;
      sum_carry := 0;
      prod := Long_Block_type (i1.blk (j)) * i2a;
      loop
        if copy then
          rk := Long_Block_type (res (k));
        else
          rk := Long_Block_type (i3.blk (k));
        end if;
        sum_carry := rk + prod + sum_carry;
        if copy then
          res (k) := Block_type (sum_carry and maxblock); -- somme
        else
          i3.blk (k) := Block_type (sum_carry and maxblock); -- somme
        end if;
        sum_carry := Shift_Right (sum_carry, Block_type_bits); -- retenue
        exit when sum_carry = 0; -- nothing more to add
        prod := 0;
        k := k + 1;
      end loop;
    end loop;

    if copy then
      i3.blk (res'Range) := res.all;
      Dispose (res);
    end if;

    Reduce_last_nonzero (i3);

    i3.neg := i1.neg /= (i2 < 0);

  end Multiply_internal_m_b;

  procedure Multiply_internal_copy is
    new Multiply_internal_m_b (copy => True);

  procedure Multiply_internal_no_copy is
    new Multiply_internal_m_b (copy => False);

  procedure Multiply (i1, i2 : in Multi_int; i3 : in out Multi_int) is
    use System;
  begin
    if Debug then
      declare
        m1 : constant Multi_int := i1;
        m2 : constant Multi_int := i2;
      begin
        Multiply_internal_no_copy (m1, m2, i3);
      end;
    else
      if i1'Address = i3'Address or i2'Address = i3'Address then
        --  Ada.Text_IO.Put_Line("* with copy");
        Multiply_internal_copy (i1, i2, i3);
      else
        --  Ada.Text_IO.Put_Line("* without copy");
        Multiply_internal_no_copy (i1, i2, i3);
      end if;
    end if;
  end Multiply;

  procedure Multiply (i1 : in Multi_int; i2 : Basic_Int; i3 : in out Multi_int) is
    use System;
  begin
    if Debug then
      declare
        m1 : constant Multi_int := i1;
        m2 : constant Basic_Int := i2;
      begin
        Multiply_internal_no_copy (m1, m2, i3);
      end;
    else
      if i1'Address = i3'Address or i2'Address = i3'Address then
        --  Ada.Text_IO.Put_Line("* with copy");
        Multiply_internal_copy (i1, i2, i3);
      else
        --  Ada.Text_IO.Put_Line("* without copy");
        Multiply_internal_no_copy (i1, i2, i3);
      end if;
    end if;
  end Multiply;

  function "*" (i1, i2 : Multi_int) return Multi_int is
  begin
    if i1.zero or i2.zero then
      return zero;
    else
      declare
        prod : Multi_int (i1.last_used + i2.last_used + 2);
      begin
        Multiply (i1, i2, prod);
        return prod;
      end;
    end if;
  end "*";

  function "*" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
  begin
    if i1.zero or i2 = 0 then
      return zero;
    else
      declare
        prod : Multi_int (i1.last_used + 4);
      begin
        Multiply (i1, i2, prod);
        return prod;
      end;
    end if;
  end "*";

  function "*" (i1 : Basic_Int; i2 : Multi_int) return Multi_int is
  begin
    if i2.zero or i1 = 0 then
      return zero;
    else
      declare
        prod : Multi_int (i2.last_used + 4);
      begin
        Multiply (i2, i1, prod);
        return prod;
      end;
    end if;
  end "*";

  ----- Begin of DIVISION part -----

  --  Interne: Division et reste en 1 coup

  procedure Div_Rem (a, b : Long_Block_type; q, r : out Long_Block_type) is
    Conflict_with_REM : exception;
  begin
    q := a / b;
    r := a - b * q;
    if Debug and then r /= (a rem b) then
      raise Conflict_with_REM;
    end if;
  end Div_Rem;

  procedure Divide_absolute_normalized (u : in out Multi_int; -- output: u = r
                                         v : in     Multi_int;
                                         q : in out Multi_int) is
    qi : Index_Int := u.last_used - v.last_used - 1; -- was: q.n; D.S. Feb-2002
    v1 : constant Long_Block_type := Long_Block_type (v.blk (v.last_used));
    v2 : constant Long_Block_type := Long_Block_type (v.blk (v.last_used - 1));

    vlast     : constant Index_Int := v.last_used;
    v1L       : constant Long_Block_type := v1;
    guess,
    comparand : Long_Block_type;

    function Divide_subtract (ustart : Index_Int) return Block_type is
      ui    : Index_Int;
      carry : Long_Block_type;
    begin
      if guess = 0 then
        return 0;
      end if;
      ui := ustart;
      carry := 0;

      --  On soustrait (le chiffre du quotient) * diviseur au dividende

      for vi in 0 .. vlast loop
        declare
          prod : constant Long_Block_type   := Long_Block_type (v.blk (vi)) * guess + carry;
          bpro : constant Block_type := Block_type (prod and maxblock);
          diff : constant Long_Block_type_signed   := Long_Block_type_signed (u.blk (ui)) - Long_Block_type_signed (bpro);
        begin
          if diff < 0 then
            u.blk (ui) := Block_type (diff + cardblock);
            carry := Shift_Right (prod, Block_type_bits) + 1;
          else
            u.blk (ui) := Block_type (diff);
            carry := Shift_Right (prod, Block_type_bits);
          end if;
          ui := ui + 1;
        end;
      end loop;

      if carry = 0 then
        return Block_type (guess and maxblock);
      end if;

      declare
        diff : constant Long_Block_type_signed :=
          Long_Block_type_signed (u.blk (ui)) - Long_Block_type_signed (carry and maxblock);
      begin
          if diff < 0 then
            u.blk (ui) := Block_type (diff + cardblock); -- carry generated
          else
            u.blk (ui) := Block_type (diff);
            return Block_type (guess and maxblock);
          end if;
      end;

      --  Carry was generated
      declare
        icarry : Block_type := 0;
      begin
        ui := ustart;
        for vi in 0 .. vlast loop
          declare
            sum : constant Long_Block_type :=
              Long_Block_type (v.blk (vi)) +
              Long_Block_type (u.blk (ui)) +
              Long_Block_type (icarry);
          begin
            u.blk (ui) := Block_type (sum and maxblock);
            ui := ui + 1;
            icarry := Block_type (Shift_Right (sum, Block_type_bits));
          end;
        end loop;

        if icarry = 1 then
          u.blk (ui) := Block_type ((Long_Block_type (u.blk (ui)) + 1) and maxblock);
        end if;
      end;

      return Block_type ((guess - 1) and maxblock);

    end Divide_subtract;

    is_q_zero : Boolean := True;

  begin -- Divide_absolute_normalized
    --  for i in q.blk'Range loop q.blk(i):= 0; end loop;
    --
    --  ^ zeroing useless: q.last_used = u.last_used-v.last_used-1
    --   and q.blk(0..q.last_used) is written below q.blk(qi) := ...
    --   GM 4-nov-2006

    q.last_used := qi; -- was: q.n; D.S. Feb-2002

    for j in reverse vlast + 1 .. u.last_used loop
      declare
        uj       : constant Long_Block_type := Long_Block_type (u.blk (j));
        uj1      : constant Long_Block_type := Long_Block_type (u.blk (j - 1));
        uj2      : constant Long_Block_type := Long_Block_type (u.blk (j - 2));
        ujL, rmL : Long_Block_type;
      begin
        ujL := Shift_Left (uj, Block_type_bits) + uj1;
        Div_Rem (ujL, v1L, guess, rmL);
        comparand := Shift_Left (rmL, Block_type_bits) + uj2;

        while comparand < v2 * guess loop
          guess := guess - 1;
          comparand := comparand + Shift_Left (v1L, Block_type_bits);
          exit when comparand > cardblock * cardblock;
        end loop;

        q.blk (qi) := Divide_subtract (j - vlast - 1);

        if q.blk (qi) /= 0 and then is_q_zero then -- n'arrive que 0 ou 1 fois
          is_q_zero := False;
          q.last_used := qi;
        end if;

        qi := qi - 1;
      end;

    end loop; -- j

    q.zero := is_q_zero;

  end Divide_absolute_normalized;

  procedure Divide_absolute_big_small (u :   in     Multi_int;
                                        v :   in     Long_Block_type;
                                        q :      out Multi_int;
                                        r :      out Long_Block_type) is
    n : Long_Block_type;
    Quotient_constraint_error : exception;
    last_u_nz :  constant Index_Int := u.last_used;
    u_zero : constant Boolean := u.zero;
    --  in case u and q are the same variables
    is_q_zero : Boolean := True;
  begin
    if q.n < last_u_nz then raise Quotient_constraint_error; end if;
    q.last_used := 0;
    q.neg := False;
    r := 0;
    if not u_zero then
      for i in reverse 0 .. last_u_nz loop
        n := Long_Block_type (u.blk (i)) + Shift_Left (r, Block_type_bits);
        r := n mod v;
        q.blk (i) := Block_type (n  /  v);
        if q.blk (i) /= 0 and then is_q_zero then
          is_q_zero := False;
          q.last_used := i;
        end if;
      end loop;
      q.zero := is_q_zero;
    end if;
  end Divide_absolute_big_small;

  procedure Solve_signs_for_Div_Rem (i1n, i2n : in Boolean; qn, rn : out Boolean) is
  begin
    --  Invariant: i1= i2*q+r   on cherche (pos) = (pos)*(pos)+(pos)

    if i1n and i2n then        -- i1<0;  i2<0  (-i1) = (-i2) *  q  + (-r)
      qn := False; -- Quotient > 0
    --      rn:= True;  -- Reste    < 0
    elsif i1n then             -- i1<0;  i2>0  (-i1) =   i2  *(-q) + (-r)
      qn := True;  -- Quotient < 0
    --      rn:= True;  -- Reste    < 0
    elsif i2n then             -- i1>0;  i2<0    i1  = (-i2) *(-q) +   r
      qn := True;  -- Quotient < 0
    --      rn:= False; -- Reste    > 0
    else                       -- i1>0;  i2>0    i1  =   i2  *  q  +   r
      qn := False; -- Quotient > 0
    --      rn:= False; -- Reste    > 0
    end if;
    --  on observe que... "(A rem B) has the sign of A " ARM 4.5.5
    --  en effet on peut mettre:
    rn := i1n;
  end Solve_signs_for_Div_Rem;

  procedure Div_Rem (i1 : in     Multi_int; i2 : in     Basic_Int;
                     q :    out Multi_int;  r :    out Basic_Int) is
    i1_neg : constant Boolean := i1.neg;
    --  in case i1 and q are the same variables
    rneg : Boolean;
    lai2, lr : Long_Block_type;
  begin
    if i2 = 0 then raise Division_by_zero; end if;

    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero := True;
      r := 0;
      return;
    end if;

    lai2 := Long_Block_type (abs i2);
    Divide_absolute_big_small (i1, lai2, q, lr);
    r := Basic_Int (lr);

    Solve_signs_for_Div_Rem (i1_neg, i2 < 0, q.neg, rneg);
    if rneg then r := -r; end if;

  end Div_Rem;

  type Div_Rem_mode is (div_only, both);

  generic
    div_rem_output : Div_Rem_mode;
  procedure Div_Rem_internal (i1, i2 : in Multi_int; q, r : in out Multi_int);

  procedure Div_Rem_internal (i1, i2 : in Multi_int; q, r : in out Multi_int) is

    --  Calculate u/v

    procedure Divide_absolute (u, v : in     Multi_int;
                                q, r : in out Multi_int) is
      shift : Integer := 0;
      v1 : Block_type := v.blk (v.last_used);
      v_zero, v1_zero : exception;
      u_work : Multi_int (u.last_used + 2);
      use System;

      procedure Normalization (source : in     Multi_int;
                                target : in out Multi_int) is
        carry : Block_type := 0;
        tl : constant Index_Int := target.last_used;
        blk :  Block_type;
      begin
        for i in 0 .. source.last_used loop
          blk := source.blk (i);
          target.blk (i) := Shift_Left (blk, shift) + carry;
          carry         := Shift_Right (blk, Block_type_bits - shift);
        end loop;
        if source.last_used < tl then
          target.blk (source.last_used + 1) := carry;
        end if;
        for i in source.last_used + 2 .. tl  loop
          target.blk (i) := 0;
        end loop;
      end Normalization;

      procedure Unnormalization (m : in out Multi_int) is
        carry : Block_type := 0;
        blk :  Block_type;
      begin
        for i in reverse 0 .. m.last_used loop
          blk := m.blk (i);
          m.blk (i) := Shift_Right (blk, shift) + carry;
          carry    := Shift_Left (blk, Block_type_bits - shift);
        end loop;
      end Unnormalization;

    begin -- Divide_absolute (multi u / multi v)

      if Debug then
        if v.zero then raise v_zero; end if;
        if v1 = 0 then raise v1_zero; end if;
      end if;

      --  Calculate shift needed to normalize
      u_work.last_used := u_work.n;
      u_work.zero := False;
      while v1 < 2**(Block_type_bits - 1) loop
        shift := shift + 1;
        v1 := v1 * 2;
      end loop;
      if shift = 0 then                  -- no shift needed
        u_work.blk (0 .. u.last_used) := u.blk (0 .. u.last_used);
        u_work.blk (u.last_used + 1 .. u_work.last_used) := (0, 0);
        --  Now, u is copied, so a Div_Rem(u, v, u, r) won't crash

        if v'Address = q'Address then
          declare
            v_work : Multi_int (v.last_used);
          begin
            --  23-Feb-2002: also copy v, in case of a Div_Rem(u, v, v, r)
            v_work.blk (0 .. v.last_used) := v.blk (0 .. v.last_used);
            v_work.neg      := v.neg;
            v_work.zero     := v.zero;
            v_work.last_used := v.last_used;
            --  Now, u is copied, so a Div_Rem(u, v, v, r) won't crash
            --  Ada.Text_IO.Put_Line("* divisor with copy");
            Divide_absolute_normalized (u_work, v_work, q);
          end;
        else
          --  Ada.Text_IO.Put_Line("* divisor without copy");
          Divide_absolute_normalized (u_work, v, q);
        end if;

      else  -- shift needed
        declare
          v_work : Multi_int (v.last_used);
        begin
          v_work.last_used := v_work.n;
          Normalization (u, u_work);
          Normalization (v, v_work);
          Reduce_last_nonzero (v_work);

          Divide_absolute_normalized (u_work, v_work, q);
        end;

        if div_rem_output /= div_only then
          Unnormalization (u_work);
        end if;
      end if;
      q.neg := False; -- check friendly
      if div_rem_output /= div_only then
        u_work.neg := False; -- check friendly
        Reduce_last_nonzero (u_work);
        Fill (r, u_work);
      end if;

    end Divide_absolute;

    l1 : constant Index_Int := i1.last_used;
    l2 : constant Index_Int := i2.last_used;
    rl : Long_Block_type;
  begin -- Div_Rem_internal
    if i2.zero then raise Division_by_zero; end if;

    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero := True;
      r.zero := True;
      return;
    end if;

    if q.n < l1 - l2 then
      --  17-Feb-2002
      raise Quotient_undersized;
    end if;

    if div_rem_output /= div_only and then r.n < Max (l1, l2) then
      --  17-Feb-2002
      raise Remainder_undersized;
    end if;

    if l2 = 0 then
      if l1 = 0 then      -- On a affaire a une ridicule division d'entiers
        q.blk (0) := i1.blk (0) / i2.blk (0);
        if div_rem_output /= div_only then
          r.blk (0) := Block_type (
            abs (
                 Long_Block_type_signed (i1.blk (0))
               - Long_Block_type_signed (i2.blk (0))
               * Long_Block_type_signed (q.blk (0))
            )
          );
        end if;
        q.zero := q.blk (0) = 0;
        q.last_used := 0;
      else                -- multi / entier
        Divide_absolute_big_small (i1, Long_Block_type (i2.blk (0)), q, rl);
        if div_rem_output /= div_only then
          r.blk (0) := Block_type (rl);
        end if;
      end if;
      if div_rem_output /= div_only then
        r.zero := r.blk (0) = 0;
        r.last_used := 0;
      end if;

    else  -- multi / multi

      case Compare_absolute (i2, i1) is

        when greater =>
          q.zero := True;    -- q:=  0;
          q.last_used := 0;
          q.neg := False;

          if div_rem_output /= div_only then
            Fill (r, i1);  -- r:= i1, q:=0 car i1 = 0 * i2 (>i1 en v.abs) + r
          end if;
          return;

        when equal =>
          Fill (q, one); -- Fill( q, Multi(1) );
          r.zero := True;  -- Fill( r, Multi(0) );

        when smaller => -- cas <<normal>>: diviseur < dividende

          Divide_absolute (i1, i2, q, r);

      end case;
    end if;

    Solve_signs_for_Div_Rem (i1.neg, i2.neg, q.neg, r.neg);
  end Div_Rem_internal;

  procedure Div_Rem_internal_div_only is
    new Div_Rem_internal (div_rem_output => div_only);

  procedure Div_Rem_internal_both is
    new Div_Rem_internal (div_rem_output => both);

  procedure Div_Rem_internal_both_export (i1, i2 : in Multi_int; q, r : in out Multi_int)
    renames Div_Rem_internal_both;

  procedure Div_Rem (i1, i2 : in Multi_int; q, r : out Multi_int) is
  begin
    if Debug then
      declare
        m1 : constant Multi_int := i1;
        m2 : constant Multi_int := i2;
      begin
        Div_Rem_internal_both (m1, m2, q, r);
      end;
    else
      Div_Rem_internal_both (i1, i2, q, r);
    end if;
  end Div_Rem;

  procedure Divide (i1, i2 : in Multi_int; q : out Multi_int) is
  begin
    if Debug then
      declare
        m1 : constant Multi_int := i1;
        m2 : constant Multi_int := i2;
        r : Multi_int (Max (i1.last_used, i2.last_used) + 2);
      begin
        Div_Rem_internal_both (m1, m2, q, r);
      end;
    else
      declare
        r : Multi_int (0); -- Fake
      begin
        Div_Rem_internal_div_only (i1, i2, q, r);
      end;
    end if;
  end Divide;

  function "/" (i1, i2 : Multi_int) return Multi_int is
    q : Multi_int (Max (0, i1.last_used - i2.last_used + 1));
    r : Multi_int (Max (i1.last_used, i2.last_used) + 2);
  begin
    Div_Rem (i1, i2, q, r);
    return q;
  end "/";

  function "/" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
    q : Multi_int (i1.last_used + 1);
    r : Basic_Int;
  begin
    Div_Rem (i1, i2, q, r);
    return q;
  end "/";

  function "rem" (i1, i2 : Multi_int) return Multi_int is
    q : Multi_int (Max (0, i1.last_used - i2.last_used + 1));
    r : Multi_int (Max (i1.last_used, i2.last_used) + 2);
  begin
    Div_Rem (i1, i2, q, r);
    return r;
  end "rem";

  function "rem" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
  begin return i1 rem Multi (i2); end "rem";

  function "rem" (i1 : Multi_int; i2 : Basic_Int) return Basic_Int is
    q : Multi_int (i1.last_used + 1);
    r : Basic_Int;
  begin
    Div_Rem (i1, i2, q, r);
    return r;
  end "rem";

  function "mod" (i1, i2 : Multi_int) return Multi_int is
    q : Multi_int (Max (0, i1.last_used - i2.last_used + 1));
    r : Multi_int (Max (i1.last_used, i2.last_used) + 2);
  begin
    --  Ada RM, 4.5.5 Multiplying Operators
    --  (8)
    --  The signed integer modulus operator is defined such that
    --  the result of A mod B has the sign of B and an absolute value
    --  less than the absolute value of B; in addition, for some signed
    --  integer value N, this result satisfies the relation:
    --  (9) A = B*N + (A mod B)

    Div_Rem (i1, i2, q, r);
    if r.zero or else i2.neg = r.neg then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2 + r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "mod";

  function "mod" (i1 : Multi_int; i2 : Basic_Int) return Multi_int is
  begin return i1 mod Multi (i2); end "mod";

  function "mod" (i1 : Multi_int; i2 : Basic_Int) return Basic_Int is
    r : constant Basic_Int := i1 rem i2;
  begin
    if r = 0 or else (i2 < 0) = (r < 0) then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2 + r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "mod";

----- End of DIVISION part ------

----- Begin of POWER part -------

  procedure Power (i : Multi_int; n : Natural; ipn : out Multi_int) is
    max_ipn_last : Index_Int; -- 17-Feb-2002
  begin
    if i.zero then
      if n = 0 then
        raise Zero_power_zero;
      else
        --  The 0**n = 0 case (17-Feb-2002).
        ipn.zero := True; -- 4-Nov-2006, was: Fill( ipn, Multi(0) );
        return;
      end if;
    end if;

    max_ipn_last := ((1 + i.last_used) * Index_Int (n) - 1) + 2;
    if ipn.n < max_ipn_last then
      raise Result_undersized;
    end if;

    case n is
      when 0 => Fill (ipn, one); -- the i**0 = 1 case
      when 1 => Fill (ipn, i);    -- the i**1 = i case
      when others =>
        declare
          nn : Natural := n - 1;
          i0, ii : Multi_int (max_ipn_last);
        begin
          Fill (i0, i);
          Fill (ii, i0);

          while nn > 0 loop
            if nn mod 2 = 0 then -- x^(2 c) = (x^2) ^c
              Mult (i0, i0, i0);
              nn := nn / 2;
            else
              Mult (i0, ii, ii);
              nn := nn - 1;
            end if;
          end loop;
          Fill (ipn, ii);
        end;
    end case;
  end Power;

  function "**" (i : Multi_int; n : Natural) return Multi_int is
    ipn : Multi_int ((1 + i.last_used) * Index_Int (n) + 2);
  begin
    Power (i, n, ipn);
    return ipn;
  end "**";

  procedure Power (i : Multi_int; n : Multi_int; ipn : out Multi_int;
                   modulo : Multi_int) is
    max_ipn_last : Index_Int;
  begin
    if i.zero then
      if n.zero then
        raise Zero_power_zero;
      else
        --  The 0**n = 0 case (17-Feb-2002).
        ipn.zero := True; -- 4-Nov-2006, was: Fill( ipn, Multi(0) );
        return;
      end if;
    end if;

    if n.neg then
     raise Power_negative;
    end if;

    if modulo.zero or else (i.neg or modulo.neg) then
      raise Power_modulo_non_positive;
    end if;

    max_ipn_last := 2 * modulo.last_used + 2;
    if ipn.n < max_ipn_last then
      raise Result_undersized;
    end if;

    if n.zero then
      Fill (ipn, one); -- the i**0 = 1 case
    elsif Equal (n, one) then
      Fill (ipn, i);    -- the i**1 = i case
    else
      declare
        nn : Multi_int (n.n) := n;
        i0, ii, dummy : Multi_int (max_ipn_last);
        dummy_b : Basic_Int;
      begin
        Subtract (nn, one, nn); -- nn:= nn - 1;
        Fill (i0, i);
        Fill (ii, i0);

        while nn > 0 loop
          if Even (nn) then                -- x^(2 c) = (x^2) ^c
            Mult (i0, i0, i0);
            Div_Rem (nn, 2, nn, dummy_b);  -- nn:= nn/2
            Div_Rem (i0, modulo, dummy, i0);  -- i0:= i0 mod modulo
          else
            Mult (i0, ii, ii);
            Subtract (nn, one, nn);      -- nn:= nn - 1;
            Div_Rem (ii, modulo, dummy, ii);  -- ii:= ii mod modulo
          end if;
        end loop;
        Fill (ipn, ii);
      end;
    end if;
  end Power;

----- End of POWER part ---------

----- Comparisons

  function Equal (i1, i2 : Multi_int) return Boolean is
  begin
    return
      (i1.zero and then i2.zero)
      or else
        (i1.zero = i2.zero and then
         i1.neg  = i2.neg  and then
         i1.last_used = i2.last_used and then
         i1.blk (0 .. i1.last_used) = i2.blk (0 .. i2.last_used));
  end Equal;

  function Equal (i1 : Multi_int; i2 : Basic_Int) return Boolean is
  begin
    return Equal (i1, Multi (i2));
  end Equal;

  function ">" (i1, i2 : Multi_int) return Boolean is
  begin
    --  (1) Cas \'evident o\`u:         i1 <= i2
    if (i1.zero or i1.neg) and then             -- i1 <= 0 et
       (i2.zero or not i2.neg)                  -- i2 >= 0
    then
        return False;
    end if;

    --  (2.1) Cas \'evident o\`u:       i1 > i2
    if ((not i1.zero) and not i1.neg) and then  -- i1 > 0 et
       (i2.zero or i2.neg)                      -- i2 <= 0
    then
        return True;
    end if;

    --  (2.2) Cas \'evident o\`u:       i1 > i2
    if (i1.zero or not i1.neg) and then         -- i1 >= 0 et
       ((not i2.zero) and i2.neg)               -- i2 < 0
    then
        return True;
    end if;

    --  Cas faciles resolus:
    --  i1 > i2  -  0  +
    -------------------
    --  -       #  F  F
    --  0       T  F  F
    --  +       T  T  #

    --  On a les cas avec "#", o\`u i1 et i2 ont le meme signe

    if i1.neg then
      return not (Compare_absolute (i1, i2) = greater);
    else
      return     (Compare_absolute (i1, i2) = greater);
    end if;

  end ">";

  function ">" (i1 : Multi_int; i2 : Basic_Int) return Boolean is
  begin
    return i1 > Multi (i2);
  end ">";

  function "<" (i1, i2 : Multi_int) return Boolean is
  begin return i2 > i1; end "<";

  function "<" (i1 : Multi_int; i2 : Basic_Int) return Boolean is
  begin
    return i1 < Multi (i2);
  end "<";

  function ">=" (i1, i2 : Multi_int) return Boolean is
  begin return not (i2 > i1); end ">=";

  function ">=" (i1 : Multi_int; i2 : Basic_Int) return Boolean is
  begin
    return i1 >= Multi (i2);
  end ">=";

  function "<=" (i1, i2 : Multi_int) return Boolean is
  begin return not (i1 > i2); end "<=";

  function "<=" (i1 : Multi_int; i2 : Basic_Int) return Boolean is
  begin
    return i1 <= Multi (i2);
  end "<=";

end HAC_Sys.Multi_Precision_Integers;
