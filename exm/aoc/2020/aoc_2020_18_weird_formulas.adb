with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed, Ada.Strings;
with Ada.Unchecked_Deallocation;

package body AoC_2020_18_Weird_Formulas is

  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
  package RIO is new Ada.Text_IO.Float_IO (Real);

  subtype Leaf is S_Form range nb .. var;
  subtype Neutral is Unary range plus_una .. accol;
  subtype Built_in_function is S_Form range abso .. max;
  subtype Binary_operator is Binary range moins .. puiss;

  type S_Form_Set is array (S_Form) of Boolean;

  par_or_terminal : constant S_Form_Set :=
    (par | croch | accol | nb | var => True, others => False);

  symmetric : constant S_Form_Set :=
    (min | max | plus | fois => True, others => False);

  function Conv_strg (s : S_Form) return String is
  begin
    case s is
      when plus_una   => return "+";
      when moins_una  => return "-";
      when abso    => return "Abs";
      when sign    => return "Sign";
      when step    => return "Step";
      when round   => return "Round";
      when trunc   => return "Trunc";
      when floor   => return "Floor";
      when ceiling => return "Ceiling";
      when expn    => return "Exp";
      when logn    => return "Log";
      when sqrt    => return "Sqrt";
      when sinus   => return "Sin";
      when arcsin  => return "Arcsin";
      when cosinus => return "Cos";
      when arccos  => return "Arccos";
      when tg      => return "Tan";
      when arctg   => return "Arctan";
      when sh      => return "Sinh";
      when arcsinh => return "Arcsinh";
      when ch      => return "Cosh";
      when arccosh => return "Arccosh";
      when th      => return "Tanh";
      when arctanh => return "Arctanh";
      when min     => return "Min";
      when max     => return "Max";
      when par     => return "(";
      when croch   => return "[";
      when accol   => return "{";
      when fois    => return "*";
      when plus    => return "+";
      when moins   => return "-";
      when sur     => return "/";
      when puiss   => return "^";
      when Leaf    => return "";
    end case;
  end Conv_strg;

  function conv_symb_una (c : Character) return S_Form is
  begin
    if c = plus_char then return plus_una;
    elsif c = minus_char then return moins_una;
    else return nb;
    end if;
  end conv_symb_una;

  function conv_symb (c : Character) return S_Form is
  begin
    return (
      if    c = plus_char  then plus
      elsif c = minus_char then moins
      elsif c = times_char then fois
      elsif c = '/' then sur
      elsif c = '^' then puiss
      elsif c = '(' then par
      elsif c = '[' then croch
      elsif c = '{' then accol
     else nb);
  end conv_symb;

  function Conv_mstr (s : S_Form) return String is
  begin
    return To_Upper (Conv_strg (s));
  end Conv_mstr;

  procedure Put (f : Formula; style : Output_style := normal) is
  begin
    Put (Ada.Text_IO.Current_Output, f, style);
  end Put;

  procedure Put (t : in Ada.Text_IO.File_Type; f : Formula; style : Output_style := normal) is
  begin
    Ada.Text_IO.Put (t, Image (f, style));
  end Put;

  function Almost_zero (x : Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_zero;

  function Image (f : p_Formula_Rec; style : Output_style) return String;

  function Image_simple (f : p_Formula_Rec; style : Output_style) return String is
    x : Real;
    use Ada.Text_IO, RIO;
    s : String (1 .. 40);
  begin
    if f = null then
      return "";
    end if;
    case f.s is
      when nb =>
        x := f.n;
        if Almost_zero (x - Real'Floor (x)) and then
          abs x < Real (Long_Integer'Last)
        then
          return Trim (Long_Integer'Image (Long_Integer (x)), Left);
        else
          begin
            Put (s, x, 5, 0);
          exception
            when Layout_Error =>
              Put (s, x);
          end;
          return Trim (s, Left);
        end if;
      when pi =>
        return "pi";
      when var =>
        return To_String (f.v);
      when moins_una =>
        return minus_char & Image (f.left, style);
      when plus_una =>
        return plus_char & Image (f.left, style);
      when Binary_operator =>
        return Image (f.left, style) & Conv_strg (f.s) & Image (f.right, style);
      when Built_in_function =>
        if f.s in Binary then
          return Conv_strg (f.s) & '(' &
            Image (f.left, style) & ',' &
            Image (f.right, style) &
          ')';
        else
          return Conv_strg (f.s) & '(' & Image (f.left, style) & ')';
        end if;
      when par =>
        return '(' & Image (f.left, style) & ')';
      when croch =>
        return '[' & Image (f.left, style) & ']';
      when accol =>
        return '{' & Image (f.left, style) & '}';
    end case;
  end Image_simple;

  function Image (f : p_Formula_Rec; style : Output_style) return String is
  begin
    if f = null then
      return "";
    end if;
    if style = bracketed and then f.s not in Leaf then
      return '{' & Image_simple (f, style) & '}';
    else
      return Image_simple (f, style);
    end if;
  end Image;

  function Image (f : Formula; style : Output_style := normal) return String is
  begin
    return Image (f.root, style);
  end Image;

  function Deep_copy (f : p_Formula_Rec) return p_Formula_Rec is
    g : p_Formula_Rec;
  begin
    if f = null then
      return null;
    end if;
    g := new Formula_Rec (f.s);
    case f.s is
      when nb =>
        g.n := f.n;
      when pi =>
        null;
      when var =>
        g.v := f.v;
      when Unary =>
        g.left := Deep_copy (f.left);
      when Binary =>
        g.left  := Deep_copy (f.left);
        g.right := Deep_copy (f.right);
    end case;
    return g;
  end Deep_copy;

  procedure Dispose is new Ada.Unchecked_Deallocation (Formula_Rec, p_Formula_Rec);

  procedure Deep_delete (f : in out p_Formula_Rec) is
  begin
    if f /= null then
      case f.s is
        when Unary =>
          Deep_delete (f.left);
        when Binary =>
          Deep_delete (f.left);
          Deep_delete (f.right);
        when others =>
          null;
      end case;
      Dispose (f);
      f := null;
    end if;
  end Deep_delete;

  Closing : constant array (Character) of Character :=
    ('(' => ')',
     '[' => ']',
     '{' => '}',
     others => 'X');

  c_fin : constant Character := Character'Val (0);

  procedure Check (expected, found : Character) is
  begin
    if expected = found then
      return;
    end if;
    if found = c_fin then
      Raise_Exception (
        Parse_Error'Identity,
        "End of formula reached, '" & expected & "' is missing");
    else
      Raise_Exception (
        Parse_Error'Identity,
        "'" & expected & "' was expected, found '" & found & ''');
    end if;
  end Check;

  procedure Check_brackets (open, close : Character) is
  begin
    Check (expected => Closing (open), found => close);
  end Check_brackets;

  function No_Spaces (s : String) return String is
    t : String (s'Range);
    j : Integer := s'First - 1;
  begin
    for i in s'Range loop
      case s (i) is
        when ' ' | ASCII.HT | ASCII.CR | ASCII.LF =>
          null;
        when others =>
          j := j + 1;
          t (j) := s (i);
      end case;
    end loop;
    return t (t'First .. j);
  end No_Spaces;

  digit         : constant Character_Set := ('0' .. '9' | '.' => True, others => False);
  expo          : constant Character_Set := ('e' | 'E' => True, others => False);
  digit_or_expo : constant Character_Set := digit or expo;

  first_symbol_after_expo : constant Character_Set :=
    ('0' .. '9' | '+' | '-' => True, others => False);

  procedure Parse (f : out Formula; s : String) is
    str : constant String := No_Spaces (s) & c_fin;
    i : Integer;
    --
    function Expression return p_Formula_Rec is
      --
      function Term return p_Formula_Rec is
        --
        function Factor return p_Formula_Rec is
          --
          function Number return p_Formula_Rec is
            n : constant p_Formula_Rec := new Formula_Rec (nb);
            j : constant Integer := i;
          begin
            while
              digit_or_expo (str (i)) or else
              (expo (str (i - 1)) and then first_symbol_after_expo (str (i)))
            loop
              i := i + 1;
            end loop;
            n.n := Real'Value (str (j .. i - 1));
            return n;
          end Number;

          function Variable_or_function return p_Formula_Rec is
            n : p_Formula_Rec;
            j : constant Integer := i;
          begin
            loop
              i := i + 1;
              exit when not following_character (str (i));
            end loop;
            declare
              chj : constant String := str (j .. i - 1);
              mch : constant String := To_Upper (chj);
            begin
              if str (i) = '(' then
                for s in Built_in_function loop
                  if mch = Conv_mstr (s) then  -- Found a built-in function
                    n := new Formula_Rec (s);
                    exit;
                  end if;
                end loop;
                i := i + 1;
                if n = null then
                  Raise_Exception (Parse_Error'Identity, "User functions not yet supported");
                else
                  n.left := Expression;
                  if n.s in Binary then  --  Function with two arguments: read 2nd argument
                    Check (',', str (i));
                    i := i + 1;
                    n.right := Expression;
                  end if;
                  Check_brackets ('(', str (i));
                  i := i + 1;
                end if;
              elsif mch = "PI" then
                n := new Formula_Rec (pi);
              else
                n := new Formula_Rec (var);
                n.v := To_Unbounded_String (chj);
              end if;
            end;
            return n;
          end Variable_or_function;

          --  Factor
          n, n1 : p_Formula_Rec;
          c     : Character := str (i);
        begin
          n := null;
          if digit (c) then
            n := Number;
          elsif letters (c) then
            n := Variable_or_function;
          elsif c = minus_char or c = plus_char then
            n := new Formula_Rec (conv_symb_una (c));
            i := i + 1;
            n.left := Factor;
          end if;
          c := str (i);
          case c is
            when '^' =>
              --  NB: right-associative in this parser.
              --     x^y^z means x^(y^z) like in R and unlike
              --     in Excel where x^y^z means (x^y)^z.
              --     Ada asks for parenthesization.
              n1 := n;
              n := new Formula_Rec (puiss);
              i := i + 1;
              n.left := n1;
              n.right := Factor;
            when '(' | '[' | '{' =>
              if n = null then
                n := new Formula_Rec (conv_symb (c));
                i := i + 1;
                n.left := Expression;
                Check_brackets (c, str (i));
                i := i + 1;
              end if;
            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
          if n = null then
            Raise_Exception (Parse_Error'Identity, "Unexpected end in factor");
          end if;
          return n;
        end Factor;

        --  Term
        n, left_factor : p_Formula_Rec;
        c : Character;
      begin
        left_factor := Factor;
        c := str (i);
        if c = times_char or c = '/' then
          i := i + 1;
          n := new Formula_Rec (conv_symb (c));
          n.left := left_factor;
          n.right := Term;
          return n;
        else
          return left_factor;
        end if;
      end Term;

      --  Expression
      n, left_term : p_Formula_Rec;
      c : Character;
    begin
      left_term := Term;
      c := str (i);
      if c = plus_char or c = minus_char then
        i := i + 1;
        n := new Formula_Rec (conv_symb (c));
        n.left  := left_term;
        n.right := Expression;
        return n;
      else
        return left_term;
      end if;
    end Expression;

    generic
      oper : S_Form;  --  - or / : these operators are left-associative only !
      ivrs : S_Form;  --  + or *
    procedure Left_Assoc (n : in out p_Formula_Rec);

    procedure Left_Assoc (n : in out p_Formula_Rec) is
      left_part : p_Formula_Rec;
    begin
      if n = null then  --  Should not happen, but who knows...
        return;
      end if;
      --
      --  Recursion
      --
      case n.s is
        when Leaf =>
          null;
        when Unary =>
          Left_Assoc (n.left);
        when Binary =>
          Left_Assoc (n.left);
          Left_Assoc (n.right);
      end case;
      if n.s = oper then
        if n.right /= null and then (n.right.s = oper or n.right.s = ivrs) then
          --  This has been parsed as X - {Y + Z} or  X - {Y - Z},
          --  should be {X - Y} + Z or {X - Y} - Z.
          left_part := n.right.left;  --  Remember Y
          n.right.left := n;
          n := n.right;
          n.left.right := left_part;
          --
          --  Redo on children. See 9-4-3-2 example...
          --
          Left_Assoc (n.left);
          Left_Assoc (n.right);
        end if;
      end if;
    end Left_Assoc;

    procedure Left_Assoc_Plus is new Left_Assoc (plus, moins);  --  !!!
    procedure Left_Assoc_Minus is new Left_Assoc (moins, plus);
    procedure Left_Assoc_Divide is new Left_Assoc (sur, fois);

  begin
    i := 1;
    Deep_delete (f.root);
    f.root := Expression;
    Left_Assoc_Plus (f.root);
    Left_Assoc_Minus (f.root);
    Left_Assoc_Divide (f.root);
    if str (i) /= c_fin then
      Deep_delete (f.root);
      Raise_Exception (Parse_Error'Identity, "Unexpected end in formula (extra symbols)");
    end if;
  exception
    when E : Parse_Error =>
      Deep_delete (f.root);
      Raise_Exception (Parse_Error'Identity, Exception_Message (E));
  end Parse;

  function Parse (s : String) return Formula is
    f : Formula;
  begin
    Parse (f, s);
    return f;
  end Parse;

  procedure Parse (f : out Formula; u : Unbounded_String) is
  begin
    Parse (f, To_String (u));
  end Parse;

  function Parse (u : Unbounded_String) return Formula is
    f : Formula;
  begin
    Parse (f, u);
    return f;
  end Parse;

  ---------------------------------- Evaluate ---------------------------------

  function Sign (x : Real) return Real is
  begin
    if x < 0.0 then
      return -1.0;
    elsif x > 0.0 then
      return +1.0;
    else
      return 0.0;
    end if;
  end Sign;

  function Step (x : Real) return Real is
  begin
    if x < 0.0 then
      return 0.0;
    else
      return 1.0;
    end if;
  end Step;

  function Evaluate (f : p_Formula_Rec) return Real is
    aux : Real;
    use REF;
  begin
    if f = null then
      return 0.0;
    end if;
    case f.s is
      when nb =>
        return f.n;
      when pi =>
        return Ada.Numerics.Pi;
      when var =>
        return 0.0;  --  Hacked.
      when moins_una =>
        return -Evaluate (f.left);
      when plus_una |
           par |
           croch |
           accol =>
        return Evaluate (f.left);
      when plus =>
        return Plus (Evaluate (f.left), Evaluate (f.right));
      when moins =>
        return Minus (Evaluate (f.left), Evaluate (f.right));
      when fois =>
        return Times (Evaluate (f.left), Evaluate (f.right));
      when sur =>
        aux := Evaluate (f.right);
        if Almost_zero (aux) then
          raise Div_By_0;
        elsif Almost_zero (aux - 1.0) then    --  X/1 -> X
          return Evaluate (f.left);
        else
          return Evaluate (f.left) / aux;
        end if;
      when puiss =>
        aux := Evaluate (f.right);
        if Almost_zero (aux - Real'Floor (aux)) then
          return Evaluate (f.left) ** Integer (aux);
          --  Negative arguments are possible in this case
        else
          return Evaluate (f.left) ** aux;
        end if;
      when abso =>
        return abs Evaluate (f.left);
      when sign =>
        return Sign (Evaluate (f.left));
      when step =>
        return Step (Evaluate (f.left));
      when round =>
        return Real'Rounding (Evaluate (f.left));
      when trunc =>
        return Real'Truncation (Evaluate (f.left));
      when floor =>
        return Real'Floor (Evaluate (f.left));
      when ceiling =>
        return Real'Ceiling (Evaluate (f.left));
      when logn =>
        return Log (Evaluate (f.left));
      when expn =>
        return Exp (Evaluate (f.left));
      when sqrt =>
        return Sqrt (Evaluate (f.left));
      when sinus =>
        return Sin (Evaluate (f.left));
      when arcsin =>
        return Arcsin (Evaluate (f.left));
      when cosinus =>
        return Cos (Evaluate (f.left));
      when arccos =>
        return Arccos (Evaluate (f.left));
      when tg =>
        return Tan (Evaluate (f.left));
      when arctg =>
        return Arctan (Evaluate (f.left));
      when sh =>
        return Sinh (Evaluate (f.left));
      when arcsinh =>
        return Arcsinh (Evaluate (f.left));
      when ch =>
        return Cosh (Evaluate (f.left));
      when arccosh =>
        return Arccosh (Evaluate (f.left));
      when th =>
        return Tanh (Evaluate (f.left));
      when arctanh =>
        return Arctanh (Evaluate (f.left));
      when min =>
        return Real'Min (Evaluate (f.left), Evaluate (f.right));
      when max =>
        return Real'Max (Evaluate (f.left), Evaluate (f.right));
    end case;
  end Evaluate;

  function Evaluate (f : Formula) return Real is
  begin
    return Evaluate (f.root);
  end Evaluate;

  ------------------------------- Compare -----------------------------------

  function Equivalent (fa, fb : p_Formula_Rec) return Boolean;

  --  Special cases.
  --    We check these equivalences because the concerned formula
  --    parts are unlikely to be changed by the Simplify procedure.
  --    E.g. we don't want to change x/2 into x*0.5; then, we check
  --    the equivalence.

  --  X * cst, or cst * X, equivalent to X / (1/cst)
  function Equivalent_Times_Div (fa, fb : p_Formula_Rec) return Boolean is
  begin
    return
      fa /= null and then fb /= null and then
      (fa.s = fois and fb.s = sur) and then
      fb.right /= null and then fb.right.s = nb and then
      (
        ( --  check X * cst
          Equivalent (fa.left, fb.left) and then
          fa.right /= null and then fa.right.s = nb and then
          Almost_zero (fa.right.n * fb.right.n - 1.0)
        )
        or else
        ( --  check cst * X
          Equivalent (fa.right, fb.left) and then
          fa.left /= null and then fa.left.s = nb and then
          Almost_zero (fa.left.n * fb.right.n - 1.0)
        )
      );
  end Equivalent_Times_Div;

  --  General case:
  function Equivalent (fa, fb : p_Formula_Rec) return Boolean is
    ga, gb : S_Form;
  begin
    if fa = null then
      return fb = null;
    end if;
    if fb = null then
      return False;
    end if;
    --  fa and fb are not null, at this point
    ga := fa.s;
    gb := fb.s;
    if ga in Neutral then
      return Equivalent (fa.left, fb); -- +A, (A), [A], {A}  -> A
    elsif gb in Neutral then
      return Equivalent (fa, fb.left); -- +B, (B), [B], {B}  -> B
    elsif ga = gb then
      --  Formulas' nodes a and b are of the same kind
      case ga is
        when nb =>
          return Almost_zero (fa.n - fb.n);
        when pi =>
          return True;
        when var =>
          return fa.v = fb.v;  --  same names
        when Unary =>
          return Equivalent (fa.left, fb.left);
        when Binary =>
          return
              (Equivalent (fa.left, fb.left) and then
               Equivalent (fa.right, fb.right))
            or else
              --  Detect that X * Y is equivalent to Y * X
              (symmetric (ga) and then
               Equivalent (fa.left, fb.right) and then
               Equivalent (fa.right, fb.left));
      end case;
    else
      --  Formulas' nodes a and b are not of the same kind, but...
      return
        Equivalent_Times_Div (fa, fb) or else
        Equivalent_Times_Div (fa => fb, fb => fa);
    end if;
  end Equivalent;

  function Equivalent (fa, fb : Formula) return Boolean is
  begin
    return Equivalent (fa.root, fb.root);
  end Equivalent;

  function Identical (fa, fb : p_Formula_Rec) return Boolean is
  begin
    if fa = null then
      return fb = null;
    end if;
    if fb = null then
      return False;
    end if;
    --  fa and fb are not null, at this point
    if fa.s /= fb.s then
      return False;
    end if;
    --  Formulas' nodes a and b are of the same kind
    case fa.s is
      when nb =>
        return Almost_zero (fa.n - fb.n);
      when pi =>
        return True;
      when var =>
        return fa.v = fb.v;  --  same names
      when Unary =>
        return Identical (fa.left, fb.left);
      when Binary =>
        return Identical (fa.left, fb.left) and then Identical (fa.right, fb.right);
    end case;
  end Identical;

  function Identical (fa, fb : Formula) return Boolean is
  begin
    return Identical (fa.root, fb.root);
  end Identical;

  ------------------------------- Simplify ----------------------------------

  function Is_constant (a : p_Formula_Rec; cst : Real) return Boolean is
  begin
    return a /= null and then a.s = nb and then a.n = cst;
  end Is_constant;

  function Is_constant_pair (a : p_Formula_Rec) return Boolean is
  begin
    return
      a.s in Binary and then
      a.left /= null and then
      a.right /= null and then
      a.left.s = nb and then
      a.right.s = nb;
  end Is_constant_pair;

  function Build_2X (X : p_Formula_Rec) return p_Formula_Rec is  --  returns 2*X
    aux : constant p_Formula_Rec := new Formula_Rec (fois);
  begin
    aux.left := new Formula_Rec (nb);
    aux.left.n := 2.0;
    aux.right := X;
    return aux;
  end Build_2X;

  function Build_X_pow_2 (X : p_Formula_Rec) return p_Formula_Rec is  --  returns X^2
    aux : constant p_Formula_Rec := new Formula_Rec (puiss);
  begin
    aux.left := X;
    aux.right := new Formula_Rec (nb);
    aux.right.n := 2.0;
    return aux;
  end Build_X_pow_2;

  procedure Simplify (f : in out p_Formula_Rec) is
    aux,
    nexp : p_Formula_Rec;

    procedure left_replaces_f is
    begin
      aux := f.left;
      f.left := null; --  empˆeche destruction
      Deep_delete (f);
      f := aux;
    end left_replaces_f;

    procedure right_replaces_f is
    begin
      aux := f.right;
      f.right := null; --  empˆeche destruction
      Deep_delete (f);
      f := aux;
    end right_replaces_f;

    procedure cst_replaces_f (cst : Real) is
    begin
      Deep_delete (f);
      f := new Formula_Rec (nb);
      f.n := cst;
    end cst_replaces_f;

    use REF;

    procedure Simplify_functions is
      x, y : Real;
      --  Assumes: f.s in Built_in_function
    begin
      if f.left = null then
        return;
      end if;
      if f.s in Binary then
        if f.right = null then
          return;
        end if;
        if f.right.s = nb then
          y := f.right.n;
        end if;
      end if;
      --
      --  Arguments are constants, we can evaluate the function and put the result as constant.
      --  Actually we could use the "Evaluate" function (for operators as well), but we then would
      --  need a fake payload. We also choose not to simplify cases where evaluation would fail.
      --
      if f.left.s = nb then        --  Evaluate "f(cst)" into f(cst)
        x := f.left.n;
        case Built_in_function (f.s) is
          when abso =>
            cst_replaces_f (abs x);
          when sign =>
            cst_replaces_f (Sign (x));
          when step =>
            cst_replaces_f (Step (x));
          when round =>
            cst_replaces_f (Real'Rounding (x));
          when trunc =>
            cst_replaces_f (Real'Truncation (x));
          when floor =>
            cst_replaces_f (Real'Floor (x));
          when ceiling =>
            cst_replaces_f (Real'Ceiling (x));
          when sinus =>
            cst_replaces_f (Sin (x));
          when arcsin =>
            cst_replaces_f (Arcsin (x));
          when cosinus =>
            cst_replaces_f (Cos (x));
          when arccos =>
            cst_replaces_f (Arccos (x));
          when expn =>
            cst_replaces_f (Exp (x));
          when logn =>
            if x > 0.0 then
              cst_replaces_f (Log (x));
            end if;
          when sqrt =>
            cst_replaces_f (Sqrt (x));
          when tg =>
            cst_replaces_f (Tan (x));
          when arctg =>
            cst_replaces_f (Arctan (x));
          when sh =>
            cst_replaces_f (Sinh (x));
          when arcsinh =>
            cst_replaces_f (Arcsinh (x));
          when ch =>
            cst_replaces_f (Cosh (x));
          when arccosh =>
            cst_replaces_f (Arccosh (x));
          when th =>
            cst_replaces_f (Tanh (x));
          when arctanh =>
            cst_replaces_f (Arctanh (x));
          when min =>
            if f.right.s = nb then
              cst_replaces_f (Real'Min (x, y));
            end if;
          when max =>
            if f.right.s = nb then
              cst_replaces_f (Real'Max (x, y));
            end if;
        end case;
      end if;
      if f.s = cosinus and then f.left /= null and then f.left.s = moins_una then
        aux := f.left.left;                              --  Cos(-X)  ->  Cos(X)
        Dispose (f.left);
        f.left := aux;
      end if;
    end Simplify_functions;

  begin
    if f = null then
      return;
    end if;
    --  Simplify arguments first, before the formula itself
    case f.s is
      when Unary =>
        Simplify (f.left);
      when Binary =>
        Simplify (f.left);
        Simplify (f.right);
      when others =>
        null;
    end case;
    if f = null or else
      (f.s in Unary and then f.left = null) or else
      (f.s in Binary and then (f.left = null or else f.right = null))
    then
      return;
    end if;

    case f.s is

      when moins_una =>
        if f.left.s = moins_una then
          aux := f.left.left;                           --  --X  ->  X
          Dispose (f.left);
          Dispose (f);
          f := aux;
        elsif f.left.s = nb then
          cst_replaces_f (-f.left.n);                   --  -cst  ->  cst
        end if;

      when plus_una =>
        left_replaces_f;                                --  +X  ->  X

      when par | croch | accol =>
        if par_or_terminal (f.left.s) then
          left_replaces_f;     --  ((...)) -> (...), (c) -> c, (v) -> v
        end if;

      when plus =>
        if f.right.s = moins_una then
          aux := new Formula_Rec (moins);               --  X + -Y  ->  X - Y
          aux.left := f.left;
          aux.right := f.right.left;
          Dispose (f.right);
          Dispose (f);
          f := aux;
        elsif Equivalent (f.left, f.right) then
          aux := Build_2X (f.left);                     --  X + X  ->  2*X
          Deep_delete (f.right);
          Dispose (f);
          f := aux;
        elsif f.right.s = plus and then Equivalent (f.left, f.right.left) then
          f.left := Build_2X (f.left);                  --  X + {X + Y}  ->  2*X + Y
          Deep_delete (f.right.left);   -- destroy 2nd occurence of X
          aux := f.right.right;         -- keep Y
          Dispose (f.right);
          f.right := aux;
        elsif f.right.s = plus and then Equivalent (f.left, f.right.right) then
          f.left := Build_2X (f.left);                  --  X + {Y + X}  ->  2*X + Y
          Deep_delete (f.right.right);  -- destroy 2nd occurence of X
          aux := f.right.left;          -- keep Y
          Dispose (f.right);
          f.right := aux;
        elsif f.left.s = nb and then f.right.s = plus and then f.right.left.s = nb then
          aux := f.right.right;                         --  cst + {cst + X} -> cst + X
          f.left.n := f.left.n + f.right.left.n;
          Dispose (f.right);
          f.right := aux;
        elsif f.right.s = nb and then f.right.n < 0.0 then
          aux := new Formula_Rec (moins);               --  X + neg_cst  ->  X - {abs neg_cst}
          aux.left := f.left;
          aux.right := f.right;
          aux.right.n := abs aux.right.n;
          Dispose (f);
          f := aux;
        elsif Is_constant_pair (f) then
          cst_replaces_f (f.left.n + f.right.n);      --  cst + cst  ->  cst
        elsif Is_constant (f.left, 0.0) then
          right_replaces_f;                             --  0 + X  ->  X
        elsif Is_constant (f.right, 0.0) then
          left_replaces_f;                              --  X + 0  ->  X
        end if;

      when moins =>
        if f.right.s = moins_una then
          aux := new Formula_Rec (plus);                --  X - -Y  ->  X + Y
          aux.left := f.left;
          aux.right := f.right.left;
          Dispose (f.right);
          Dispose (f);
          f := aux;
        elsif Equivalent (f.left, f.right) then
          cst_replaces_f (0.0);                         --  X - X   ->    0
        elsif f.right.s = nb and then f.right.n < 0.0 then
          aux := new Formula_Rec (plus);                --  X - neg_cst  ->  X + {abs neg_cst}
          aux.left := f.left;
          aux.right := f.right;
          aux.right.n := abs aux.right.n;
          Dispose (f);
          f := aux;
        elsif Is_constant_pair (f) then
          cst_replaces_f (f.left.n - f.right.n);      --  cst - cst  ->  cst
        elsif Is_constant (f.left, 0.0) then
          aux := new Formula_Rec (moins_una);           --  0 - X   ->   -X
          aux.left := f.right;
          Deep_delete (f.left);
          Dispose (f);
          f := aux;
        elsif Is_constant (f.right, 0.0) then
          left_replaces_f;                              --  X - 0   ->   X
        end if;

      when fois =>
        if Equivalent (f.left, f.right) then            --  X*X -> X^2
          aux := Build_X_pow_2 (f.left);
          Deep_delete (f.right);        -- destroy 2nd occurence of X
          Dispose (f);
          f := aux;
        elsif f.right.s = fois and then Equivalent (f.left, f.right.left) then
          f.left := Build_X_pow_2 (f.left);              --  X * {X * Y}  ->  X^2 * Y
          Deep_delete (f.right.left);   -- destroy 2nd occurence of X
          aux := f.right.right;         -- keep Y
          Dispose (f.right);
          f.right := aux;
        elsif f.right.s = fois and then Equivalent (f.left, f.right.right) then
          f.left := Build_X_pow_2 (f.left);              --  X * {Y * X}  ->  X^2 * Y
          Deep_delete (f.right.right);  -- destroy 2nd occurence of X
          aux := f.right.left;          -- keep Y
          Dispose (f.right);
          f.right := aux;
        elsif Is_constant_pair (f) then
          cst_replaces_f (f.left.n * f.right.n);       --  cst*cst  ->  cst
        elsif f.left.s  = puiss and then
              f.right.s = puiss and then
          Equivalent (f.left.left, f.right.left)
        then
          aux := new Formula_Rec (par);                  --  X^m * X^n   ->   X^(m+n)
          aux.left := new Formula_Rec (plus);
          aux.left.left := f.left.right;
          aux.left.right := f.right.right;        --  aux= "(m+n)"
          nexp := new Formula_Rec (puiss);
          nexp.left := f.left.left;
          nexp.right := aux;                 --  nexp= "X^(m+n)"
          Deep_delete (f.right.left);
          Dispose (f.left);
          Dispose (f.right);
          Dispose (f);                --  dissoudre ancienne expr
          f := nexp;
        elsif f.right.s = puiss and then Equivalent (f.left, f.right.left) then
          aux := f.right;                                --  X * X^n   ->   X^(n+1)
          Deep_delete (f.left);
          Dispose (f);
          f := aux;            --  got rid of *
          aux := new Formula_Rec (par);
          aux.left := new Formula_Rec (plus);
          aux.left.left := f.right;
          aux.left.right := new Formula_Rec (nb);
          aux.left.right.n := 1.0;     --  (n+1) prepared
          f.right := aux;
        elsif f.left.s = puiss and then Equivalent (f.left.left, f.right) then
          aux := f.left;                                --  X^n * X   ->   X^(n+1)
          Deep_delete (f.right);
          Dispose (f);
          f := aux;            --  got rid of *
          aux := new Formula_Rec (par);
          aux.left := new Formula_Rec (plus);
          aux.left.left := f.right;
          aux.left.right := new Formula_Rec (nb);
          aux.left.right.n := 1.0;     --  (n+1) prepared
          f.right := aux;
        elsif Is_constant (f.left, 0.0) or else Is_constant (f.right, 0.0) then
          cst_replaces_f (0.0);                         --  0*X or X*0  ->  0
        elsif Is_constant (f.left, 1.0) then
          right_replaces_f;                             --  1*X  ->  X
        elsif Is_constant (f.right, 1.0) then
          left_replaces_f;                              --  X*1  ->  X
        end if;

      when sur =>
        if Is_constant (f.right, 1.0) then
          left_replaces_f;                              --  X/1  ->  X
        elsif Equivalent (f.left, f.right) then
          cst_replaces_f (1.0);                         --  X/X  ->  1
        elsif Is_constant_pair (f) and then not Almost_zero (f.right.n) then
          cst_replaces_f (f.left.n / f.right.n);      --  cst/cst -> cst
        end if;

      when puiss =>
        if Is_constant (f.right, 0.0) then
          cst_replaces_f (1.0);                         --  X^0  ->  1
        elsif Is_constant (f.right, 1.0) then
          left_replaces_f;                              --  X^1  ->  X
        elsif f.left.s = nb and f.right.s = nb then     --  cst^cst  ->  cst
          cst_replaces_f (Exp (Log (f.left.n) * f.right.n));
        end if;

      when Built_in_function =>
        Simplify_functions;

      when pi =>
        cst_replaces_f (Ada.Numerics.Pi);

      when nb | var =>
        null;
    end case;
  end Simplify;

  procedure Simplify (f : in out Formula) is
  begin
    Simplify (f.root);
  end Simplify;

  procedure Enumerate_custom (f : Formula; lp : List_proc) is
    procedure EC (f : p_Formula_Rec) is
    begin
      if f /= null then
        case f.s is
          when var =>
            lp (To_String (f.v), 0);
          when Unary =>
            EC (f.left);
          when Binary =>
            EC (f.left);
            EC (f.right);
          when others =>
            null;
        end case;
      end if;
    end EC;
    --
  begin
    if lp /= null then
      EC (f.root);
    end if;
  end Enumerate_custom;

  overriding procedure Adjust (f : in out Formula) is
  begin
    f.root := Deep_copy (f.root);
  end Adjust;

  overriding procedure Finalize (f : in out Formula) is
  begin
    Deep_delete (f.root);
  end Finalize;

end AoC_2020_18_Weird_Formulas;
