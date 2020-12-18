--  Variant of the Formulas package (see the open-source MathPaqs project),
--  with "alternative" arithmetics...

with Ada.Finalization;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

generic

  type Real is digits <>;

  with function Plus (a, b : Real) return Real;
  with function Minus (a, b : Real) return Real;
  with function Times (a, b : Real) return Real;

  plus_char  : Character;
  minus_char : Character;
  times_char : Character;

package AoC_2020_18_Weird_Formulas is

  type Formula is tagged private;

  ---------------------------------------------------
  --  The parsing of the formula is done once      --
  --  The evaluation is quick and done many times  --
  ---------------------------------------------------

  procedure Parse (f : out Formula; s : String);
  function Parse (s : String) return Formula;  --  NB: a bit slower than the procedure
  procedure Parse (f : out Formula; u : Unbounded_String);
  function Parse (u : Unbounded_String) return Formula;  --  NB: a bit slower than the procedure

  function Evaluate (f : Formula) return Real;

  Parse_Error, Div_By_0 : exception;

  -----------------------------------
  --  Comparison / simplification  --
  -----------------------------------

  function Equivalent (fa, fb : Formula) return Boolean;
  --  Strict form of formula equivalence:
  function Identical (fa, fb : Formula) return Boolean;
  procedure Simplify (f : in out Formula);

  --  Deep_copy and Deep_delete are no more needed and
  --  were removed, it is done now automatically.

  ----------------------------------------------------------------
  --  Display. Caution: the displayed constants may be rounded  --
  ----------------------------------------------------------------

  type Output_style is (
    normal,     --  Normal is infix, should be the closest to the parsed formula
    bracketed   --  Like normal, but displays a {} around every parse tree node
  );

  procedure Put (f : Formula; style : Output_style := normal);
  procedure Put (t : in Ada.Text_IO.File_Type; f : Formula; style : Output_style := normal);
  function Image (f : Formula; style : Output_style := normal) return String;

  -----------------
  --  Utilities  --
  -----------------

  type List_proc is access procedure (name : String; parameters : Natural);

  procedure Enumerate_custom (f : Formula; lp : List_proc);
  --  This will find all custom variables or functions appearing
  --  in a formula and list it through List_proc.
  --  A name will be listed as many times as it appears in the formula.

  ---------------------------------------------------
  --  Character set used for defining identifiers  --
  ---------------------------------------------------

  type Character_Set is array (Character) of Boolean;

  --  Custom variables or functions always begin with a letter:
  letters  : constant Character_Set :=
    ('a' .. 'z' | 'A' .. 'Z' => True, others => False);

  --  Set of following characters in custom variables or functions names is defined here:
  following_character  : constant Character_Set :=
    ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '.' => True, others => False);

private

  type S_Form is
                (                                    --  0 argument (leaf, terminal nodes):
                 nb, pi, var,
                 --                                      1 argument:
                 moins_una, plus_una,
                 par, croch, accol,
                 --  vvv begin of built-in functions
                 abso, sign, step,
                 round, trunc, floor, ceiling,
                 expn, logn,
                 sqrt,
                 sinus, arcsin, cosinus, arccos, tg, arctg,
                 sh, arcsinh, ch, arccosh, th, arctanh,
                 --                                      2 arguments:
                 min, max,
                 --  ^^^ end of built-in functions
                 moins, plus, sur, fois, puiss);

  subtype Unary is S_Form range moins_una .. arctanh;
  subtype Binary is S_Form range min .. puiss;

  type Formula_Rec (s :  S_Form);
  type p_Formula_Rec is access Formula_Rec;

  type Formula_Rec (s :  S_Form) is record
    case s is
      when nb  =>  n : Real;
      when pi  =>  null;
      when var =>  v : Unbounded_String;
      when Unary | Binary => left, right : p_Formula_Rec;
    end case;
  end record;

  type Formula is new Ada.Finalization.Controlled with record
    root : p_Formula_Rec := null;
  end record;

  overriding procedure Adjust (f : in out Formula);
  overriding procedure Finalize (f : in out Formula);

end AoC_2020_18_Weird_Formulas;
