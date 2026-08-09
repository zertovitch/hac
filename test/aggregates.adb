--  Test aggregates (RM 4.3): array and record aggregates used as object
--  declaration initializers and in plain assignment statements.

with HAT;
with Testing_Utilities;

procedure Aggregates is
  use HAT, Testing_Utilities;

  procedure Positional_Array is
    type Arr is array (1 .. 3) of Integer;
    a : Arr := (1, 2, 3);
  begin
    Assert (a (1) = 1 and a (2) = 2 and a (3) = 3, +"Compiler bug [Positional_Array]");
  end Positional_Array;

  procedure Named_Array_With_Others is
    type Arr is array (1 .. 5) of Integer;
    a : Arr := (1 => 10, 3 => 30, others => 0);
  begin
    Assert (
      a (1) = 10 and a (2) = 0 and a (3) = 30 and a (4) = 0 and a (5) = 0,
      +"Compiler bug [Named_Array_With_Others]"
    );
  end Named_Array_With_Others;

  procedure Multidimensional_Array_With_Others is
    --  A true multi-dimensional array (RM 3.6.1 comma syntax, one array
    --  type with 2 dimensions, not an array-of-arrays) whose "others" fill
    --  value is itself a nested aggregate for the inner dimension, not a
    --  scalar literal -- exercises the materialize-into-temp-then-copy
    --  path for array "others", not just the static-literal fast path.
    type Vector_2 is array (1 .. 3, 1 .. 2) of Integer;
    All_Others  : Vector_2 := (others => (others => 2));
    Row_1_Then_Others : Vector_2 := (1 => (9, 9), others => (others => 0));
  begin
    Assert (
      All_Others (1, 1) = 2 and All_Others (1, 2) = 2 and
      All_Others (2, 1) = 2 and All_Others (2, 2) = 2 and
      All_Others (3, 1) = 2 and All_Others (3, 2) = 2,
      +"Compiler bug [Multidimensional_Array_With_Others/All_Others]"
    );
    Assert (
      Row_1_Then_Others (1, 1) = 9 and Row_1_Then_Others (1, 2) = 9 and
      Row_1_Then_Others (2, 1) = 0 and Row_1_Then_Others (2, 2) = 0 and
      Row_1_Then_Others (3, 1) = 0 and Row_1_Then_Others (3, 2) = 0,
      +"Compiler bug [Multidimensional_Array_With_Others/Row_1_Then_Others]"
    );
  end Multidimensional_Array_With_Others;

  procedure Positional_Record is
    type Rec is record
      i : Integer;
      r : Real;
      b : Boolean;
    end record;
    x : Rec := (1, 2.0, True);
  begin
    Assert (x.i = 1 and x.r = 2.0 and x.b, +"Compiler bug [Positional_Record]");
  end Positional_Record;

  procedure Named_Record is
    type Rec is record
      i : Integer;
      r : Real;
    end record;
    x : Rec := (r => 3.0, i => 7);  --  Named, out of declaration order.
  begin
    Assert (x.i = 7 and x.r = 3.0, +"Compiler bug [Named_Record]");
  end Named_Record;

  procedure Variable_And_Function_Values is
    --  Aggregate component values need not be literals: variable/constant
    --  references and function calls must work in every position -- array
    --  positional and named-choice values, record named-field values, and
    --  record positional values (both the first component and later ones,
    --  since the first component's disambiguation logic is separate code
    --  from the later-component path).
    function Double (N : Integer) return Integer is
    begin
      return N * 2;
    end Double;

    X : constant Integer := 5;
    Y : constant Integer := 10;

    type Arr is array (1 .. 3) of Integer;
    type Rec is record
      A, B, C : Integer;
    end record;

    Arr_Positional : constant Arr := (X, Double (X), Y);
    Arr_Named      : constant Arr := (1 => X, 2 => Double (X), 3 => Y);
    Rec_Named      : constant Rec := (A => X, B => Double (X), C => Y);
    Rec_Pos_First_Var  : constant Rec := (X, 2, 3);
    Rec_Pos_First_Func : constant Rec := (Double (X), 2, 3);
    Rec_Pos_Later_Var  : constant Rec := (1, X, 3);
    Rec_Pos_Later_Func : constant Rec := (1, Double (X), 3);
  begin
    Assert (
      Arr_Positional (1) = 5 and Arr_Positional (2) = 10 and Arr_Positional (3) = 10,
      +"Compiler bug [Variable_And_Function_Values/Arr_Positional]"
    );
    Assert (
      Arr_Named (1) = 5 and Arr_Named (2) = 10 and Arr_Named (3) = 10,
      +"Compiler bug [Variable_And_Function_Values/Arr_Named]"
    );
    Assert (
      Rec_Named.A = 5 and Rec_Named.B = 10 and Rec_Named.C = 10,
      +"Compiler bug [Variable_And_Function_Values/Rec_Named]"
    );
    Assert (
      Rec_Pos_First_Var.A = 5 and Rec_Pos_First_Var.B = 2 and Rec_Pos_First_Var.C = 3,
      +"Compiler bug [Variable_And_Function_Values/Rec_Pos_First_Var]"
    );
    Assert (
      Rec_Pos_First_Func.A = 10 and Rec_Pos_First_Func.B = 2 and Rec_Pos_First_Func.C = 3,
      +"Compiler bug [Variable_And_Function_Values/Rec_Pos_First_Func]"
    );
    Assert (
      Rec_Pos_Later_Var.A = 1 and Rec_Pos_Later_Var.B = 5 and Rec_Pos_Later_Var.C = 3,
      +"Compiler bug [Variable_And_Function_Values/Rec_Pos_Later_Var]"
    );
    Assert (
      Rec_Pos_Later_Func.A = 1 and Rec_Pos_Later_Func.B = 10 and Rec_Pos_Later_Func.C = 3,
      +"Compiler bug [Variable_And_Function_Values/Rec_Pos_Later_Func]"
    );
  end Variable_And_Function_Values;

  procedure Nested_Array_of_Arrays is
    type Inner is array (1 .. 2) of Integer;
    type Outer is array (1 .. 2) of Inner;
    a : Outer := ((1, 2), (3, 4));
  begin
    Assert (
      a (1) (1) = 1 and a (1) (2) = 2 and a (2) (1) = 3 and a (2) (2) = 4,
      +"Compiler bug [Nested_Array_of_Arrays]"
    );
  end Nested_Array_of_Arrays;

  procedure Record_with_Array_Field is
    type Arr is array (1 .. 2) of Integer;
    type Rec is record
      tag  : Integer;
      vals : Arr;
    end record;
    x : Rec := (tag => 1, vals => (10, 20));
  begin
    Assert (
      x.tag = 1 and x.vals (1) = 10 and x.vals (2) = 20,
      +"Compiler bug [Record_with_Array_Field]"
    );
  end Record_with_Array_Field;

  procedure Assignment_Form is
    --  Aggregates used in a plain assignment statement, not just a declaration.
    type Arr is array (1 .. 3) of Integer;
    a : Arr;
  begin
    a := (7, 8, 9);
    Assert (a (1) = 7 and a (2) = 8 and a (3) = 9, +"Compiler bug [Assignment_Form]");
  end Assignment_Form;

  procedure Selector_Qualified_Destination is
    --  Aggregate assigned to a field, reached via a selector (not a bare
    --  variable) -- exercises the temp-then-Copy_Block fallback path.
    type Arr is array (1 .. 2) of Integer;
    type Rec is record
      f : Arr;
    end record;
    r : Rec;
  begin
    r.f := (11, 22);
    Assert (r.f (1) = 11 and r.f (2) = 22, +"Compiler bug [Selector_Qualified_Destination]");
  end Selector_Qualified_Destination;

  procedure By_Reference_Parameter_Destination is
    --  Aggregate assigned to a bare "out"/"in out" composite parameter
    --  (passed by reference) -- exercises the temp-then-Copy_Block fallback
    --  path via a runtime-held reference, not a compile-time address.
    type Arr is array (1 .. 3) of Integer;
    type Rec is record f1, f2 : Integer; end record;

    procedure Make (Result : out Arr) is
    begin
      Result := (1, 2, 3);
    end Make;

    procedure Reset (R : in out Rec) is
    begin
      R := (10, 20);
    end Reset;

    a : Arr;
    r : Rec := (1, 2);
  begin
    Make (a);
    Reset (r);
    Assert (
      a (1) = 1 and a (2) = 2 and a (3) = 3 and r.f1 = 10 and r.f2 = 20,
      +"Compiler bug [By_Reference_Parameter_Destination]"
    );
  end By_Reference_Parameter_Destination;

  procedure Positional_Then_Others is
    --  The common "some explicit values, then others" idiom: positional
    --  components followed by a trailing "others" association.
    type Arr is array (1 .. 5) of Integer;
    a : Arr := (1, 2, others => 0);
  begin
    Assert (
      a (1) = 1 and a (2) = 2 and a (3) = 0 and a (4) = 0 and a (5) = 0,
      +"Compiler bug [Positional_Then_Others]"
    );
  end Positional_Then_Others;

  procedure Record_Others is
    --  "others" for records: allowed when every field it covers shares the
    --  same type (RM 4.3.1). Exercises the materialize-into-temp-then-fan-
    --  out-copy mechanism, including when the fill value is itself a
    --  nested aggregate, and nested "others" at multiple levels.
    type Point_2D is record
      X, Y : Integer;
    end record;
    type Box is record
      Upper, Lower : Point_2D;
    end record;

    Sole      : Point_2D := (others => 42);
    Mixed     : Box := (Upper => (1, 2), others => (3, 4));
    All_Same  : Box := (others => (4, 5));
    Doubly    : Box := (others => (others => 0));
    Positional_With_Inner_Others : Box := ((1, 2), (others => 4));
    Named_Reordered : Box := (Lower => (others => 7), Upper => (8, 9));
  begin
    Assert (Sole.X = 42 and Sole.Y = 42, +"Compiler bug [Record_Others/Sole]");
    Assert (
      Mixed.Upper.X = 1 and Mixed.Upper.Y = 2 and Mixed.Lower.X = 3 and Mixed.Lower.Y = 4,
      +"Compiler bug [Record_Others/Mixed]"
    );
    Assert (
      All_Same.Upper.X = 4 and All_Same.Upper.Y = 5 and
      All_Same.Lower.X = 4 and All_Same.Lower.Y = 5,
      +"Compiler bug [Record_Others/All_Same]"
    );
    Assert (
      Doubly.Upper.X = 0 and Doubly.Upper.Y = 0 and Doubly.Lower.X = 0 and Doubly.Lower.Y = 0,
      +"Compiler bug [Record_Others/Doubly]"
    );
    Assert (
      Positional_With_Inner_Others.Upper.X = 1 and Positional_With_Inner_Others.Upper.Y = 2 and
      Positional_With_Inner_Others.Lower.X = 4 and Positional_With_Inner_Others.Lower.Y = 4,
      +"Compiler bug [Record_Others/Positional_With_Inner_Others]"
    );
    Assert (
      Named_Reordered.Upper.X = 8 and Named_Reordered.Upper.Y = 9 and
      Named_Reordered.Lower.X = 7 and Named_Reordered.Lower.Y = 7,
      +"Compiler bug [Record_Others/Named_Reordered]"
    );
  end Record_Others;

  procedure Others_Evaluated_Once_Per_Position is
    --  RM 4.3.3 (5): the "others" expression must be evaluated once *per
    --  component it covers*, not once total. A true 2D array (array-of-
    --  arrays internally), fully filled via nested "others" at both
    --  dimensions, with a side-effecting fill function -- this is the
    --  exact shape that used to silently copy one evaluation's result to
    --  every position instead of calling the function again for each one.
    type Vector_2 is array (1 .. 2, 1 .. 2) of Integer;
    Counter : Integer := 0;
    function New_Number return Integer is
    begin
      Counter := Counter + 1;
      return Counter;
    end New_Number;
    V : Vector_2 := (others => (others => New_Number));
  begin
    Assert (
      V (1, 1) = 1 and V (1, 2) = 2 and V (2, 1) = 3 and V (2, 2) = 4,
      +"Compiler bug [Others_Evaluated_Once_Per_Position]"
    );
  end Others_Evaluated_Once_Per_Position;

  procedure Others_With_Mixed_Nested_Fields is
    --  A composite (record) fill value that mixes an ordinary named
    --  component with a side-effecting one, itself nested under an
    --  array's "others" -- exercises that *every* leaf of a captured
    --  fill value (not just a top-level scalar) is independently
    --  replayed once per position, including ordinary (non-"others")
    --  fields of a nested aggregate literal.
    type Pair is record
      A, B : Integer;
    end record;
    type Pair_Array is array (1 .. 2) of Pair;
    Counter : Integer := 100;
    function Next return Integer is
    begin
      Counter := Counter + 1;
      return Counter;
    end Next;
    PA : Pair_Array := (others => (A => 1, B => Next));
  begin
    Assert (
      PA (1).A = 1 and PA (1).B = 101 and PA (2).A = 1 and PA (2).B = 102,
      +"Compiler bug [Others_With_Mixed_Nested_Fields]"
    );
  end Others_With_Mixed_Nested_Fields;

  procedure Plain_Parenthesized_Still_Works is
    --  Regression: "(X)" as a plain parenthesized expression (not an
    --  aggregate) must be unaffected by aggregate support.
    type Arr is array (1 .. 2) of Integer;
    src : Arr := (5, 6);
    dst : Arr := (src);
  begin
    Assert (dst (1) = 5 and dst (2) = 6, +"Compiler bug [Plain_Parenthesized_Still_Works]");
  end Plain_Parenthesized_Still_Works;

begin
  Positional_Array;
  Named_Array_With_Others;
  Multidimensional_Array_With_Others;
  Positional_Record;
  Named_Record;
  Variable_And_Function_Values;
  Nested_Array_of_Arrays;
  Record_with_Array_Field;
  Assignment_Form;
  Selector_Qualified_Destination;
  By_Reference_Parameter_Destination;
  Positional_Then_Others;
  Record_Others;
  Others_Evaluated_Once_Per_Position;
  Others_With_Mixed_Nested_Fields;
  Plain_Parenthesized_Still_Works;
  --
  --  Intentionally-invalid forms (left as comments per project convention,
  --  see test/exception_01.adb, test/optim.adb):
  --
  --  declare
  --    type Rec is record i, j : Integer; end record;
  --    x : Rec := (1, 2, 3);  --  Compile-time error: too many components.
  --  begin null; end;
  --
  --  declare
  --    type Arr is array (1 .. 3) of Integer;
  --    a : Arr := (1, 2);  --  Compile-time error: err_aggregate_index_not_covered.
  --  begin null; end;
  --
  --  declare
  --    type Arr is array (1 .. 3) of Integer;
  --    a : Arr := (1 => 1, 1 => 2, others => 0);  --  Compile-time error:
  --                                                --  err_aggregate_index_covered_twice.
  --  begin null; end;
  --
  --  declare
  --    type Rec is record i, j : Integer; end record;
  --    x : Rec := (i => 1);  --  Compile-time error: err_aggregate_field_not_covered ("j").
  --  begin null; end;
  --
  --  declare
  --    type Arr is array (1 .. 3) of Integer;
  --    a : Arr := (others => 0, 1 => 5);  --  Compile-time error:
  --                                       --  err_aggregate_others_not_last.
  --  begin null; end;
  --
  --  declare
  --    type Rec is record i, j : Integer; end record;
  --    x : Rec := (1, j => 2);  --  Compile-time error:
  --                            --  err_aggregate_positional_after_named ("j"
  --                            --  cannot be named after "1" was positional).
  --  begin null; end;
  --
  --  declare
  --    type Rec is record I : Integer; B : Boolean; end record;
  --    x : Rec := (others => 5);  --  Compile-time error:
  --                              --  err_aggregate_others_field_types_differ
  --                              --  (I and B do not have the same type).
  --  begin null; end;
end Aggregates;
