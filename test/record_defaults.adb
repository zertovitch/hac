--  Test record component default values (RM 3.7 / 3.8 default_expression),
--  restricted to static values, and their automatic application to any
--  object declared without an explicit initializer (recursively, through
--  nested records and arrays of records).

with HAT;
with Testing_Utilities;

procedure Record_Defaults is
  use HAT, Testing_Utilities;

  procedure Scalar_Field_Defaults is
    type Point_2D is record
      X : Integer := 0;
      Y : Integer := 0;
    end record;
    P : Point_2D;  --  No initializer -- becomes (0, 0).
  begin
    Assert (P.X = 0 and P.Y = 0, +"Compiler bug [Scalar_Field_Defaults]");
  end Scalar_Field_Defaults;

  procedure Composite_Field_Default_And_Inheritance is
    --  The user's own example: a field with its own aggregate default
    --  (Upper), and a field with no own default that falls back to its
    --  type's own defaults (Lower).
    type Point_2D is record
      X : Integer := 0;
      Y : Integer := 0;
    end record;
    type Box is record
      Upper : Point_2D := (1, 1);
      Lower : Point_2D;
    end record;
    B : Box;
  begin
    Assert (
      B.Upper.X = 1 and B.Upper.Y = 1 and B.Lower.X = 0 and B.Lower.Y = 0,
      +"Compiler bug [Composite_Field_Default_And_Inheritance]"
    );
  end Composite_Field_Default_And_Inheritance;

  procedure Array_Of_Record_Inheritance is
    --  A whole array-typed variable, with no initializer, whose element
    --  type is a record with defaults -- every element must independently
    --  get that record type's own default (RM 3.3.1: default
    --  initialization applies to every subcomponent at any depth,
    --  including through array indexing).
    type Point_2D is record
      X : Integer := 5;
      Y : Integer := 7;
    end record;
    type Point_Array is array (1 .. 3) of Point_2D;
    PA : Point_Array;
  begin
    Assert (
      PA (1).X = 5 and PA (1).Y = 7 and
      PA (2).X = 5 and PA (2).Y = 7 and
      PA (3).X = 5 and PA (3).Y = 7,
      +"Compiler bug [Array_Of_Record_Inheritance]"
    );
  end Array_Of_Record_Inheritance;

  procedure Partial_Defaults is
    --  Some fields defaulted, some not -- the undefaulted ones must remain
    --  ordinary (explicitly assignable/readable) variables, unaffected by
    --  the defaulting mechanism.
    type Point_2D is record
      X : Integer := 5;
      Y : Integer := 7;
    end record;
    type Mixed is record
      A : Integer;         --  No default: ordinary field.
      B : Point_2D := (1, 2);
      C : Point_2D;         --  Inherits (5, 7).
    end record;
    M : Mixed;
  begin
    M.A := 42;  --  Undefaulted field: must still work like any variable.
    Assert (
      M.A = 42 and M.B.X = 1 and M.B.Y = 2 and M.C.X = 5 and M.C.Y = 7,
      +"Compiler bug [Partial_Defaults]"
    );
  end Partial_Defaults;

  procedure No_Defaults_Still_Works is
    --  Regression: a record type with no defaults at all must compile and
    --  behave exactly as before this feature existed.
    type Point_2D is record
      X, Y : Integer;
    end record;
    P : Point_2D;
  begin
    P.X := 3;
    P.Y := 4;
    Assert (P.X = 3 and P.Y = 4, +"Compiler bug [No_Defaults_Still_Works]");
  end No_Defaults_Still_Works;

  procedure Explicit_Initializer_Overrides_Default is
    --  An explicit initializer on the object declaration itself must take
    --  priority over -- not be overwritten by -- the type's own defaults.
    type Point_2D is record
      X : Integer := 0;
      Y : Integer := 0;
    end record;
    P : Point_2D := (9, 9);
  begin
    Assert (P.X = 9 and P.Y = 9, +"Compiler bug [Explicit_Initializer_Overrides_Default]");
  end Explicit_Initializer_Overrides_Default;

  procedure Multiple_Names_Share_Default is
    --  "A, B : Point_2D;" (no initializer): both must independently get
    --  the same default, at their own distinct addresses.
    type Point_2D is record
      X : Integer := 1;
      Y : Integer := 2;
    end record;
    A, B : Point_2D;
  begin
    A.X := 100;  --  Must not affect B.
    Assert (
      A.X = 100 and A.Y = 2 and B.X = 1 and B.Y = 2,
      +"Compiler bug [Multiple_Names_Share_Default]"
    );
  end Multiple_Names_Share_Default;

begin
  Scalar_Field_Defaults;
  Composite_Field_Default_And_Inheritance;
  Array_Of_Record_Inheritance;
  Partial_Defaults;
  No_Defaults_Still_Works;
  Explicit_Initializer_Overrides_Default;
  Multiple_Names_Share_Default;
end Record_Defaults;
