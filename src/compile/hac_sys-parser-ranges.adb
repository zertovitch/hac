with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Ranges is

  --  Are we sitting on the identifier of a discrete subtype
  --  like "Color" in "Color [range red .. blue]" ?
  --  In that case, we return the range red .. blue.
  --
  procedure Static_Subtype_Indication (  --  RM 3.2.2
    CD        : in out Co_Defs.Compiler_Data;
    Level     : in     Defs.Nesting_level;
    Low, High :    out Co_Defs.Constant_Rec;
    Found     :    out Boolean
  )
  is
    Idx : Integer;
    use Co_Defs, Defs, Helpers, Scanner;
  begin
    Found := False;
    if CD.Sy /= IDent then
      return;  --  Perhaps we have a number like "1" in "1 .. 3", or something wrong.
    end if;
    Idx := Locate_Identifier (CD, CD.Id, Level);
    if Idx /= No_Id then
      declare
        Id_T : IdTabEntry renames CD.IdTab (Idx);
      begin
        if Id_T.entity = TypeMark and then Discrete_Typ (Id_T.xtyp.TYP) then
          --  Subtype S, but need to exclude the attribute case: S'First, S'Image, ...
          Skip_Blanks (CD);
          if CD.CUD.c /= ''' then  --  We sneak a look at the next symbol.
            --  Not a S'... attribute here.
            --  We can use the subtype identifier as a range.
            Low.TP  := Id_T.xtyp;
            Low.I   := Id_T.xtyp.Discrete_First;
            --
            High.TP := Id_T.xtyp;
            High.I  := Id_T.xtyp.Discrete_Last;
            --
            Found   := True;
            InSymbol (CD);  --  Consume the identifier.
          end if;
        end if;
      end;
    end if;
  end Static_Subtype_Indication;

  ---------------------------
  -- Explicit_Static_Range --
  ---------------------------

  procedure Explicit_Static_Range (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Specific_Error : in     Defs.Compile_Error;
    Lower_Bound    :    out Co_Defs.Constant_Rec;
    Higher_Bound   :    out Co_Defs.Constant_Rec
  )
  is
    use Co_Defs, Defs, Expressions, Helpers, Errors;
  begin
    Static_Scalar_Expression (CD, Level, OF_RANGE_Double_Dot_RParent + FSys, Lower_Bound);
    --
    if Lower_Bound.TP.TYP = Floats then
      Error (CD, Specific_Error, "a float type is not expected here");
      Construct_Root (Lower_Bound.TP, Ints);
      Lower_Bound.I  := 0;
    end if;
    --
    Need (CD, Range_Double_Dot_Symbol, err_expecting_double_dot);  --  " .. "
    --
    Static_Scalar_Expression (CD, Level, Comma_OF_RParent + FSys, Higher_Bound);
    --
    if Higher_Bound.TP /= Lower_Bound.TP then
      Error (CD, Specific_Error, "types in range bounds do not match");
      Higher_Bound.I := Lower_Bound.I;
    end if;
  end Explicit_Static_Range;

  ------------------
  -- Static_Range --
  ------------------

  procedure Static_Range (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Specific_Error : in     Defs.Compile_Error;
    Lower_Bound    :    out Co_Defs.Constant_Rec;
    Higher_Bound   :    out Co_Defs.Constant_Rec
  )
  is
    --  The variant "Low .. High" was initially
    --  in HAC.Parser <= 0.07 for array bounds.
    Is_SI_Found : Boolean;
  begin
    Static_Subtype_Indication (CD, Level, Lower_Bound, Higher_Bound, Is_SI_Found);
    if Is_SI_Found then
      return;
      --  All right, we have parsed, e.g., "Boolean" and
      --  pass further "False .. True".
    end if;
    --
    --  We try an explicit static range, like: `1 .. N` (N declared number),
    --  `red .. blue` or `Base_Colour'First .. Colour'Last`.
    --
    Explicit_Static_Range (CD, Level, FSys, Specific_Error, Lower_Bound, Higher_Bound);
  end Static_Range;

  -------------------
  -- Dynamic_Range --
  -------------------

  procedure Dynamic_Range (
    CD                 : in out Co_Defs.Compiler_Data;
    Level              : in     Defs.Nesting_level;
    FSys               : in     Defs.Symset;
    Non_Discrete_Error : in     Defs.Compile_Error;
    Range_Typ          :    out Co_Defs.Exact_Subtyp
  )
  is
    use Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;
    --  The variant "Low_Expr .. High_Expr" was initially
    --  in HAC.Parser <= 0.07 for FOR statements.
    Lower_Bound_Typ, Upper_Bound_Typ        : Exact_Subtyp;
    Lower_Bound_Static, Higher_Bound_Static : Constant_Rec;
    Is_SI_Found                             : Boolean;
  begin
    Static_Subtype_Indication (CD, Level, Lower_Bound_Static, Higher_Bound_Static, Is_SI_Found);
    --
    if Is_SI_Found then
      --  All right, we have parsed a subtype indication, e.g., "Boolean".
      --  Since we are in a dynamic context, we need to push
      --  the bounds on the stack (E.g., "False .. True").
      Emit_1 (CD, k_Push_Discrete_Literal, Lower_Bound_Static.I);
      Emit_1 (CD, k_Push_Discrete_Literal, Higher_Bound_Static.I);
      Range_Typ := Lower_Bound_Static.TP;
      return;
    end if;
    --
    --  We try an explicit dynamic range, like: "f (z) + j .. n * 2"  or  "1 .. 6".
    --  See RM 3.5 (3).
    --
    Simple_Expression (CD, Level, END_LOOP_RANGE_Double_Dot + FSys, Lower_Bound_Typ);
    --  You may ask: why did the Ada standard authors take Simple_Expression
    --  instead of Expression for the range bounds ?
    --  It's for stopping the parsing on relational and logical operators.
    --  Consider the following example (in exm/aoc/2021/aoc_2021_11.adb ).
    --  With Expression for bounds, you need brackets for the membership tests:
    --
    --      if (xx in 1 .. sx) and then (yy in 1 .. sy) and then map (xx, yy) <= 9 then
    --
    --  With Simple_Expression, you can write, instead, the more intuitive:
    --
    --      if xx in 1 .. sx and then yy in 1 .. sy and then map (xx, yy) <= 9 then
    --
    Range_Typ          := Lower_Bound_Typ;
    Range_Typ.Is_Range := False;
    --
    if not Discrete_Typ (Range_Typ.TYP) then
      Error (CD, Non_Discrete_Error, Nice_Exact_Image (CD, Range_Typ));
    end if;
    --
    if Lower_Bound_Typ.Is_Range then
      --  We got a ` X'Range ` expression which is a shortcut for ` X'First .. X'Last `.
      --  The ` .. X'Last ` part has been implicitly parsed with ` X'Range ` .
      null;
    elsif CD.Sy = Range_Double_Dot_Symbol then  --  ".."
      InSymbol (CD);
      --
      Simple_Expression (CD, Level, FSys + LOOP_Symbol, Upper_Bound_Typ);
      --
      if Exact_Typ (Upper_Bound_Typ) /= Exact_Typ (Lower_Bound_Typ) then
        Type_Mismatch (
          CD, err_bounds_type_mismatch,
          Found    => Upper_Bound_Typ,
          Expected => Lower_Bound_Typ
        );
      end if;
      Range_Typ.Discrete_Last := Upper_Bound_Typ.Discrete_Last;
    else
      Skip (CD, END_LOOP_Semicolon + FSys, err_expecting_double_dot);
    end if;
  end Dynamic_Range;

  procedure Set_Singleton_Range (X : in out Co_Defs.Exact_Subtyp; Value : Defs.HAC_Integer) is
  begin
    X.Discrete_First := Value;
    X.Discrete_Last  := Value;
  end Set_Singleton_Range;

  function Is_Singleton_Range (X : Co_Defs.Exact_Subtyp) return Boolean is
    use type Defs.HAC_Integer;
  begin
    return X.Discrete_First = X.Discrete_Last;
  end Is_Singleton_Range;

  function Is_Singleton_Range (X : Co_Defs.Exact_Subtyp; Value : Defs.HAC_Integer) return Boolean is
    use type Defs.HAC_Integer;
  begin
    return Is_Singleton_Range (X) and then X.Discrete_First = Value;
  end Is_Singleton_Range;

  procedure Negate_Range
    (CD : in out Co_Defs.Compiler_Data;
     X  : in out Co_Defs.Exact_Subtyp)
  is
    use Defs;
    use type HAC_Integer;
    temp : HAC_Integer;
  begin
    pragma Assert (X.TYP = Ints);
    if X.Discrete_Last = HAC_Integer'First then
      --  *Upper* bound is -2**(bits-1) -> overflow guaranteed on negating.
      Errors.Error (CD, err_range_constraint_error, ": overflow on applying ""-""");
    end if;
    temp := X.Discrete_Last;
    if X.Discrete_First = HAC_Integer'First then  --  Compile-time overflow if we negate that!
      X.Discrete_Last := HAC_Integer'Last;  -- one off
    else
      X.Discrete_Last := -X.Discrete_First;
    end if;
    X.Discrete_First := -temp;
  end Negate_Range;

  function Do_Ranges_Overlap (X_min, X_max, Y_min, Y_max : Defs.HAC_Integer) return Boolean is
    use type Defs.HAC_Integer;
  begin
    pragma Assert (X_min <= X_max and Y_min <= Y_max);
    --
    --  The following is logically identical to: "not (Y_max < X_min or X_max < Y_min)",
    --  which means we don't have this situation:
    --       [X_min .. X_max] ... a gap ... [Y_min .. Y_max].
    --  or   [Y_min .. Y_max] ... a gap ... [X_min .. X_max].
    --
    return Y_max >= X_min and then
           X_max >= Y_min;
  end Do_Ranges_Overlap;

  function Do_Ranges_Overlap (X, Y : Co_Defs.Exact_Subtyp) return Boolean is
  begin
    return Do_Ranges_Overlap
             (X.Discrete_First, X.Discrete_Last,
              Y.Discrete_First, Y.Discrete_Last);
  end Do_Ranges_Overlap;

end HAC_Sys.Parser.Ranges;
