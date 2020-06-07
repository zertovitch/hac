with HAC.Parser.Type_Def,
     HAC.Parser.Expressions,
     HAC.Parser.Helpers,
     HAC.Scanner,
     HAC.UErrors;

package body HAC.Parser.Ranges is

  ------------------
  -- Static_Range --
  ------------------

  procedure Static_Range (
    CD             : in out Compiler_Data;
    Level          : in     PCode.Nesting_level;
    FSys           : in     Defs.Symset;
    Specific_Error : in     Defs.Compile_Error;
    Lower_Bound    :    out Constant_Rec;
    Higher_Bound   :    out Constant_Rec
  )
  is
    --  The variant "Low .. High" was initially
    --  in HAC <= 0.07 for array bounds.
    use Defs, Helpers, Type_Def, UErrors;
  begin
    Number_Declaration_or_Enum_Item (CD, Level, OF_RANGE_Double_Dot_RParent + FSys, Lower_Bound);
    --
    if Lower_Bound.TP.TYP = Floats then
      Error (CD, Specific_Error, "a float type is not expected here");
      Lower_Bound.TP := (Ints, 0);
      Lower_Bound.I  := 0;
    end if;
    Need (CD, Range_Double_Dot_Symbol, err_expecting_double_dot);
    --
    Number_Declaration_or_Enum_Item (CD, Level, Comma_OF_RParent + FSys, Higher_Bound);
    --
    if Higher_Bound.TP /= Lower_Bound.TP then
      Error (CD, Specific_Error, "types in range bounds do not match");
      Higher_Bound.I := Lower_Bound.I;
    end if;
  end Static_Range;

  -------------------
  -- Dynamic_Range --
  -------------------

  procedure Dynamic_Range (
    CD                 : in out Compiler_Data;
    Level              : in     PCode.Nesting_level;
    FSys               : in     Defs.Symset;
    Non_Discrete_Error : in     Defs.Compile_Error
  )
  is
    --  The variant "Low_Expr .. High_Expr" was initially
    --  in HAC <= 0.07 for FOR statements.
    FOR_Lower_Bound,
    FOR_Upper_Bound : Exact_Typ;
    use Defs, Expressions, Helpers, Scanner, UErrors;
  begin
    Expression (CD, Level, END_LOOP_RANGE_Double_Dot + FSys, FOR_Lower_Bound);
    CD.IdTab (CD.Id_Count).xTyp := FOR_Lower_Bound;
    if not Discrete_Typ (FOR_Lower_Bound.TYP) then
      Error (CD, Non_Discrete_Error);
    end if;
    if CD.Sy = Range_Double_Dot_Symbol then  --  ".."
      InSymbol (CD);
      Expression (CD, Level, FSys + LOOP_Symbol, FOR_Upper_Bound);
      if FOR_Upper_Bound /= FOR_Lower_Bound then
        Type_Mismatch (
          CD, err_first_and_last_must_have_matching_types,
          Found    => FOR_Upper_Bound,
          Expected => FOR_Lower_Bound
        );
      end if;
    else
      Skip (CD, END_LOOP_Semicolon + FSys, err_expecting_double_dot);
    end if;
  end Dynamic_Range;

end HAC.Parser.Ranges;
