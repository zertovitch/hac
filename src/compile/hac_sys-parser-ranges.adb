with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Ranges is

  --  Are we sitting on the identifier of a discrete subtype
  --  like "Color" in "Color [range red .. blue]" ?
  --  In that case, we return the range red .. blue.
  --
  procedure Static_Subtype_Indication (  --  RM 3.2.2
    CD        : in out Compiler_Data;
    Level     : in     PCode.Nesting_level;
    Low, High :    out Constant_Rec;
    Found     :    out Boolean
  )
  is
    Idx : Integer;
    use Defs, Helpers, Scanner;
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
        if Id_T.Obj = TypeMark and then Discrete_Typ (Id_T.xTyp.TYP) then
          Low.TP  := Id_T.xTyp;
          Low.I   := Id_T.Discrete_First;
          High.TP := Id_T.xTyp;
          High.I  := Id_T.Discrete_Last;
          Found   := True;
          InSymbol (CD);
        end if;
      end;
    end if;
  end Static_Subtype_Indication;

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
    --  in HAC.Parser <= 0.07 for array bounds.
    Is_SI_Found : Boolean;
    use Defs, Helpers, Type_Def, UErrors;
  begin
    Static_Subtype_Indication (CD, Level, Lower_Bound, Higher_Bound, Is_SI_Found);
    if Is_SI_Found then
      return;
      --  All right, we have parsed, e.g., "Boolean" and
      --  pass further "False .. True".
    end if;
    --
    --  We try an explicit static range, like: "1 .. N" (N declared number) or "red .. blue".
    --
    Number_Declaration_or_Enum_Item (CD, Level, OF_RANGE_Double_Dot_RParent + FSys, Lower_Bound);
    --
    if Lower_Bound.TP.TYP = Floats then
      Error (CD, Specific_Error, "a float type is not expected here");
      Lower_Bound.TP := (Ints, 0);
      Lower_Bound.I  := 0;
    end if;
    --
    Need (CD, Range_Double_Dot_Symbol, err_expecting_double_dot);  --  " .. "
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
    Non_Discrete_Error : in     Defs.Compile_Error;
    Range_Typ          :    out Exact_Typ
  )
  is
    --  The variant "Low_Expr .. High_Expr" was initially
    --  in HAC.Parser <= 0.07 for FOR statements.
    Lower_Bound : Exact_Typ;
    Upper_Bound : Exact_Typ;
    Lower_Bound_Static  : Constant_Rec;
    Higher_Bound_Static : Constant_Rec;
    Is_SI_Found : Boolean;
    use Compiler.PCode_Emit, Defs, Expressions, Helpers, PCode, Scanner, UErrors;
  begin
    Static_Subtype_Indication (CD, Level, Lower_Bound_Static, Higher_Bound_Static, Is_SI_Found);
    if Is_SI_Found then
      --  All right, we have parsed, e.g., "Boolean".
      --  Since we are in a dynamic context, we need to push
      --  the bounds on the stack (E.g., "False .. True").
      Emit_1 (CD, k_Push_Discrete_Literal, Lower_Bound_Static.I);
      Emit_1 (CD, k_Push_Discrete_Literal, Higher_Bound_Static.I);
      Range_Typ := Lower_Bound_Static.TP;
      return;
    end if;
    --
    --  We try an explicit dynamic range, like: "f (z) + j .. n * 2"  or  "1 .. 6".
    --
    Expression (CD, Level, END_LOOP_RANGE_Double_Dot + FSys, Lower_Bound);
    Range_Typ := Lower_Bound;
    if not Discrete_Typ (Lower_Bound.TYP) then
      Error (CD, Non_Discrete_Error);
    end if;
    if CD.Sy = Range_Double_Dot_Symbol then  --  ".."
      InSymbol (CD);
      Expression (CD, Level, FSys + LOOP_Symbol, Upper_Bound);
      if Upper_Bound /= Lower_Bound then
        Type_Mismatch (
          CD, err_first_and_last_must_have_matching_types,
          Found    => Upper_Bound,
          Expected => Lower_Bound
        );
      end if;
    else
      Skip (CD, END_LOOP_Semicolon + FSys, err_expecting_double_dot);
    end if;
  end Dynamic_Range;

end HAC_Sys.Parser.Ranges;
