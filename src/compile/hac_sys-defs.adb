with Ada.Strings.Fixed;

package body HAC_Sys.Defs is

  function "+" (a, b : Symset) return Symset is
  begin
    return a or b;
  end "+";

  function "+" (a : Symset; b : KeyWSymbol) return Symset is
    c : Symset := a;
  begin
    c (b) := True;
    return c;
  end "+";

  function "-" (a, b : Symset) return Symset is
  begin
    return a and not b;
  end "-";

  function "-" (a : Symset; b : KeyWSymbol) return Symset is
    c : Symset := a;
  begin
    c (b) := False;
    return c;
  end "-";

  use Ada.Strings, Ada.Strings.Fixed;

  function Equal (a : Alfa; s : String) return Boolean is
  begin
    return To_String (a) = s;
  end Equal;

  function Initial (a : Alfa) return Character is
  begin
    return a (a'First);
  end Initial;

  function To_String (a : Alfa) return String is
  begin
    return Trim (a, Right);
  end To_String;

  function To_Alfa (s : String) return Alfa is
  begin
    if s'Length > Alfa'Length then
      raise Constraint_Error
        with "Alfa length capacity exceeded (" & s & ')';
    else
      return s & (Alfa'Length - s'Length) * ' ';
    end if;
  end To_Alfa;

  function HAC_Image (F : HAC_Float) return String is
  begin
    --  We want to be sure to have the same output from HAC's VM
    --  and from a program compiled on a "full Ada", using HAC_Pack.
    --  So we use HAC_Pack ourselves.
    return HAC_Pack.HAC_Image (HAC_Pack.Real (F));
  end HAC_Image;

  function HAC_Image (T : Ada.Calendar.Time) return String is
  begin
    --  We want to be sure to have the same output from HAC's VM
    --  and from a program compiled on a "full Ada", using HAC_Pack.
    --  So we use HAC_Pack ourselves.
    return HAC_Pack.HAC_Image (T);
  end HAC_Image;

end HAC_Sys.Defs;
