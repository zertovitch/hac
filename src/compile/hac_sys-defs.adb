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

end HAC_Sys.Defs;
