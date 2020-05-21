with Ada.Strings.Fixed;

package body HAC.Defs is

  function "+" (a, b : Symset) return Symset is
    c : Symset;
  begin
    for i in a'Range loop
      c (i) := a (i) or b (i);
    end loop;
    return c;
  end "+";

  function "+" (a : Symset; b : KeyWSymbol) return Symset is
    c : Symset := a;
  begin
    c (b) := True;
    return c;
  end "+";

  function "-" (a, b : Symset) return Symset is
    c : Symset;
  begin
    for i in a'Range loop
      c (i) := a (i) and not b (i);
    end loop;
    return c;
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

  function To_String (a: Alfa) return String is
  begin
    return Trim (a, Right);
  end To_String;

  function To_Alfa (s: String) return Alfa is
  begin
    if s'Length > Alfa'Length then
      raise Constraint_Error;
    else
      return s & (Alfa'Length - s'Length) * ' ';
    end if;
  end To_Alfa;

  function HAC_Image (F : HAC_Float) return String is
  begin
    return HAC_Pack.HAC_Image (HAC_Pack.Real (F));
  end HAC_Image;

end HAC.Defs;
