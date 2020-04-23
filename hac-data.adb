with Ada.Strings.Fixed;

package body HAC.Data is

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

end HAC.Data;
