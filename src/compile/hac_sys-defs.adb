package body HAC_Sys.Defs is

  function "+" (a, b : Symset) return Symset is
  begin
    return a or b;
  end "+";

  function "+" (a : Symset; b : Symbol) return Symset is
    c : Symset := a;
  begin
    c (b) := True;
    return c;
  end "+";

  function "-" (a, b : Symset) return Symset is
  begin
    return a and not b;
  end "-";

  function "-" (a : Symset; b : Symbol) return Symset is
    c : Symset := a;
  begin
    c (b) := False;
    return c;
  end "-";

  function Minimum_Level (r : Compile_Remark) return Remark_Level is
  begin
    for lev in Remark_Level loop
      if preset_remarks (lev)(r) then return lev; end if;
    end loop;
    return Remark_Level'Last;
  end Minimum_Level;

end HAC_Sys.Defs;
