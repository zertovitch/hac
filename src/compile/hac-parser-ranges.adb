package body HAC.Parser.Ranges is

  ------------------
  -- Static_Range --
  ------------------

  procedure Static_Range (
    CD         : in out Compiler_Data;
    Level      :        PCode.Nesting_level;
    FSys       :        Defs.Symset;
    Low_Bound  :    out Defs.HAC_Integer;
    High_Bound :    out Defs.HAC_Integer
  )
  is
    --  The variant "Low .. High" was initially
    --  in HAC <= 0.07 for array bounds.
  begin
    pragma Compile_Time_Warning
       (Standard.True, "Static_Range unimplemented");
    raise Program_Error with "Unimplemented procedure Static_Range";
  end Static_Range;

  -------------------
  -- Dynamic_Range --
  -------------------

  procedure Dynamic_Range (
    CD         : in out Compiler_Data;
    Level      :        PCode.Nesting_level;
    FSys       :        Defs.Symset
  )
  is
    --  The variant "Low_Expr .. High_Expr" was initially
    --  in HAC <= 0.07 for FOR statements.
  begin
    pragma Compile_Time_Warning
       (Standard.True, "Dynamic_Range unimplemented");
    raise Program_Error with "Unimplemented procedure Dynamic_Range";
  end Dynamic_Range;

end HAC.Parser.Ranges;
