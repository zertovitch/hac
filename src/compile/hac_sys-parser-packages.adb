package body HAC_Sys.Parser.Packages is

   -------------------------
   -- Package_Declaration --
   -------------------------

   procedure Package_Declaration
     (CD : in out Co_Defs.Compiler_Data; FSys : Defs.Symset)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Package_Declaration unimplemented");
      raise Program_Error with "Unimplemented procedure Package_Declaration";
   end Package_Declaration;

   ------------------
   -- Package_Body --
   ------------------

   procedure Package_Body
     (CD : in out Co_Defs.Compiler_Data; FSys : Defs.Symset)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Package_Body unimplemented");
      raise Program_Error with "Unimplemented procedure Package_Body";
   end Package_Body;

end HAC_Sys.Parser.Packages;
