-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs;

with HAT;

package HAC_Sys.Errors is

  nothing_to_repair : constant Defs.Repair_Kit := (Defs.none, HAT.Null_VString);

  type Error_Severity is
    (minor,   --  Extra ';', ')', value out of range, etc.: we can continue the
              --     compilation normally for catching other possible errors.
     medium,  --  Compilation is shortened at some points in order to avoid
              --     infinite loops in the parser. Tricky!
     major);  --  In this case, the best choice is to STOP the compilation immediately.

  type Symbol_Location_Method is
    (current_symbol, previous_symbol, explicit);

  procedure Error
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Diagnostic;
     hint_1              :        String           := "";
     hint_2              :        String           := "";
     severity            :        Error_Severity   := medium;
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location  := (0, 0, 0));

  procedure Remark
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Remark;
     hint_1              :        String                 := "";
     hint_2              :        String                 := "";
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location        := (0, 0, 0));

  procedure Compilation_Diagnostics_Summary (CD : Co_Defs.Compiler_Data);

  type Table_OverFlow_Error is
    (IDENTIFIERS,
     PROCEDURES,
     FLOAT_CONSTANTS,
     ARRAYS,
     LEVELS,
     Loop_Nesting_Levels,
     Object_Code,
     Case_Labels,
     STRING_CONSTANTS,
     TASKS,
     ENTRIES,
     PATCHING);

  procedure Fatal (N : Table_OverFlow_Error; Current_Error_Output : Boolean := False);

  Internal_error : exception;
  Failure_1_0 : exception;
  Compilation_abandoned : exception;
  Compilation_of_package_body_before_spec : exception;

end HAC_Sys.Errors;
