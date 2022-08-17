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

  has_new_line : constant array (Defs.Repair_Kind_Type) of Boolean :=
    (Defs.insert_line => True, others => False);

  nothing_to_repair : constant Defs.Repair_Kit := (Defs.none, HAT.Null_VString);

  type Error_Severity is (
    minor,   --  Extra ';', ')', value out of range, etc.: we can continue the
             --     compilation normally for catching other eventual errors.
    medium,  --  Compilation is shortened at some points in order to avoid
             --     infinite loops in the parser. Tricky!
    major    --  In this case, the best choice is to STOP the compilation immediately.
  );

  procedure Error (
    CD              : in out Co_Defs.Compiler_Data;
    code            :        Defs.Compile_Error;
    hint            :        String         := "";
    severity        :        Error_Severity := medium;
    previous_symbol :        Boolean        := False
  );

  procedure Compilation_Errors_Summary (CD : Co_Defs.Compiler_Data);

  type Table_OverFlow_Error is
    (IDENTIFIERS,
     PROCEDURES,
     FLOAT_CONSTANTS,
     ARRAYS,
     LEVELS,
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

  function Error_String (code : Defs.Compile_Error; hint : String := "") return String;

end HAC_Sys.Errors;
