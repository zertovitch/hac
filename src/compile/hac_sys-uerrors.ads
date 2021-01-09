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

with HAL;

package HAC_Sys.UErrors is

  has_new_line : constant array (Defs.Repair_kind) of Boolean :=
    (Defs.insert_line => True, others => False);

  nothing_to_repair : constant Defs.Repair_kit := (Defs.none, HAL.Null_VString);

  procedure Error (
    CD   : in out Co_Defs.Compiler_Data;
    code :        Defs.Compile_Error;
    hint :        String      := "";
    stop :        Boolean     := False
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

end HAC_Sys.UErrors;
