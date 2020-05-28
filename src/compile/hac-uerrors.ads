-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

with HAC.Co_Defs, HAC.Defs;

package HAC.UErrors is

  use Co_Defs, Defs;

  has_new_line : constant array (Repair_kind) of Boolean := (insert_line => True, others => False);

  nothing_to_repair : constant Repair_kit := (none, Null_VString);

  procedure Error (
    CD   : in out Compiler_Data;
    code :        Compile_Error;
    hint :        String      := "";
    stop :        Boolean     := False
  );

  procedure Compilation_Errors_Summary (CD : Compiler_Data);

  type Table_OverFlow_Error is
    (IDENTIFIERS,
     PROCEDURES,
     FLOAT_CONSTANTS,
     ARRAYS,
     LEVELS,
     OBJECTS,
     Case_Labels,
     STRING_CONSTANTS,
     TASKS,
     ENTRIES,
     PATCHING);

  procedure Fatal (N: Table_OverFlow_Error; Current_Error_Output : Boolean := False);

  Internal_error: exception;
  Failure_1_0: exception;
  Compilation_abandoned: exception;

  function Error_String (code: HAC.Defs.Compile_Error; hint: String:= "") return String;

end HAC.UErrors;
