-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

with HAC.Compiler, HAC.Data;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package HAC.UErrors is

  type Message_kind is (error, warning, note, style);

  type Repair_kind is (none, insert, insert_line, replace_token);

  has_new_line : array (Repair_kind) of Boolean := (insert_line => True, others => False);

  type Repair_kit is tagged record
    kind : Repair_kind      := none;
    text : Unbounded_String := Null_Unbounded_String;
  end record;

  nothing_to_repair : constant Repair_kit := (none, Null_Unbounded_String);

  --  See current_error_pipe in HAC.Data for main pipe.

  type Smart_error_pipe is access procedure (
    message   : String;
    file_name : String;
    line      : Natural;
    column_a  : Natural;       --  Before first selected character, can be 0.
    column_z  : Natural;
    kind      : Message_kind;  --  Error, or warning, or ? ...
    repair    : Repair_kit     --  Can error be automatically repaired; if so, how ?
  );

  --  Error messages can be routed to a specialized pipe instead of
  --  text-based Standard_Error.  !!  Global variable alarm!

  current_error_pipe: HAC.UErrors.Smart_error_pipe := null;

  procedure Error (
    CD            : in out HAC.Compiler.Compiler_Data;
    code          :        HAC.Data.Compile_Error;
    hint          :        String      := "";
    stop_on_error :        Boolean     := False
  );

  procedure Compilation_Errors_Summary (CD : HAC.Compiler.Compiler_Data);

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

  function Error_String (code: HAC.Data.Compile_Error; hint: String:= "") return String;

end HAC.UErrors;
