-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  The target Semantics produces no machine code but
--  serves semantics analysis purposes, like cross-references
--  for an editor with auto-complete, contextual menus and tool tips.

with Ada.Containers.Hashed_Maps,
     Ada.Strings.Unbounded.Hash;

with HAC_Sys.Co_Defs;

with HAT;

package HAC_Sys.Targets.Semantics is

  --  Reference map
  --  =============
  --
  --  Key                  Value
  --  ===                  =====
  --  [file_name i j]      index in the Id Table

  package Reference_Mapping is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,  --  [file_name i j]
     Element_Type    => Positive,     --  Index in the Id Table
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."=");

  --  Declarations map
  --  ================
  --
  --  Key                  Value
  --  ===                  =====
  --  index in Id Table    file_name, i, j of declaration

  type Declaration_Point_Array is
    array (Co_Defs.Identifier_Table_Type'Range) of Declaration_Point;

  --  Possible sanity checks for an usage within an editor:
  --    - compare identifiers between the one at [file_name i j] and
  --      the one in the identifier table (weak check)
  --    - ensure the analysis was completed after the latest
  --      modification in the editor (keystroke, cut, paste, ...)
  --

  type Machine is limited new Targets.Machine with record
    CD         : Co_Defs.Compiler_Data_Access;
    ref_map    : Reference_Mapping.Map;
    decl_map   : Declaration_Point_Array;
    busy       : Boolean  := False with Volatile;
    started    : HAT.Time;
    finished   : HAT.Time;
    total_time : Duration := 0.0;
  end record;

  --------------------
  --  Informations  --
  --------------------

  overriding function Name (m : Machine) return String is ("HAC Semantics");
  overriding function CPU (m : Machine) return String is ("No CPU");
  overriding function OS (m : Machine) return String is ("Any");
  overriding function Null_Terminated_String_Literals (m : Machine) return Boolean is (False);

  ---------------------------------------
  --  Initialize & Finalize Semantics  --
  ---------------------------------------

  overriding procedure Initialize_Code_Emission (m : in out Machine);
  overriding procedure Finalize_Code_Emission
    (m       : in out Machine;
     strings :        String);

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  overriding procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  :        Defs.Arithmetic_Binary_Operator;
     base_typ  :        Defs.Numeric_Typ) is null;

  overriding procedure Emit_Halt (m : in out Machine) is null;

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer) is null;

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer) is null;

  ----------------------------
  --  Built-In Subprograms  --
  ----------------------------

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer) is null;

  -------------
  --  Misc.  --
  -------------

  overriding function Assembler_File_Name (m : Machine) return String is ("");

  overriding procedure Mark_Declaration (m : in out Machine; is_built_in : Boolean);

  overriding procedure Mark_Reference (m : in out Machine; located_id : Natural);

  overriding procedure Find_Declaration
    (m          : in out Machine;
     point      : in out Declaration_Point;  --  in: reference; out: declaration
     located_id :    out Integer;
     found      :    out Boolean);

end HAC_Sys.Targets.Semantics;