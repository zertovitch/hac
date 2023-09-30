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
     Ada.Containers.Ordered_Maps,
     Ada.Strings.Unbounded.Hash;

with HAC_Sys.Co_Defs;

with HAT;

package HAC_Sys.Targets.Semantics is

  type Reference_Point is tagged record
    file_name    : HAT.VString;
    line, column : Integer;
  end record;

  type Declaration_Point is new Reference_Point with record
    is_built_in : Boolean;
    id_index    : Integer;
  end record;

  --  Reference map
  --  =============
  --
  --  This container holds all references to variables, functions,
  --  types, etc. The search key is the exact position of the first
  --  letter of the identifier.

  --
  --  Key                  Value
  --  ===                  =====
  --  [file_name i j]      index in the Id Table

  package Reference_Mapping is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,                --  [file_name i j]
     Element_Type    => Positive,                   --  Index in the Id Table
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."=");

  --  Declaration array
  --  =================
  --
  --  This container holds the exact position of the declarations.
  --  It is a simple array; the element #i corresponds to
  --  the identifer #i in the compiler's identifier table.
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

  --  Declaration localization
  --  ========================
  --
  --  This container provides the reverse operation of a
  --  Declaration_Point_Array. It enables the localization of the index
  --  of the last identifier on any given line in a source code file.
  --  From the index, it is then possible to gather all the identifiers
  --  visible at the next line (we don't go down to a column-wise
  --  localization for simplicity purposes).
  --  Due to nesting, that overall identifier list is not simply growing
  --  along the text, so the list of visible identifiers it is a part of
  --  a declaration tree. You build it by going from a leaf (index of
  --  the last identifier) to the root.

  package Declaration_Line_Maps is new Ada.Containers.Ordered_Maps
    (Key_Type        => Integer,     --  Line number
     Element_Type    => Natural);    --  Index in the Id Table

  type Declaration_Line_Map_Access is access Declaration_Line_Maps.Map;

  package File_Names_to_Line_Maps_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,                  --  File_name
     Element_Type    => Declaration_Line_Map_Access,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."=");

  type Machine is limited new Targets.Machine with record
    CD         : Co_Defs.Compiler_Data_Access;
    ref_map    : Reference_Mapping.Map;
    decl_array : Declaration_Point_Array;
    loc_map    : File_Names_to_Line_Maps_Maps.Map;
    started    : HAT.Time;
    finished   : HAT.Time;
    total_time : Duration := 0.0;
    busy       : Boolean  := False with Volatile;
  end record;

  type Semantics_Machine_Reference is access all Machine'Class;

  overriding procedure Finalize (m : in out Machine);

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

  overriding procedure Mark_Reference (m : in out Machine; located_id : Natural);

  overriding procedure Mark_Declaration (m : in out Machine; is_built_in : Boolean);

  --  From given valid reference point, get the corresponding declaration.
  procedure Find_Referenced_Declaration
    (m         : in     Machine;
     ref       : in     Reference_Point'Class;
     decl      :    out Declaration_Point'Class;
     was_found :    out Boolean);

  --  This is for "auto-complete" purposes.
  --  The list of identifiers is sorted, separated by spaces.
  function Find_Possible_Declarations
    (m        : Machine;
     point    : Reference_Point'Class;
     prefix   : String;
     max_list : Positive;
     max_scan : Positive) return String;

end HAC_Sys.Targets.Semantics;
