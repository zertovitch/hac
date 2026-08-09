-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with Ada.Containers.Vectors;

with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Calls,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.Parser.Standard_Functions,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Aggregates is

  use Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Errors;
  use type Defs.HAC_Integer;

  ------------------------------------------------------------------
  --  A "fill template" records, once, how one "others" fill value
  --  was computed, as a tree of independently-replayable pieces:
  --  a Leaf_Code node is a captured range of already-emitted CD.ObjCode
  --  (the value computation and its store/copy, *not* including the
  --  destination-address push, which is always re-emitted fresh); a
  --  Nested_Template node is a composite fill value that was itself a
  --  nested aggregate literal, whose own components were captured the
  --  same way, recursively. This is what lets "others => Expr" (RM
  --  4.3.3) re-run Expr's underlying code once per covered component
  --  -- including any function calls in it -- without HAC's strictly
  --  forward-only scanner ever having to re-parse source text: see
  --  Replay_Fill_Template_Node below, which raw-copies a Leaf_Code
  --  node's CD.ObjCode range forward, exactly as
  --  Const_Var.Possibly_Initialized_Constant_or_Variable already does
  --  for relocated initializer code.
  ------------------------------------------------------------------
  type Fill_Template_Kind is (Leaf_Code, Nested_Template);
  type Fill_Template;
  type Fill_Template_Access is access Fill_Template;

  package Fill_Template_Vectors is new Ada.Containers.Vectors (Positive, Fill_Template_Access);
  subtype Fill_Template_List is Fill_Template_Vectors.Vector;

  type Fill_Template (Kind : Fill_Template_Kind := Leaf_Code) is record
    Relative_Offset : Defs.HAC_Integer;  --  relative to this node's own parent
    case Kind is
      when Leaf_Code =>
        Code_First, Code_Last : Integer;  --  inclusive CD.LC range to replay
      when Nested_Template =>
        Children : Fill_Template_List;
    end case;
  end record;

  --  Does the actual work of Parse_Aggregate (see the .ads for its public
  --  contract), plus one extra parameter, Template, used only for the
  --  internal recursive calls this body makes into itself: non-null
  --  exactly when the aggregate currently being parsed is itself the fill
  --  value of an ancestor "others" clause, in which case it accumulates a
  --  replayable record of each component's emitted code so that ancestor's
  --  own "others" can re-run it once per further position/field it covers
  --  (RM 4.3.3). See Fill_Others below. Kept private to this body (rather
  --  than added to Parse_Aggregate's own public profile) since
  --  Fill_Template_List is an implementation detail with no meaning to any
  --  external caller.
  procedure Parse_Aggregate_Worker
    (CD             : in out Co_Defs.Compiler_Data;
     Block_Data     : in out Block_Data_Type;
     Follow_Symbols :        Defs.Symset;
     Expected       :        Co_Defs.Exact_Subtyp;
     Dest_Level     :        Defs.Nesting_Level;
     Dest_Base      :        Defs.HAC_Integer;
     Template       :        access Fill_Template_List := null)
  is
    procedure In_Symbol is begin Scanner.In_Symbol (CD); end In_Symbol;

    --  A single view of Compiler_Data's identifier table, so this package
    --  can spell it "Id_Table" throughout instead of "id_table" (GNAT's
    --  identifier-reference style check requires every reference to match
    --  the casing of its own declaration -- this is a fresh, locally
    --  declared name, so it is free to have its own casing).
    Id_Table : Identifier_Table_Type renames CD.id_table;

    --  Terminator set used while parsing one component's value expression.
    Component_Follow_Symbols : constant Symset := Follow_Symbols + Comma_RParent;

    ------------------------------------------------------------------
    --  Appends a Leaf_Code node (Offset relative to this aggregate's own
    --  base, and the CD.ObjCode range from Code_First through the current
    --  CD.LC - 1) into Template, when Template is being captured (i.e. this
    --  whole aggregate is itself the fill value of an ancestor's "others"
    --  clause). A no-op whenever Template is null, which is the case for
    --  the overwhelming majority of ordinary (non-nested-under-"others")
    --  aggregates.
    ------------------------------------------------------------------
    procedure Capture_Leaf
      (Template : access Fill_Template_List; Offset : HAC_Integer; Code_First : Integer)
    is
    begin
      if Template /= null then
        Template.Append (new Fill_Template'(Leaf_Code, Offset, Code_First, CD.LC - 1));
      end if;
    end Capture_Leaf;

    --  Same, for a composite fill value that was itself a nested aggregate
    --  literal: Children are that nested aggregate's own captured leaves.
    procedure Capture_Nested
      (Template : access Fill_Template_List; Offset : HAC_Integer; Children : Fill_Template_List)
    is
    begin
      if Template /= null then
        Template.Append (new Fill_Template'(Nested_Template, Offset, Children));
      end if;
    end Capture_Nested;

    ------------------------------------------------------------------
    --  Replays a captured fill template at a new (New_Level, New_Base):
    --  for a Leaf_Code node, emits a fresh destination-address push at
    --  New_Base + Node.Relative_Offset, then raw-copies the node's
    --  captured CD.ObjCode range forward -- bypassing Emit_1/Emit_2 (and
    --  therefore Try_Folding/Try_Specialization) entirely, exactly as
    --  Const_Var.Possibly_Initialized_Constant_or_Variable's relocation
    --  copy loop does, since every instruction encodes addresses as
    --  Base + Y recomputed at run time and folding only ever inspects
    --  the immediately-preceding instruction, never how it got there.
    --  For a Nested_Template node, recurses over its children with the
    --  base shifted by this node's own offset.
    ------------------------------------------------------------------
    procedure Replay_Fill_Template_Node
      (Node : Fill_Template_Access; New_Level : Nesting_Level; New_Base : HAC_Integer)
    is
    begin
      case Node.Kind is
        when Leaf_Code =>
          Emit_2
            (CD, k_Push_Address,
             Operand_1_Type (New_Level), Operand_2_Type (New_Base + Node.Relative_Offset));
          for Source_LC in Node.Code_First .. Node.Code_Last loop
            CD.LC := CD.LC + 1;
            CD.ObjCode (CD.LC - 1) := CD.ObjCode (Source_LC);
          end loop;
        when Nested_Template =>
          for Child of Node.Children loop
            Replay_Fill_Template_Node (Child, New_Level, New_Base + Node.Relative_Offset);
          end loop;
      end case;
    end Replay_Fill_Template_Node;

    --  Returns a copy of Node with its own Relative_Offset shifted by
    --  Delta_Offset. A Nested_Template's Children keep their own offsets
    --  unchanged -- they are relative to Node itself, which moves as one
    --  unit; only Node's own offset (relative to *its* parent) changes.
    function Clone_With_Offset_Delta
      (Node : Fill_Template_Access; Delta_Offset : HAC_Integer) return Fill_Template_Access
    is
    begin
      case Node.Kind is
        when Leaf_Code =>
          return new Fill_Template'
            (Leaf_Code, Node.Relative_Offset + Delta_Offset, Node.Code_First, Node.Code_Last);
        when Nested_Template =>
          return new Fill_Template'
            (Nested_Template, Node.Relative_Offset + Delta_Offset, Node.Children);
      end case;
    end Clone_With_Offset_Delta;

    ------------------------------------------------------------------
    --  Emit a component (array element or record field) whose type
    --  and compile-time byte offset (relative to Dest_Base) are known.
    --  If the component's own type is composite and a nested aggregate
    --  literal follows, recurse instead of materializing an intermediate
    --  value: the recursive call writes its own leaves directly into
    --  (Dest_Level, Dest_Base + Offset), so no k_Copy_Block is needed.
    --  Capture_Template, when non-null, receives one node describing
    --  what was just emitted (a Leaf_Code range or a Nested_Template),
    --  so an enclosing "others" clause can replay it for other positions.
    ------------------------------------------------------------------
    procedure Emit_Component
      (Component_Typ : Exact_Subtyp; Offset : HAC_Integer;
       Capture_Template : access Fill_Template_List := null)
    is
      Found_Typ  : Exact_Subtyp;
      Code_First : Integer;
    begin
      if Component_Typ.TYP in Composite_Typ and then CD.Sy = LParent then
        declare
          Children : aliased Fill_Template_List;
        begin
          Parse_Aggregate_Worker
            (CD, Block_Data, Follow_Symbols, Component_Typ, Dest_Level, Dest_Base + Offset,
             Template => Children'Access);
          Capture_Nested (Capture_Template, Offset, Children);
        end;
      else
        Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base + Offset));
        Code_First := CD.LC;
        Expression (CD, Block_Data.context, Component_Follow_Symbols, Found_Typ);
        Emit_Type_Checked_Store_or_Copy (CD, Component_Typ, Found_Typ);
        Capture_Leaf (Capture_Template, Offset, Code_First);
      end if;
    end Emit_Component;

    ------------------------------------------------------------------
    --  Parses the first (ambiguous) component of a positional-looking
    --  aggregate. Per RM 4.3.3, "(Expr)" with no comma and no named
    --  association is *never* an aggregate -- it is always a plain
    --  parenthesized expression, checked against the whole destination
    --  type (Expected), not against the first element/field's type.
    --  We can only tell which case we are in once we see whether a
    --  comma follows. Position 1 is always at offset 0 (both for an
    --  array's first element and a record's first declared field), so
    --  the destination address pushed here is correct either way.
    --
    --  Returns True if a comma was seen (confirmed multi-component
    --  positional aggregate; the caller must continue from position 2),
    --  or False if this was the sole content (already fully handled as
    --  a whole-object copy; nothing more to parse for this aggregate).
    ------------------------------------------------------------------
    function Parse_First_Component_Or_Whole_Object
      (First_Component_Typ : Exact_Subtyp) return Boolean
    is
      Found_Typ  : Exact_Subtyp;
      Code_First : Integer;
    begin
      if First_Component_Typ.TYP in Composite_Typ and then CD.Sy = LParent then
        --  Offset is always 0 for position 1: leaves flatten directly into
        --  the ambient Template, exactly as if this call were itself one
        --  ordinary component of the enclosing aggregate.
        Parse_Aggregate_Worker
          (CD, Block_Data, Follow_Symbols, First_Component_Typ, Dest_Level, Dest_Base,
           Template => Template);
        return CD.Sy = Comma;
      else
        Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base));
        Code_First := CD.LC;
        Expression (CD, Block_Data.context, Component_Follow_Symbols, Found_Typ);
        if CD.Sy = Comma then
          Emit_Type_Checked_Store_or_Copy (CD, First_Component_Typ, Found_Typ);
          Capture_Leaf (Template, 0, Code_First);
          return True;
        else
          Emit_Type_Checked_Store_or_Copy (CD, Expected, Found_Typ);
          --  Sole content, not a multi-component aggregate at all -- Template
          --  cannot be non-null here (see Emit_Component/Fill_Others, which
          --  never dispatch into a "known composite, multi-component"
          --  Parse_Aggregate call with Template set unless a comma is
          --  guaranteed to have been seen for this alternative to matter).
          return False;
        end if;
      end if;
    end Parse_First_Component_Or_Whole_Object;

    ------------------------------------------------------------------
    --  Array aggregates.
    ------------------------------------------------------------------
    procedure Parse_Array_Aggregate is
      Array_Entry  : Array_Table_Entry renames CD.Arrays_Table (Expected.Ref);
      Lower_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_First;
      Upper_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_Last;
      Element_Typ  : constant Exact_Subtyp := Array_Entry.Element_xTyp;
      Element_Size : constant HAC_Integer := HAC_Integer (Array_Entry.Element_Size);

      Covered : array (HAC_Integer range Lower_Bound .. Upper_Bound) of Boolean := (others => False);

      function Offset_Of (Array_Index : HAC_Integer) return HAC_Integer is
        ((Array_Index - Lower_Bound) * Element_Size);

      procedure Mark_Covered (Array_Index : HAC_Integer) is
      begin
        if Array_Index not in Lower_Bound .. Upper_Bound then
          Error
            (CD, err_choice_out_of_range,
             "index" & HAC_Integer'Image (Array_Index) & " is out of the array's range, " &
             HAC_Integer'Image (Lower_Bound) & " .." & HAC_Integer'Image (Upper_Bound),
             severity => minor);
        elsif Covered (Array_Index) then
          Error (CD, err_aggregate_index_covered_twice, HAC_Integer'Image (Array_Index), severity => minor);
        else
          Covered (Array_Index) := True;
        end if;
      end Mark_Covered;

      procedure Check_Full_Coverage is
      begin
        for Array_Index in Lower_Bound .. Upper_Bound loop
          if not Covered (Array_Index) then
            Error (CD, err_aggregate_index_not_covered, "", severity => minor);
            exit;
          end if;
        end loop;
      end Check_Full_Coverage;

      --  Parses and fills "others => Expr" for every remaining (not yet
      --  covered) position. Since bounds are always static in HAC, every
      --  remaining position is known at compile time. Expr is parsed once,
      --  directly into the first remaining position, while Emit_Component
      --  captures a replayable template of the code it emitted (whether a
      --  scalar/whole-value computation or, for a nested-aggregate fill
      --  value such as "others => (others => 0)", a whole subtree of
      --  captured leaves) -- then that template is replayed (cloned with a
      --  shifted offset, emitting fresh destination-address pushes and
      --  raw-copied leaf code) once per remaining position, so any function
      --  calls or other side effects in Expr genuinely re-run once per
      --  position, per RM 4.3.3.
      procedure Fill_Others is
        Local_Root : aliased Fill_Template_List;
        Effective_Template : constant not null access Fill_Template_List :=
          (if Template /= null then Template else Local_Root'Access);
        First_Remaining : HAC_Integer := Lower_Bound;
        Found_Any       : Boolean := False;
      begin
        In_Symbol;  --  Consume OTHERS_Symbol.
        Need (CD, Finger, err_FINGER_missing);
        for Array_Index in Lower_Bound .. Upper_Bound loop
          if not Covered (Array_Index) then
            First_Remaining := Array_Index;
            Found_Any := True;
            exit;
          end if;
        end loop;
        if not Found_Any then
          --  Nothing left to cover (e.g. "(1, 2, others => X)" for a
          --  2-element array); still parse (and discard) the value once,
          --  so the parser stays positioned correctly.
          declare
            Discard : Exact_Subtyp;
          begin
            Expression (CD, Block_Data.context, Follow_Symbols + Comma_RParent, Discard);
          end;
        else
          Emit_Component (Element_Typ, Offset_Of (First_Remaining), Effective_Template);
          Covered (First_Remaining) := True;
          declare
            Captured : constant Fill_Template_Access := Effective_Template.Last_Element;
          begin
            for Array_Index in Lower_Bound .. Upper_Bound loop
              if not Covered (Array_Index) then
                declare
                  Cloned : constant Fill_Template_Access :=
                    Clone_With_Offset_Delta
                      (Captured, Offset_Of (Array_Index) - Offset_Of (First_Remaining));
                begin
                  Replay_Fill_Template_Node (Cloned, Dest_Level, Dest_Base);
                  --  Also recorded, so a further ancestor's own capture
                  --  (this whole array being nested under yet another
                  --  "others") sees every position, not just the first.
                  Effective_Template.Append (Cloned);
                end;
                Covered (Array_Index) := True;
              end if;
            end loop;
          end;
        end if;
        if CD.Sy = Comma then
          --  "others" was not the last association (RM 4.3.3 (5)).
          Error (CD, err_aggregate_others_not_last, severity => major);
        end if;
      end Fill_Others;

      --  Pushes the destination address, the literal Literal_Value (of kind
      --  Literal_Symbol), and returns its (singleton-range) Exact_Subtyp --
      --  shared by the first ambiguous component and by Continue_Positional,
      --  so a literal choice's value is emitted identically in both places.
      --  Code_First reports the point right after the destination-address
      --  push, for the caller to hand to Capture_Leaf once it has also
      --  emitted this component's store.
      function Push_Literal_Component
        (Literal_Symbol : Symbol; Literal_Value : HAC_Integer; Offset : HAC_Integer;
         Code_First     : out Integer) return Exact_Subtyp
      is
        Found_Typ : Exact_Subtyp;
      begin
        Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base + Offset));
        Code_First := CD.LC;
        Found_Typ.Construct_Root (if Literal_Symbol = character_literal then Chars else Ints);
        CD.target.Emit_Push_Discrete_Literal (Literal_Value);
        Ranges.Set_Singleton_Range (Found_Typ, Literal_Value);
        return Found_Typ;
      end Push_Literal_Component;

      --  Continues a positional array aggregate from Starting_Index. A plain
      --  "others => Expr" may appear as the last association even after
      --  positional ones (the common "(1, 2, others => 0)" idiom).
      procedure Continue_Positional (Starting_Index : HAC_Integer) is
        Next_Index : HAC_Integer := Starting_Index;
      begin
        while CD.Sy = Comma loop
          In_Symbol;  --  Consume ','.
          if CD.Sy = OTHERS_Symbol then
            Fill_Others;
            exit;
          end if;
          if Next_Index > Upper_Bound then
            Error (CD, err_general_error, "too many components in array aggregate", severity => minor);
            exit;
          end if;
          if CD.Sy in integer_literal | character_literal then
            --  Peek: is this actually a named choice illegally following
            --  positional associations (RM 4.3 (4)/(5))?
            declare
              Peek_Symbol : constant Symbol     := CD.Sy;
              Peek_Value  : constant HAC_Integer := CD.INum;
            begin
              In_Symbol;
              if CD.Sy = Finger then
                Error (CD, err_aggregate_positional_after_named, severity => major);
                exit;
              end if;
              --  Not a named choice after all: genuinely this position's value.
              declare
                Code_First : Integer;
                Found_Typ  : constant Exact_Subtyp :=
                  Push_Literal_Component (Peek_Symbol, Peek_Value, Offset_Of (Next_Index), Code_First);
              begin
                Emit_Type_Checked_Store_or_Copy (CD, Element_Typ, Found_Typ);
                Capture_Leaf (Template, Offset_Of (Next_Index), Code_First);
              end;
            end;
          else
            Emit_Component (Element_Typ, Offset_Of (Next_Index), Template);
          end if;
          Mark_Covered (Next_Index);
          Next_Index := Next_Index + 1;
        end loop;
        Check_Full_Coverage;
      end Continue_Positional;

    begin
      if CD.Sy = OTHERS_Symbol then
        Fill_Others;
      elsif CD.Sy in integer_literal | character_literal then
        declare
          Choice_Symbol : constant Symbol     := CD.Sy;
          Choice_Value  : constant HAC_Integer := CD.INum;
        begin
          In_Symbol;  --  Peek past the literal.
          if CD.Sy = Finger then
            --  Confirmed named form: "Choice_Value => Value".
            In_Symbol;  --  Consume '=>'.
            Mark_Covered (Choice_Value);
            Emit_Component (Element_Typ, Offset_Of (Choice_Value), Template);
            while CD.Sy = Comma loop
              In_Symbol;
              if CD.Sy = OTHERS_Symbol then
                Fill_Others;
                exit;
              elsif CD.Sy in integer_literal | character_literal then
                declare
                  Next_Choice_Value : constant HAC_Integer := CD.INum;
                begin
                  In_Symbol;
                  Need (CD, Finger, err_FINGER_missing);
                  Mark_Covered (Next_Choice_Value);
                  Emit_Component (Element_Typ, Offset_Of (Next_Choice_Value), Template);
                end;
              else
                Error
                  (CD, err_not_yet_implemented,
                   "this kind of array aggregate choice (only literal indices " &
                   "and ""others"" are supported)",
                   severity => major);
                exit;
              end if;
            end loop;
            Check_Full_Coverage;
          else
            --  Positional: the first component's value is this literal,
            --  already consumed while peeking.
            declare
              Code_First : Integer;
              Found_Typ  : constant Exact_Subtyp :=
                Push_Literal_Component (Choice_Symbol, Choice_Value, 0, Code_First);
            begin
              if CD.Sy = Comma then
                Emit_Type_Checked_Store_or_Copy (CD, Element_Typ, Found_Typ);
                Capture_Leaf (Template, 0, Code_First);
                Mark_Covered (Lower_Bound);
                Continue_Positional (Lower_Bound + 1);
              else
                --  Sole bare literal: whole-object check against Expected.
                Emit_Type_Checked_Store_or_Copy (CD, Expected, Found_Typ);
              end if;
            end;
          end if;
        end;
      else
        --  Generic first component: nested aggregate, identifier, function
        --  call, parenthesized sub-expression, unary operator, ...
        if Parse_First_Component_Or_Whole_Object (Element_Typ) then
          Mark_Covered (Lower_Bound);
          Continue_Positional (Lower_Bound + 1);
        end if;
      end if;
    end Parse_Array_Aggregate;

    ------------------------------------------------------------------
    --  Record aggregates.
    ------------------------------------------------------------------
    procedure Parse_Record_Aggregate is
      Field_Count   : Natural := 0;
      Counting_Walk : Integer := CD.Blocks_Table (Expected.Ref).Last_Id_Idx;
    begin
      while Counting_Walk /= No_Id loop
        Field_Count := Field_Count + 1;
        Counting_Walk := Id_Table (Counting_Walk).link;
      end loop;
      if Field_Count = 0 then
        return;
      end if;
      declare
        --  Field Id_Table indices, in forward (declaration) order.
        Fields        : array (1 .. Field_Count) of Integer;
        Covered       : array (1 .. Field_Count) of Boolean := (others => False);
        Building_Walk : Integer := CD.Blocks_Table (Expected.Ref).Last_Id_Idx;

        function Position_Of (Field_Id : Integer) return Natural is
        begin
          for Position in Fields'Range loop
            if Fields (Position) = Field_Id then
              return Position;
            end if;
          end loop;
          return 0;
        end Position_Of;

        procedure Mark_Covered (Position : Natural) is
        begin
          if Position = 0 then
            null;  --  Unresolved field; error already reported by the caller.
          elsif Covered (Position) then
            Error
              (CD, err_aggregate_field_covered_twice,
               A2S (Id_Table (Fields (Position)).name_with_case), severity => minor);
          else
            Covered (Position) := True;
          end if;
        end Mark_Covered;

        procedure Check_Full_Coverage is
        begin
          for Position in Covered'Range loop
            if not Covered (Position) then
              Error
                (CD, err_aggregate_field_not_covered,
                 A2S (Id_Table (Fields (Position)).name_with_case), severity => minor);
            end if;
          end loop;
        end Check_Full_Coverage;

        procedure Emit_Field (Position : Positive; Capture_Template : access Fill_Template_List := null) is
        begin
          Emit_Component
            (Id_Table (Fields (Position)).xtyp, Id_Table (Fields (Position)).adr_or_sz, Capture_Template);
        end Emit_Field;

        --  Resolves an already-consumed identifier (so Locate_CD_Id's
        --  reliance on the *current* token cannot be used) as either a
        --  direct_name (RM 4.1) reference -- an object/constant/parameter,
        --  or a declared number or enumeration literal (e.g. True/False) --
        --  or the start of a function_call (RM 6.4): CD.Sy is already
        --  positioned right after the identifier, e.g. at '(' for the
        --  argument list, exactly as Primary.Process_Identifier expects
        --  when it dispatches to Calls.Subprogram_or_Entry_Call /
        --  Standard_Functions.Standard_Function. Used whenever a positional
        --  record component's first token is an identifier that turned out
        --  (after peeking) *not* to be one of this record's own field names
        --  in named-association form.
        function Resolve_Direct_Name_Or_Call_Value (Name : Alfa) return Exact_Subtyp is
          Identifier_Id : constant Natural :=
            Locate_Identifier (CD, Name, Block_Data.context.level, Fail_when_No_Id => False);
          Found_Typ : Exact_Subtyp := undefined_subtyp;
        begin
          if Identifier_Id = No_Id then
            Error (CD, err_undefined_identifier, A2S (Name), severity => major);
            return Found_Typ;
          end if;
          declare
            Identifier_Entry : Identifier_Table_Entry renames Id_Table (Identifier_Id);
          begin
            Found_Typ := Identifier_Entry.xtyp;
            case Identifier_Entry.entity is
              when Object_Kind =>
                Emit_2
                  (CD,
                   (if Standard_or_Enum_Typ (Found_Typ.TYP) then
                      (if Identifier_Entry.normal then
                         (if Discrete_Typ (Identifier_Entry.xtyp.TYP) then
                            k_Push_Discrete_Value
                          else
                            k_Push_Value)
                       else k_Push_Indirect_Value)
                    elsif Identifier_Entry.normal then k_Push_Address
                    else k_Push_Discrete_Value),
                   Operand_1_Type (Identifier_Entry.lev), Operand_2_Type (Identifier_Entry.adr_or_sz));
                Mark_Read_and_Check_Read_before_Written (CD, Block_Data.context, Identifier_Entry);
              when declared_number_or_enum_item =>
                if Found_Typ.TYP = Floats then
                  Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (Identifier_Entry.adr_or_sz));
                else
                  Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (Identifier_Entry.adr_or_sz));
                  Ranges.Set_Singleton_Range (Found_Typ, Identifier_Entry.adr_or_sz);
                end if;
              when funktion =>
                Calls.Subprogram_or_Entry_Call
                  (CD, Block_Data.context, Component_Follow_Symbols, Identifier_Id, Normal_Procedure_Call);
              when funktion_intrinsic =>
                Standard_Functions.Standard_Function
                  (CD, Block_Data.context, Component_Follow_Symbols, Identifier_Id,
                   SF_Code'Val (Identifier_Entry.adr_or_sz), Found_Typ);
              when others =>
                Error
                  (CD, err_not_yet_implemented,
                   "this kind of positional record aggregate component (attributes, " &
                   "selected/indexed components) starting with an identifier",
                   severity => major);
            end case;
          end;
          return Found_Typ;
        end Resolve_Direct_Name_Or_Call_Value;

        --  Parses and fills "others => Expr" for the remaining (not yet
        --  covered) fields. Ada allows this only when every field "others"
        --  applies to shares the same type; check that first. Expr is
        --  parsed once, directly into the first remaining field, while
        --  Emit_Field/Emit_Component capture a replayable template of the
        --  code emitted; that template is then replayed (cloned with a
        --  shifted offset) once per remaining field, so side effects in
        --  Expr re-run once per field, per RM 4.3.3.
        procedure Fill_Others is
          Common_Typ      : Exact_Subtyp;
          Any_Uncovered   : Boolean := False;
          Types_Differ    : Boolean := False;
          First_Remaining : Natural := 0;
        begin
          for Position in Covered'Range loop
            if not Covered (Position) then
              declare
                This_Typ : constant Exact_Subtyp := Id_Table (Fields (Position)).xtyp;
              begin
                if not Any_Uncovered then
                  Common_Typ := This_Typ;
                  Any_Uncovered := True;
                  First_Remaining := Position;
                elsif Common_Typ.TYP /= This_Typ.TYP or else Common_Typ.Ref /= This_Typ.Ref then
                  Types_Differ := True;
                end if;
              end;
            end if;
          end loop;
          In_Symbol;  --  Consume OTHERS_Symbol.
          Need (CD, Finger, err_FINGER_missing);
          if Types_Differ then
            Error (CD, err_aggregate_others_field_types_differ, severity => major);
          end if;
          if not Any_Uncovered then
            --  Nothing left to cover; still parse (and discard) the value so
            --  the parser stays positioned correctly.
            declare
              Discard : Exact_Subtyp;
            begin
              Expression (CD, Block_Data.context, Component_Follow_Symbols, Discard);
            end;
          elsif Types_Differ then
            --  Legality error already reported above; still parse (and
            --  discard) the value so the parser stays positioned correctly,
            --  matching the "nothing left to cover" branch.
            declare
              Discard : Exact_Subtyp;
            begin
              Expression (CD, Block_Data.context, Component_Follow_Symbols, Discard);
            end;
          else
            declare
              Local_Root : aliased Fill_Template_List;
              Effective_Template : constant not null access Fill_Template_List :=
                (if Template /= null then Template else Local_Root'Access);
            begin
              Emit_Field (First_Remaining, Effective_Template);
              Covered (First_Remaining) := True;
              declare
                Captured : constant Fill_Template_Access := Effective_Template.Last_Element;
              begin
                for Position in Covered'Range loop
                  if not Covered (Position) then
                    declare
                      Cloned : constant Fill_Template_Access :=
                        Clone_With_Offset_Delta
                          (Captured,
                           Id_Table (Fields (Position)).adr_or_sz -
                             Id_Table (Fields (First_Remaining)).adr_or_sz);
                    begin
                      Replay_Fill_Template_Node (Cloned, Dest_Level, Dest_Base);
                      Effective_Template.Append (Cloned);
                    end;
                    Covered (Position) := True;
                  end if;
                end loop;
              end;
            end;
          end if;
          if CD.Sy = Comma then
            --  "others" was not the last association (RM 4.3.3 (5)).
            Error (CD, err_aggregate_others_not_last, severity => major);
          end if;
        end Fill_Others;

        --  Continues a positional record aggregate from position 2 (field 1
        --  is already filled by the caller). Shared by both entry points
        --  that can reach a confirmed positional aggregate: the ordinary
        --  first-component case in Parse_First_Component_Or_Whole_Object,
        --  and the case where the first token was an identifier that
        --  turned out (after peeking) not to be a named field association.
        procedure Continue_Positional_Record is
          Position : Positive := 2;
        begin
          while CD.Sy = Comma loop
            In_Symbol;  --  Consume ','.
            if CD.Sy = OTHERS_Symbol then
              Fill_Others;
              exit;
            end if;
            if Position > Field_Count then
              Error (CD, err_general_error, "too many components in record aggregate", severity => minor);
              exit;
            end if;
            if CD.Sy = IDent then
              --  Peek: is this actually a named field association
              --  illegally following positional ones (RM 4.3 (4)/(5))?
              declare
                Peek_Name     : constant Alfa   := CD.Id;
                Peek_Field_Id : constant Integer := Locate_Record_Field (CD, Expected.Ref, Peek_Name);
              begin
                In_Symbol;
                if Peek_Field_Id /= No_Id and then CD.Sy = Finger then
                  Error (CD, err_aggregate_positional_after_named, severity => major);
                  exit;
                end if;
                --  Genuinely a positional value starting with a bare
                --  identifier (a variable/constant reference, an
                --  enumeration literal such as True/False, or a function
                --  call).
                declare
                  Offset     : constant HAC_Integer := Id_Table (Fields (Position)).adr_or_sz;
                  Code_First : Integer;
                begin
                  Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base + Offset));
                  Code_First := CD.LC;
                  Emit_Type_Checked_Store_or_Copy
                    (CD, Id_Table (Fields (Position)).xtyp,
                     Resolve_Direct_Name_Or_Call_Value (Peek_Name));
                  Capture_Leaf (Template, Offset, Code_First);
                end;
              end;
            else
              Emit_Field (Position, Template);
            end if;
            Mark_Covered (Position);
            Position := Position + 1;
          end loop;
          Check_Full_Coverage;
        end Continue_Positional_Record;

      begin
        for Position in reverse Fields'Range loop
          Fields (Position) := Building_Walk;
          Building_Walk := Id_Table (Building_Walk).link;
        end loop;

        if CD.Sy = OTHERS_Symbol then
          Fill_Others;
          Check_Full_Coverage;
        elsif CD.Sy = IDent then
          declare
            Choice_Name : constant Alfa   := CD.Id;
            Field_Id    : constant Integer := Locate_Record_Field (CD, Expected.Ref, Choice_Name);
          begin
            In_Symbol;  --  Peek past the identifier.
            if Field_Id /= No_Id and then CD.Sy = Finger then
              --  Confirmed named form.
              In_Symbol;  --  Consume '=>'.
              Mark_Covered (Position_Of (Field_Id));
              Emit_Field (Position_Of (Field_Id), Template);
              while CD.Sy = Comma loop
                In_Symbol;
                if CD.Sy = OTHERS_Symbol then
                  Fill_Others;
                  exit;
                end if;
                if CD.Sy /= IDent then
                  Error (CD, err_identifier_missing, severity => major);
                  exit;
                end if;
                declare
                  Next_Field_Name : constant Alfa   := CD.Id;
                  Next_Field_Id   : constant Integer := Locate_Record_Field (CD, Expected.Ref, Next_Field_Name);
                begin
                  In_Symbol;
                  if Next_Field_Id = No_Id then
                    Error (CD, err_undefined_identifier, A2S (CD.Id_with_case), severity => major);
                    exit;
                  end if;
                  Need (CD, Finger, err_FINGER_missing);
                  Mark_Covered (Position_Of (Next_Field_Id));
                  Emit_Field (Position_Of (Next_Field_Id), Template);
                end;
              end loop;
              Check_Full_Coverage;
            else
              --  Not a named field association: the already-consumed
              --  identifier is not one of this record's own field names,
              --  so resolve it as an ordinary value and treat it as this
              --  aggregate's first (ambiguous) component -- mirrors
              --  Parse_First_Component_Or_Whole_Object, which cannot be
              --  reused directly here since the identifier had to be
              --  looked up as a possible field name first (and consumed
              --  in the process).
              declare
                Code_First : Integer;
                Found_Typ  : Exact_Subtyp;
              begin
                Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base));
                Code_First := CD.LC;
                Found_Typ := Resolve_Direct_Name_Or_Call_Value (Choice_Name);
                if CD.Sy = Comma then
                  Emit_Type_Checked_Store_or_Copy (CD, Id_Table (Fields (1)).xtyp, Found_Typ);
                  Capture_Leaf (Template, 0, Code_First);
                  Mark_Covered (1);
                  Continue_Positional_Record;
                else
                  --  Sole bare identifier: whole-object check against Expected.
                  Emit_Type_Checked_Store_or_Copy (CD, Expected, Found_Typ);
                end if;
              end;
            end if;
          end;
        else
          if Parse_First_Component_Or_Whole_Object (Id_Table (Fields (1)).xtyp) then
            Mark_Covered (1);
            Continue_Positional_Record;
          end if;
        end if;
      end;
    end Parse_Record_Aggregate;

  begin
    In_Symbol;  --  Consume '('.
    case Composite_Typ (Expected.TYP) is
      when Arrays  => Parse_Array_Aggregate;
      when Records => Parse_Record_Aggregate;
    end case;
    Need (CD, RParent, err_closing_parenthesis_missing);
  end Parse_Aggregate_Worker;

  procedure Parse_Aggregate
    (CD             : in out Co_Defs.Compiler_Data;
     Block_Data     : in out Block_Data_Type;
     Follow_Symbols :        Defs.Symset;
     Expected       :        Co_Defs.Exact_Subtyp;
     Dest_Level     :        Defs.Nesting_Level;
     Dest_Base      :        Defs.HAC_Integer)
  is
  begin
    Parse_Aggregate_Worker (CD, Block_Data, Follow_Symbols, Expected, Dest_Level, Dest_Base);
  end Parse_Aggregate;

end HAC_Sys.Parser.Aggregates;
