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

with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Defaults is

  use Compiler.PCode_Emit, Co_Defs, Defs, Helpers, PCode, Errors;
  use type Defs.HAC_Integer;

  function Parse_Static_Default_Value
    (CD       : in out Co_Defs.Compiler_Data;
     Level    :        Defs.Nesting_Level;
     FSys     :        Defs.Symset;
     Expected :        Co_Defs.Exact_Subtyp) return Co_Defs.Default_Value_Access
  is
    procedure In_Symbol is begin Scanner.In_Symbol (CD); end In_Symbol;

    Id_Table : Identifier_Table_Type renames CD.id_table;

    Component_Follow : constant Symset := FSys + Comma_RParent;

    ------------------------------------------------------------------
    --  Resolves an already-consumed identifier as a static value: only
    --  a declared number or enumeration literal qualifies (a variable,
    --  constant, or function reference is out of scope for a static
    --  default -- see the package's header comment).
    ------------------------------------------------------------------
    function Resolve_Static_Direct_Name (Name : Alfa) return Default_Value_Access is
      Identifier_Id : constant Natural :=
        Locate_Identifier (CD, Name, Level, Fail_when_No_Id => False);
    begin
      if Identifier_Id = No_Id
        or else Id_Table (Identifier_Id).entity /= declared_number_or_enum_item
      then
        Error (CD, err_default_value_must_be_static, A2S (Name), severity => major);
        return new Default_Value'
          (Scalar_Default, undefined_subtyp, 0, 0.0);
      end if;
      declare
        Entry_Ref : Identifier_Table_Entry renames Id_Table (Identifier_Id);
      begin
        return new Default_Value'
          (Scalar_Default, Entry_Ref.xtyp,
           (if Entry_Ref.xtyp.TYP = Floats then 0 else Entry_Ref.adr_or_sz),
           (if Entry_Ref.xtyp.TYP = Floats
            then CD.Float_Constants_Table (Integer (Entry_Ref.adr_or_sz))
            else 0.0));
      end;
    end Resolve_Static_Direct_Name;

    function Parse_Static_Scalar_Value (Scalar_Typ : Exact_Subtyp) return Default_Value_Access is
      pragma Unreferenced (Scalar_Typ);
      C : Constant_Rec;
    begin
      Expressions.Static_Scalar_Expression (CD, Level, Component_Follow, C);
      --  C.R is only meaningfully set by Static_Scalar_Expression when
      --  C.TP.TYP = Floats -- for any other (discrete) result, C.R is left
      --  untouched (possibly uninitialized), so it must not be read here.
      return new Default_Value'
        (Scalar_Default, C.TP, C.I, (if C.TP.TYP = Floats then C.R else 0.0));
    end Parse_Static_Scalar_Value;

    function Parse_Static_Composite_Value
      (Composite_Expected : Exact_Subtyp) return Default_Value_Access;
    --  Forward declaration: needed since array/record parsing below may
    --  recurse into a nested composite default.

    ------------------------------------------------------------------
    --  Array defaults: "(Value, Value, ...)", "(Index => Value, ...)",
    --  "(others => Value)" -- mirrors the shape of
    --  Aggregates.Parse_Array_Aggregate's coverage tracking, but builds a
    --  Default_Value tree instead of emitting code, and never needs the
    --  Fill_Template capture/replay mechanism: since every value here is
    --  static, the *same* computed Default_Value_Access can simply be
    --  reused (shared, not cloned) for every position "others" covers.
    ------------------------------------------------------------------
    function Parse_Array_Default (Array_Expected : Exact_Subtyp) return Default_Value_Access is
      Array_Entry  : Array_Table_Entry renames CD.Arrays_Table (Array_Expected.Ref);
      Lower_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_First;
      Upper_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_Last;
      Element_Typ  : constant Exact_Subtyp := Array_Entry.Element_xTyp;
      Element_Size : constant HAC_Integer := HAC_Integer (Array_Entry.Element_Size);

      Covered    : array (HAC_Integer range Lower_Bound .. Upper_Bound) of Boolean :=
        (others => False);
      Components : Default_Component_Vectors.Vector;

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

      function Parse_One_Value return Default_Value_Access is
      begin
        if Element_Typ.TYP in Composite_Typ and then CD.Sy = LParent then
          return Parse_Static_Composite_Value (Element_Typ);
        elsif CD.Sy = IDent then
          declare
            Name : constant Alfa := CD.Id;
          begin
            In_Symbol;
            return Resolve_Static_Direct_Name (Name);
          end;
        else
          return Parse_Static_Scalar_Value (Element_Typ);
        end if;
      end Parse_One_Value;

      --  Builds a literal value directly from an already-consumed (peeked)
      --  literal token, without going through Parse_Static_Scalar_Value
      --  (which expects to start parsing fresh at the current token).
      function Literal_Value (Literal_Symbol : Symbol; Literal_Int : HAC_Integer) return Default_Value_Access is
        Scalar_Typ : Exact_Subtyp;
      begin
        Scalar_Typ.Construct_Root (if Literal_Symbol = character_literal then Chars else Ints);
        return new Default_Value'(Scalar_Default, Scalar_Typ, Literal_Int, 0.0);
      end Literal_Value;

      Next_Index : HAC_Integer := Lower_Bound;
    begin
      loop
        if CD.Sy = OTHERS_Symbol then
          In_Symbol;  --  Consume OTHERS_Symbol.
          Need (CD, Finger, err_FINGER_missing);
          declare
            Fill : constant Default_Value_Access := Parse_One_Value;
          begin
            for Array_Index in Lower_Bound .. Upper_Bound loop
              if not Covered (Array_Index) then
                Components.Append ((Offset_Of (Array_Index), Fill));
                Covered (Array_Index) := True;
              end if;
            end loop;
          end;
          exit;
        elsif CD.Sy in integer_literal | character_literal then
          declare
            Peek_Symbol : constant Symbol     := CD.Sy;
            Peek_Value  : constant HAC_Integer := CD.INum;
          begin
            In_Symbol;
            if CD.Sy = Finger then
              --  Named index choice: "Index => Value".
              In_Symbol;  --  Consume '=>'.
              Mark_Covered (Peek_Value);
              Components.Append ((Offset_Of (Peek_Value), Parse_One_Value));
            else
              --  Positional: the peeked literal is this position's own value.
              Components.Append ((Offset_Of (Next_Index), Literal_Value (Peek_Symbol, Peek_Value)));
              Mark_Covered (Next_Index);
              Next_Index := Next_Index + 1;
            end if;
          end;
        else
          Components.Append ((Offset_Of (Next_Index), Parse_One_Value));
          Mark_Covered (Next_Index);
          Next_Index := Next_Index + 1;
        end if;
        exit when CD.Sy /= Comma;
        In_Symbol;  --  Consume ','.
      end loop;
      for Array_Index in Lower_Bound .. Upper_Bound loop
        if not Covered (Array_Index) then
          Error (CD, err_aggregate_index_not_covered, "", severity => minor);
          exit;
        end if;
      end loop;
      return new Default_Value'(Composite_Default, Components);
    end Parse_Array_Default;

    ------------------------------------------------------------------
    --  Record defaults: "(Value, Value, ...)", "(Field => Value, ...)",
    --  "(others => Value)". Unlike Aggregates' record parsing, a
    --  positional component may not start with a bare identifier here
    --  (documented v1 restriction: use named form, "Field => Some_Enum",
    --  for a field whose static value is itself a bare identifier) --
    --  this avoids re-implementing the field-name/value-identifier
    --  disambiguation for a case that this feature's real use (literal
    --  values) never needs.
    ------------------------------------------------------------------
    function Parse_Record_Default (Record_Expected : Exact_Subtyp) return Default_Value_Access is
      Field_Count   : Natural := 0;
      Counting_Walk : Integer := CD.Blocks_Table (Record_Expected.Ref).Last_Id_Idx;
    begin
      while Counting_Walk /= No_Id loop
        Field_Count := Field_Count + 1;
        Counting_Walk := Id_Table (Counting_Walk).link;
      end loop;
      if Field_Count = 0 then
        return null;
      end if;
      declare
        Fields        : array (1 .. Field_Count) of Integer;
        Covered       : array (1 .. Field_Count) of Boolean := (others => False);
        Building_Walk : Integer := CD.Blocks_Table (Record_Expected.Ref).Last_Id_Idx;
        Components    : Default_Component_Vectors.Vector;
        Next_Position : Positive := 1;

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
            null;
          elsif Covered (Position) then
            Error
              (CD, err_aggregate_field_covered_twice,
               A2S (Id_Table (Fields (Position)).name_with_case), severity => minor);
          else
            Covered (Position) := True;
          end if;
        end Mark_Covered;

        function Parse_One_Value (Field_Typ : Exact_Subtyp) return Default_Value_Access is
        begin
          if Field_Typ.TYP in Composite_Typ and then CD.Sy = LParent then
            return Parse_Static_Composite_Value (Field_Typ);
          elsif CD.Sy = IDent then
            declare
              Name : constant Alfa := CD.Id;
            begin
              In_Symbol;
              return Resolve_Static_Direct_Name (Name);
            end;
          else
            return Parse_Static_Scalar_Value (Field_Typ);
          end if;
        end Parse_One_Value;

      begin
        for Position in reverse Fields'Range loop
          Fields (Position) := Building_Walk;
          Building_Walk := Id_Table (Building_Walk).link;
        end loop;

        loop
          if CD.Sy = OTHERS_Symbol then
            In_Symbol;  --  Consume OTHERS_Symbol.
            Need (CD, Finger, err_FINGER_missing);
            declare
              Common_Typ    : Exact_Subtyp;
              Any_Uncovered : Boolean := False;
              Types_Differ  : Boolean := False;
            begin
              for Position in Covered'Range loop
                if not Covered (Position) then
                  declare
                    This_Typ : constant Exact_Subtyp := Id_Table (Fields (Position)).xtyp;
                  begin
                    if not Any_Uncovered then
                      Common_Typ := This_Typ;
                      Any_Uncovered := True;
                    elsif Common_Typ.TYP /= This_Typ.TYP or else Common_Typ.Ref /= This_Typ.Ref then
                      Types_Differ := True;
                    end if;
                  end;
                end if;
              end loop;
              if Types_Differ then
                Error (CD, err_aggregate_others_field_types_differ, severity => major);
              end if;
              if Any_Uncovered and then not Types_Differ then
                declare
                  Fill : constant Default_Value_Access := Parse_One_Value (Common_Typ);
                begin
                  for Position in Covered'Range loop
                    if not Covered (Position) then
                      Components.Append ((Id_Table (Fields (Position)).adr_or_sz, Fill));
                      Covered (Position) := True;
                    end if;
                  end loop;
                end;
              else
                declare
                  Discard : Default_Value_Access;
                  pragma Unreferenced (Discard);
                begin
                  Discard := Parse_One_Value (undefined_subtyp);
                end;
              end if;
            end;
            exit;
          elsif CD.Sy = IDent then
            declare
              Peek_Name : constant Alfa := CD.Id;
              Field_Id  : constant Integer :=
                Locate_Record_Field (CD, Record_Expected.Ref, Peek_Name);
            begin
              In_Symbol;
              if Field_Id = No_Id or else CD.Sy /= Finger then
                Error
                  (CD, err_default_value_must_be_static,
                   "expected a field name (""Field => Value"") -- a positional " &
                   "record-default component cannot start with a bare identifier",
                   severity => major);
                exit;
              end if;
              In_Symbol;  --  Consume '=>'.
              Mark_Covered (Position_Of (Field_Id));
              Components.Append
                ((Id_Table (Field_Id).adr_or_sz,
                  Parse_One_Value (Id_Table (Field_Id).xtyp)));
              Next_Position := Position_Of (Field_Id) + 1;
            end;
          else
            if Next_Position > Field_Count then
              Error (CD, err_general_error, "too many components in default aggregate", severity => minor);
              exit;
            end if;
            Components.Append
              ((Id_Table (Fields (Next_Position)).adr_or_sz,
                Parse_One_Value (Id_Table (Fields (Next_Position)).xtyp)));
            Mark_Covered (Next_Position);
            Next_Position := Next_Position + 1;
          end if;
          exit when CD.Sy /= Comma;
          In_Symbol;  --  Consume ','.
        end loop;
        for Position in Covered'Range loop
          if not Covered (Position) then
            Error
              (CD, err_aggregate_field_not_covered,
               A2S (Id_Table (Fields (Position)).name_with_case), severity => minor);
          end if;
        end loop;
        return new Default_Value'(Composite_Default, Components);
      end;
    end Parse_Record_Default;

    function Parse_Static_Composite_Value
      (Composite_Expected : Exact_Subtyp) return Default_Value_Access
    is
    begin
      In_Symbol;  --  Consume '('.
      declare
        Result : constant Default_Value_Access :=
          (case Composite_Typ (Composite_Expected.TYP) is
             when Arrays  => Parse_Array_Default (Composite_Expected),
             when Records => Parse_Record_Default (Composite_Expected));
      begin
        Need (CD, RParent, err_closing_parenthesis_missing);
        return Result;
      end;
    end Parse_Static_Composite_Value;

  begin
    if Expected.TYP in Composite_Typ then
      if CD.Sy /= LParent then
        Error_then_Skip
          (CD, FSys, err_default_value_must_be_static,
           "a composite component's default must be an aggregate literal");
        return null;
      end if;
      return Parse_Static_Composite_Value (Expected);
    else
      return Parse_Static_Scalar_Value (Expected);
    end if;
  end Parse_Static_Default_Value;

  procedure Emit_Default_Value
    (CD         : in out Co_Defs.Compiler_Data;
     Dest_Level :        Defs.Nesting_Level;
     Dest_Base  :        Defs.HAC_Integer;
     Value      :        Co_Defs.Default_Value_Access)
  is
  begin
    if Value = null then
      return;
    end if;
    case Value.Kind is
      when Scalar_Default =>
        Emit_2 (CD, k_Push_Address, Operand_1_Type (Dest_Level), Operand_2_Type (Dest_Base));
        if Value.Scalar_Typ.TYP = Floats then
          Emit_Push_Float_Literal (CD, Value.Scalar_R);
        else
          CD.target.Emit_Push_Discrete_Literal (Value.Scalar_Int);
        end if;
        Emit_1 (CD, k_Store, Typen'Pos (Value.Scalar_Typ.TYP));
      when Composite_Default =>
        for Component of Value.Components loop
          Emit_Default_Value (CD, Dest_Level, Dest_Base + Component.Offset, Component.Value);
        end loop;
    end case;
  end Emit_Default_Value;

  function Inherited_Default
    (CD  : in out Co_Defs.Compiler_Data;
     Typ :        Co_Defs.Exact_Subtyp) return Co_Defs.Default_Value_Access
  is
  begin
    case Typ.TYP is
      when Records =>
        return CD.Blocks_Table (Typ.Ref).Default;
      when Arrays =>
        declare
          Array_Entry     : Array_Table_Entry renames CD.Arrays_Table (Typ.Ref);
          Element_Default : constant Default_Value_Access :=
            Inherited_Default (CD, Array_Entry.Element_xTyp);
        begin
          if Element_Default = null then
            return null;
          end if;
          declare
            Lower_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_First;
            Upper_Bound  : constant HAC_Integer := Array_Entry.Index_xTyp.Discrete_Last;
            Element_Size : constant HAC_Integer := HAC_Integer (Array_Entry.Element_Size);
            Components   : Default_Component_Vectors.Vector;
          begin
            for Array_Index in Lower_Bound .. Upper_Bound loop
              Components.Append
                (((Array_Index - Lower_Bound) * Element_Size, Element_Default));
            end loop;
            return new Default_Value'(Composite_Default, Components);
          end;
        end;
      when others =>
        return null;
    end case;
  end Inherited_Default;

end HAC_Sys.Parser.Defaults;
