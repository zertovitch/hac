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

with HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

with HAT;

with Ada.Characters.Handling;

package body HAC_Sys.Parser.Type_Def is

  use Co_Defs, Defs, Enter_Def, Helpers, Errors;
  use type HAC_Integer;

  procedure Type_or_Subtype_Declaration
    (CD         : in out Co_Defs.Compiler_Data;
     Level      : in     Defs.Nesting_Level;
     FSys_NTD   : in     Defs.Symset)
  is
    T1 : Integer;
    forward_id_idx : Natural;
    is_subtype : constant Boolean := CD.Sy = SUBTYPE_Symbol;
  begin
    Scanner.In_Symbol (CD);  --  Consume TYPE or SUBTYPE symbol.
    Test (CD, IDent_Set, Semicolon_Set, err_identifier_missing);
    Enter_Prefixed (CD, Level, CD.Id, CD.Id_with_case, type_mark, forward_id_idx);
    T1 := CD.Id_Count;
    Scanner.In_Symbol (CD);
    Need (CD, IS_Symbol, err_IS_missing);
    declare
      New_T : IdTabEntry renames CD.IdTab (T1);
    begin
      if is_subtype then
        Subtype_Indication
          (CD, Level,
           FSys_TD => Comma_IDent_Semicolon + FSys_NTD,
           xTP     => New_T.xtyp,
           Size    => Integer (New_T.adr_or_sz));
      else
        Type_Definition
          (CD, Level,
           FSys_TD => Comma_IDent_Semicolon + FSys_NTD,
           xTP     => New_T.xtyp,
           Size    => Integer (New_T.adr_or_sz));
      end if;
    end;
    --
    Need_Semicolon_after_Declaration (CD, FSys_NTD);
  end Type_or_Subtype_Declaration;

  log_max_index : constant HAT.Real := HAT.Log (HAT.Real (Index'Last));

  --  constrained_array_definition 3.6 (5) : "array (1 .. 2, 3 .. 4) of Integer;"
  --  index_constraint 3.6.1 (2)           : "Matrix (1 .. 2, 3 .. 4);"
  --
  procedure Array_Typ
    (CD            : in out Co_Defs.Compiler_Data;
     Level : in     Defs.Nesting_Level;
     FSys_TD       : in     Defs.Symset;
     arr_tab_ref, arr_size, arr_dimensions : out Integer;
     string_constrained_subtype            :     Boolean)
  is
    Element_Exact_Subtyp, Index_Exact_Subtyp : Exact_Subtyp;
    Element_Size                             : Integer;
    recursive_dimensions                     : Natural := 0;
    Lower_Bound, Higher_Bound                : Constant_Rec;
    new_dim_size                             : HAC_Integer;
    use HAT, Ranges;
  begin
    Static_Range (CD, Level, FSys_TD, err_illegal_array_bounds, Lower_Bound, Higher_Bound);
    Index_Exact_Subtyp := Lower_Bound.TP;
    Index_Exact_Subtyp.Discrete_First := Lower_Bound.I;
    Index_Exact_Subtyp.Discrete_Last  := Higher_Bound.I;
    Enter_Array (CD, Index_Exact_Subtyp);
    arr_tab_ref := CD.Arrays_Count;
    if string_constrained_subtype then
      --  We define String (L .. H) exactly as an "array (L .. H) of Character".
      Construct_Root (Element_Exact_Subtyp, Chars);
      Element_Exact_Subtyp.Discrete_First := 0;
      Element_Exact_Subtyp.Discrete_Last  := 255;
      Element_Size := 1;
      Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
    elsif CD.Sy = Comma then
      --  Multidimensional array is equivalant to:  array (range_1) of array (range_2,...).
      Scanner.In_Symbol (CD);  --  Consume ',' symbol.
      Construct_Root (Element_Exact_Subtyp, Arrays);  --  Recursion for next array dimension.
      Array_Typ (CD, Level, FSys_TD, Element_Exact_Subtyp.Ref, Element_Size, recursive_dimensions, False);
    else
      Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
      Need (CD, OF_Symbol, err_missing_OF);         --  "of"  in  "array (...) of Some_Type"
      if Type_Begin_Symbol (CD.Sy) then
        Error (CD, err_general_error, "anonymous definition not permitted here");
        --  Recovery:
        Type_Definition (CD, Level, FSys_TD, Element_Exact_Subtyp, Element_Size);
      else
        --  RM 3.6 (2)
        --  Here is a component_definition, which is a
        --  subtype indication (possibly aliased).
        Subtype_Indication (CD, Level, FSys_TD, Element_Exact_Subtyp, Element_Size);
      end if;
    end if;
    if Element_Size = 0 then
      --  Happens when the element type is undefined (source is already in error) or
      --  if the element type is an empty array type.
      arr_size := 0;
    else
      new_dim_size := Higher_Bound.I - Lower_Bound.I + 1;
      if new_dim_size <= 0 then
        --  This dimension is empty.
        arr_size := 0;
      elsif Log (Real (new_dim_size)) + Log (Real (Element_Size)) < log_max_index then
        arr_size := Integer (new_dim_size) * Element_Size;
      else
        Error
          (CD,
           err_illegal_array_bounds,
           "array is too large (more than" & Defs.Index'Last'Image & " elements)");
        arr_size := 0;
      end if;
    end if;
    arr_dimensions := 1 + recursive_dimensions;
    declare
      New_A : ATabEntry renames CD.Arrays_Table (arr_tab_ref);
    begin
      --  New_A.Index_xTyp already set by Enter_Array.
      New_A.Array_Size   := arr_size;
      New_A.Element_xTyp := Element_Exact_Subtyp;
      New_A.Element_Size := Element_Size;
      New_A.dimensions   := arr_dimensions;
    end;
  end Array_Typ;

  procedure Type_Definition
    (CD            : in out Co_Defs.Compiler_Data;
     Initial_Level : in     Defs.Nesting_Level;
     FSys_TD       : in     Defs.Symset;
     xTP           :    out Co_Defs.Exact_Subtyp;
     Size          :    out Integer)
  is
    Level : Nesting_Level := Initial_Level;
    procedure In_Symbol is begin Scanner.In_Symbol (CD); end In_Symbol;

    procedure Enumeration_Typ is  --  RM 3.5.1 Enumeration Types
      enum_count : Natural := 0;
      forward_id_idx : Natural;
    begin
      xTP.Construct_Root (Enums);
      xTP.Ref := CD.Id_Count;
      loop
        In_Symbol;  --  Consume '(' symbol.
        if CD.Sy = IDent then
          enum_count := enum_count + 1;
          Enter_Prefixed (CD, Level, CD.Id, CD.Id_with_case, declared_number_or_enum_item, forward_id_idx);
          declare
            new_enum_item : IdTabEntry renames CD.IdTab (CD.Id_Count);
          begin
            new_enum_item.xtyp          := xTP;
            new_enum_item.adr_or_sz     := HAC_Integer (enum_count - 1);  --  RM 3.5.1 (7): position begins with 0.
            new_enum_item.is_referenced := False;
          end;
        else
          Error (CD, err_identifier_missing);
        end if;
        In_Symbol;
        exit when CD.Sy /= Comma;
      end loop;
      Size  := 1;
      xTP.Discrete_First := 0;
      xTP.Discrete_Last  := HAC_Integer (enum_count - 1);
      Need (CD, RParent, err_closing_parenthesis_missing);
    end Enumeration_Typ;

    procedure Record_Typ is  --  RM 3.8
      Field_Exact_Subtyp : Exact_Subtyp;
      Field_Size, Offset, T0, T1 : Integer;
    begin
      In_Symbol;  --  Consume RECORD symbol.
      Enter_Block (CD, CD.Id_Count);
      Construct_Root (xTP, Records);
      xTP.Ref := CD.Blocks_Count;
      if Level = nesting_level_max then
        Fatal (LEVELS);  --  Exception is raised there.
      end if;
      Level              := Level + 1;
      CD.Display (Level) := CD.Blocks_Count;
      Offset             := 0;
      --
      loop
        if CD.Sy = IDent then
          --  RM 3.8: Record component declaration
          T0 := CD.Id_Count;
          Enter_Variables (CD, Level, False);
          Need (CD, Colon, err_colon_missing);  --  ':'  in  "a, b, c : Integer;"
          T1 := CD.Id_Count;
          if Type_Begin_Symbol (CD.Sy) then
            Error (CD, err_general_error, "anonymous definition not permitted here");
            --  Recovery:
            Type_Definition
              (CD, Level, FSys_TD + Comma_END_IDent_Semicolon,
               Field_Exact_Subtyp, Field_Size);
          else
            --  RM 3.6 (2)
            --  Here is a component_definition, which is a
            --  subtype indication (possibly aliased).
            Subtype_Indication
              (CD, Level, FSys_TD + Comma_END_IDent_Semicolon,
               Field_Exact_Subtyp, Field_Size);
          end if;
          while T0 < T1 loop
            T0                      := T0 + 1;
            CD.IdTab (T0).xtyp      := Field_Exact_Subtyp;
            CD.IdTab (T0).adr_or_sz := HAC_Integer (Offset);
            Offset                  := Offset + Field_Size;
          end loop;
        else
          Error (CD, err_identifier_missing, severity => major);
        end if;
        Need_Semicolon (CD);
        exit when CD.Sy = END_Symbol;
      end loop;
      --
      CD.Blocks_Table (xTP.Ref).VSize := Offset;
      Size                            := Offset;
      CD.Blocks_Table (xTP.Ref).PSize := 0;
      In_Symbol;
      Need (CD, RECORD_Symbol, err_RECORD_missing);  --  (END) RECORD
      Level := Level - 1;
    end Record_Typ;

    dummy_dims : Natural;
  begin
    xTP  := Undefined;
    Size := 0;
    if CD.Sy in
      ABSTRACT_Symbol | ACCESS_Symbol |
      DIGITS_Symbol | DELTA_Symbol |
      INTERFACE_Symbol | LIMITED_Symbol |
      NOT_Symbol | NEW_Symbol |
      PRIVATE_Symbol | PROTECTED_Symbol |
      RANGE_Keyword_Symbol | SYNCHRONIZED_Symbol |
      TASK_Symbol | TAGGED_Symbol
    then
      Error
        (CD, err_not_yet_implemented,
         ": type definitions starting with """ &
         Ada.Characters.Handling.To_Lower (A2S (CD.Id)) & '"',
         severity => major);
    end if;
    Test (CD, Type_Begin_Symbol, FSys_TD, err_missing_type_begin_symbol);
    if Type_Begin_Symbol (CD.Sy) then
      case CD.Sy is
        when ARRAY_Symbol =>
          In_Symbol;
          Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
          Construct_Root (xTP, Arrays);
          Array_Typ (CD, Level, FSys_TD, xTP.Ref, Size, dummy_dims, string_constrained_subtype => False);
        when RECORD_Symbol =>
          Record_Typ;
        when LParent =>
          Enumeration_Typ;
        when others =>
          null;
      end case;
      Test (CD, FSys_TD, empty_symset, err_incorrectly_used_symbol);
    end if;
    if CD.error_count = 0 then
      pragma Assert (Level = Initial_Level);
    end if;
  end Type_Definition;

  procedure Subtype_Indication
    (CD      : in out Co_Defs.Compiler_Data;
     Level   : in     Defs.Nesting_Level;
     FSys_TD : in     Defs.Symset;
     xTP     :    out Co_Defs.Exact_Subtyp;
     Size    :    out Integer)
  is
    procedure In_Symbol is begin Scanner.In_Symbol (CD); end In_Symbol;

    dummy_dims : Natural;

    procedure String_Sub_Typ is
      --  Prototype of constraining an array type: String -> String (1 .. 26)
      --  We need to implement general constraints one day...
    begin
      In_Symbol;
      Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
      Construct_Root (xTP, Arrays);
      Array_Typ (CD, Level, FSys_TD, xTP.Ref, Size, dummy_dims, string_constrained_subtype => True);
    end String_Sub_Typ;

    Ident_Index : Integer;

    --  Here we are sitting, say, on `Character` in `subtype My_Chars is Character` [range 'a' .. 'z']
    --
    procedure Sub_Typ is
      Low, High : Constant_Rec;
    begin
      if Ident_Index = No_Id then
        return;  --  Error already issued due to undefined identifier
      end if;
      declare
        Id_T : IdTabEntry renames CD.IdTab (Ident_Index);
      begin
        if Id_T.entity = type_mark then
          xTP   := Id_T.xtyp;
          Size  := Integer (Id_T.adr_or_sz);
          if xTP.TYP = NOTYP then
            Error (CD, err_undefined_type);
          end if;
        else
          Error (CD, err_missing_a_type_identifier);
        end if;
      end;
      In_Symbol;
      if CD.Sy = RANGE_Keyword_Symbol then
        --  Here comes the optional `  range 'a' .. 'z'  ` constraint.
        In_Symbol;
        Ranges.Explicit_Static_Range (CD, Level, FSys_TD, err_range_constraint_error, Low, High);
        if Exact_Typ (Low.TP) /= Exact_Typ (xTP) then
          Error
            (CD, err_range_constraint_error,
             "type of bounds don't match with the parent type", severity => major);
        elsif Low.I not in xTP.Discrete_First .. xTP.Discrete_Last then
          Error
            (CD,
             err_range_constraint_error,
             "lower bound, " & Discrete_Image (CD, Low.I, xTP.TYP, xTP.Ref) &
             ", is out of parent type's range, " &
             Discrete_Range_Image (CD, xTP.Discrete_First, xTP.Discrete_Last, xTP.TYP, xTP.Ref),
             severity => major);
        elsif High.I not in xTP.Discrete_First .. xTP.Discrete_Last then
          Error
            (CD,
             err_range_constraint_error,
             "higher bound, " & Discrete_Image (CD, High.I, xTP.TYP, xTP.Ref) &
             ", is out of parent type's range, " &
             Discrete_Range_Image (CD, xTP.Discrete_First, xTP.Discrete_Last, xTP.TYP, xTP.Ref),
             severity => major);
        else
          xTP.Discrete_First := Low.I;
          xTP.Discrete_Last  := High.I;
        end if;
      end if;
    end Sub_Typ;

  begin
    xTP  := Undefined;
    Size := 0;
    if CD.Sy = NOT_Symbol then
      Error
        (CD, err_not_yet_implemented,
         ": subtype indications starting with """ &
         Ada.Characters.Handling.To_Lower (A2S (CD.Id)) & '"',
         severity => major);
    end if;
    Test (CD, Subtype_Begin_Symbol, FSys_TD, err_identifier_missing);
    if CD.Sy /= IDent then
      --  Normally this case should have been filtered out before.
      Error (CD, err_general_error, severity => major);
    end if;
    Ident_Index := Locate_CD_Id (CD, Level);
    if Ident_Index = CD.String_Id_Index then
      String_Sub_Typ;
    else
      Sub_Typ;
    end if;
    Test (CD, FSys_TD, empty_symset, err_incorrectly_used_symbol);
  end Subtype_Indication;

end HAC_Sys.Parser.Type_Def;
