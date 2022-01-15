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
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Type_Def is

  use Co_Defs, Defs, Enter_Def, Helpers, UErrors;
  use type Nesting_level;

  procedure Number_Declaration_or_Enum_Item_or_Literal_Char (
    CD      : in out Co_Defs.Compiler_Data;
    Level   : in     Defs.Nesting_level;
    FSys_ND : in     Defs.Symset;
    C       :    out Co_Defs.Constant_Rec
  )
  is
    --  This covers number declarations (RM 3.3.2) and enumeration items (RM 3.5.1).
    --  Additionally this compiler does on-the-fly declarations for static values:
    --  bounds in ranges (FOR, ARRAY), and values in CASE statements.
    --  Was: Constant in the Pascal compiler.
    X : Integer;
    Sign : HAC_Integer;
    use type HAC_Float, HAC_Integer;
    signed : Boolean := False;
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;
  begin
    C.TP := Undefined;
    C.I  := 0;
    Test (CD, Constant_Definition_Begin_Symbol, FSys_ND, err_illegal_symbol_for_a_number_declaration);
    if not Constant_Definition_Begin_Symbol (CD.Sy) then
      return;
    end if;
    if CD.Sy = CharCon then  --  Untyped character constant, occurs only in ranges.
      C.TP := Construct_Root (Chars);
      C.I  := CD.INum;
      InSymbol;
    else
      Sign := 1;
      if Plus_Minus (CD.Sy) then
        signed := True;
        if CD.Sy = Minus then
          Sign := -1;
        end if;
        InSymbol;
      end if;
      case CD.Sy is
        when IDent =>
          --  Number defined using another one: "minus_pi : constant := -pi;"
          --  ... or, we have an enumeration item.
          X := Locate_Identifier (CD, CD.Id, Level);
          if X /= 0 then
            if CD.IdTab (X).Entity = Declared_Number_or_Enum_Item then
              C.TP := Exact_Typ (CD.IdTab (X).xTyp);
              if C.TP.TYP = Floats then
                C.R := HAC_Float (Sign) * CD.Float_Constants_Table (CD.IdTab (X).Adr_or_Sz);
              else
                C.I := Sign * HAC_Integer (CD.IdTab (X).Adr_or_Sz);
                if signed and then C.TP.TYP not in Numeric_Typ then
                  Error (CD, err_numeric_constant_expected);
                end if;
              end if;
            else
              Error (CD, err_illegal_constant_or_constant_identifier, severity => major);
            end if;
          end if;  --  X /= 0
          InSymbol;
        when IntCon =>
          C.TP := Construct_Root (Ints);
          C.I  := Sign * CD.INum;
          InSymbol;
        when FloatCon =>
          C.TP := Construct_Root (Floats);
          C.R  := HAC_Float (Sign) * CD.RNum;
          InSymbol;
        when others =>
          Skip (CD, FSys_ND, err_illegal_symbol_for_a_number_declaration);
      end case;
    end if;
    Test (CD, FSys_ND, Empty_Symset, err_incorrectly_used_symbol);
  end Number_Declaration_or_Enum_Item_or_Literal_Char;

  procedure Type_Declaration (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    FSys_NTD : in     Defs.Symset
  )
  is
    T1 : Integer;
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;
  begin
    InSymbol;  --  Consume TYPE or SUBTYPE symbol.
    Test (CD, IDent_Set, Semicolon_Set, err_identifier_missing);
    Enter (CD, Level, CD.Id, CD.Id_with_case, TypeMark);
    T1 := CD.Id_Count;
    InSymbol;
    Need (CD, IS_Symbol, err_IS_missing);
    declare
      New_T : IdTabEntry renames CD.IdTab (T1);
    begin
      Type_Definition (
        CD, Level,
        FSys_TD => Comma_IDent_Semicolon + FSys_NTD,
        xTP     => New_T.xTyp,
        Size    => New_T.Adr_or_Sz
      );
    end;
    --
    Test_Semicolon_in_Declaration (CD, FSys_NTD);
  end Type_Declaration;

  procedure Type_Definition (
    CD            : in out Compiler_Data;
    Initial_Level : in     Defs.Nesting_level;
    FSys_TD       : in     Defs.Symset;
    xTP           :    out Exact_Subtyp;
    Size          :    out Integer
  )
  is
    Level : Nesting_level := Initial_Level;
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;
    --
    --  constrained_array_definition 3.6 (5)
    --
    procedure Array_Typ (
      Arr_Tab_Ref, Arr_Size      : out Integer;
      String_Constrained_Subtype :     Boolean
    )
    is
      Element_Exact_Subtyp : Exact_Subtyp;
      Element_Size         : Integer;
      Lower_Bound          : Constant_Rec;
      Higher_Bound         : Constant_Rec;
      Index_Exact_Subtyp   : Exact_Subtyp;
      use Ranges;
    begin
      Static_Range (CD, Level, FSys_TD, err_illegal_array_bounds, Lower_Bound, Higher_Bound);
      --  !!  Use a exact_subtyp for Static_Range, once exact_subtyp
      --  !!  can also memorize float ranges.
      Exact_Typ (Index_Exact_Subtyp) := Lower_Bound.TP;
      Index_Exact_Subtyp.Discrete_First := Lower_Bound.I;
      Index_Exact_Subtyp.Discrete_Last  := Higher_Bound.I;
      Enter_Array (CD, Index_Exact_Subtyp);
      Arr_Tab_Ref := CD.Arrays_Count;
      if String_Constrained_Subtype then
        --  We define String (L .. H) exactly as an "array (L .. H) of Character".
        Element_Exact_Subtyp := Construct_Root (Chars);
        Element_Exact_Subtyp.Discrete_First := 0;
        Element_Exact_Subtyp.Discrete_Last  := 255;
        Element_Size := 1;
        Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
      elsif CD.Sy = Comma then
        --  Multidimensional array is equivalant to:  array (range_1) of array (range_2,...).
        InSymbol;  --  Consume ',' symbol.
        Element_Exact_Subtyp.TYP := Arrays;  --  Recursion for next array dimension.
        Array_Typ (Element_Exact_Subtyp.Ref, Element_Size, False);
      else
        Need (CD, RParent, err_closing_parenthesis_missing, Forgive => RBrack);
        Need (CD, OF_Symbol, err_missing_OF);         --  "of"  in  "array (...) of Some_Type"
        Type_Definition (CD, Level, FSys_TD, Element_Exact_Subtyp, Element_Size);
      end if;
      Arr_Size := (Integer (Higher_Bound.I) - Integer (Lower_Bound.I) + 1) * Element_Size;
      declare
        New_A : ATabEntry renames CD.Arrays_Table (Arr_Tab_Ref);
      begin
        --  New_A.Index_xTyp already set by Enter_Array.
        New_A.Array_Size   := Arr_Size;
        New_A.Element_xTyp := Element_Exact_Subtyp;
        New_A.Element_Size := Element_Size;
      end;
    end Array_Typ;

    procedure Enumeration_Typ is  --  RM 3.5.1 Enumeration Types
      enum_count : Natural := 0;
    begin
      xTP := Construct_Root (Enums);
      xTP.Ref := CD.Id_Count;
      loop
        InSymbol;  --  Consume '(' symbol.
        if CD.Sy = IDent then
          enum_count := enum_count + 1;
          Enter (CD, Level, CD.Id, CD.Id_with_case, Declared_Number_or_Enum_Item);
          declare
            New_Enum_Item : IdTabEntry renames CD.IdTab (CD.Id_Count);
          begin
            New_Enum_Item.Read_only := True;
            New_Enum_Item.xTyp      := xTP;
            New_Enum_Item.Adr_or_Sz := enum_count - 1;  --  RM 3.5.1 (7): position begins with 0.
          end;
        else
          Error (CD, err_identifier_missing);
        end if;
        InSymbol;
        exit when CD.Sy /= Comma;
      end loop;
      Size  := 1;
      xTP.Discrete_Last := HAC_Integer (enum_count - 1);
      Need (CD, RParent, err_closing_parenthesis_missing);
    end Enumeration_Typ;

    procedure Record_Typ is
      Field_Exact_Subtyp : Exact_Subtyp;
      Field_Size, Offset, T0, T1 : Integer;
      Dummy_First : HAC_Integer;
      Dummy_Last  : HAC_Integer;
    begin
      InSymbol;  --  Consume RECORD symbol.
      Enter_Block (CD, CD.Id_Count);
      xTP := Construct_Root (Records);
      xTP.Ref := CD.Blocks_Count;
      if Level = Nesting_Level_Max then
        Fatal (LEVELS);  --  Exception is raised there.
      end if;
      Level              := Level + 1;
      CD.Display (Level) := CD.Blocks_Count;
      Offset             := 0;
      --
      loop
        if CD.Sy = IDent then
          --  RM 3.8 Component declaration
          T0 := CD.Id_Count;
          Enter_Variables (CD, Level);
          Need (CD, Colon, err_colon_missing);  --  ':'  in  "a, b, c : Integer;"
          T1 := CD.Id_Count;
          Type_Definition (
            CD, Level, FSys_TD + Comma_END_IDent_Semicolon,
            Field_Exact_Subtyp, Field_Size
          );
          while T0 < T1 loop
            T0                      := T0 + 1;
            CD.IdTab (T0).xTyp      := Field_Exact_Subtyp;
            CD.IdTab (T0).Adr_or_Sz := Offset;
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
      InSymbol;
      Need (CD, RECORD_Symbol, err_RECORD_missing);  --  (END) RECORD
      Level := Level - 1;
    end Record_Typ;

    procedure String_Sub_Typ is
      --  Prototype of constraining an array type: String -> String (1 .. 26)
      --  We need to implement general constraints one day...
    begin
      InSymbol;
      Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
      xTP.TYP := Arrays;
      Array_Typ (xTP.Ref, Size, String_Constrained_Subtype => True);
    end String_Sub_Typ;

    Ident_Index : Integer;

    --  Here we are sitting on `Character` in `subtype My_Chars is Character` [range 'a' .. 'z']
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
        if Id_T.Entity = TypeMark then
          xTP   := Id_T.xTyp;
          Size  := Id_T.Adr_or_Sz;
          if xTP.TYP = NOTYP then
            Error (CD, err_undefined_type);
          end if;
        else
          Error (CD, err_missing_a_type_identifier);
        end if;
      end;
      InSymbol;
      if CD.Sy = RANGE_Keyword_Symbol then
        --  Here comes the optional `  range 'a' .. 'z'  ` constraint.
        InSymbol;
        Ranges.Explicit_Static_Range (CD, Level, FSys_TD, err_range_constraint_error, Low, High);
        if Low.TP /= Exact_Typ (xTP) then
          Error (CD, err_range_constraint_error, "type of bounds don't match with the parent type");
        elsif Low.I not in xTP.Discrete_First .. xTP.Discrete_Last then
          Error (CD, err_range_constraint_error, "lower bound is out of parent type's range");
        elsif High.I not in xTP.Discrete_First .. xTP.Discrete_Last then
          Error (CD, err_range_constraint_error, "higher bound is out of parent type's range");
        else
          xTP.Discrete_First := Low.I;
          xTP.Discrete_Last  := High.I;
        end if;
      end if;
    end Sub_Typ;

  begin
    xTP   := Undefined;
    Size  := 0;
    Test (CD, Type_Begin_Symbol, FSys_TD, err_missing_ARRAY_RECORD_or_ident);
    if Type_Begin_Symbol (CD.Sy) then
      case CD.Sy is
        when IDent =>
          Ident_Index := Locate_Identifier (CD, CD.Id, Level);
          if Ident_Index = CD.String_Id_Index then
            String_Sub_Typ;
          else
            Sub_Typ;
          end if;
        when ARRAY_Symbol =>
          InSymbol;
          Need (CD, LParent, err_missing_an_opening_parenthesis, Forgive => LBrack);
          xTP.TYP := Arrays;
          Array_Typ (xTP.Ref, Size, String_Constrained_Subtype => False);
        when RECORD_Symbol =>
          Record_Typ;
        when LParent =>
          Enumeration_Typ;
        when others =>
          null;
      end case;  --  CD.Sy
      Test (CD, FSys_TD, Empty_Symset, err_incorrectly_used_symbol);
    end if;
    if CD.error_count = 0 then
      pragma Assert (Level = Initial_Level);
    end if;
  end Type_Definition;

end HAC_Sys.Parser.Type_Def;
