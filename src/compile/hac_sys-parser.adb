with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Modularity,
     HAC_Sys.Parser.Statements,
     HAC_Sys.Parser.Tasking,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

with HAL;

package body HAC_Sys.Parser is

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset;
    Is_a_block_statement :        Boolean;        --  5.6 Block Statements
    Initial_Block_Data   :        Block_Data_Type;
    Block_Id             :        Defs.Alfa;      --  Name of this block (if any)
    Block_Id_with_case   :        Defs.Alfa
  )
  is
    use Co_Defs, Compiler, Defs, Enter_Def,
        Helpers, PCode, Compiler.PCode_Emit,
        UErrors;
    use type Nesting_level;
    --
    block_data : Block_Data_Type := Initial_Block_Data;
    subprogram_block_index          : Integer;  --  Was: PRB
    initialization_object_code_size : Integer;  --  Was: ICode

    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;

    ------------------------------------------------------------------
    --------------------------------------------Formal_Parameter_List-
    procedure Formal_Parameter_List is
      Sz, X, T0 : Integer;
      ValParam  : Boolean;
      xTP       : Exact_Subtyp := Undefined;
    begin
      InSymbol;  --  Consume '(' symbol.
      Sz := 0;
      Test (CD, IDent_Set, FSys + RParent, err_identifier_missing, stop_on_error => True);
      --
      while CD.Sy = IDent loop
        T0 := CD.Id_Count;
        Enter_Variables (CD, block_data.level);
        --
        if CD.Sy = Colon then  --  The ':'  in  "function F (x, y : in Real) return Real;"
          InSymbol;
          if CD.Sy = IN_Symbol then
            InSymbol;
          end if;
          if block_data.is_a_function then  --  If I am a function, no InOut parms allowed
            ValParam := True;
          elsif CD.Sy /= OUT_Symbol then
            ValParam := True;
          else
            InSymbol;
            ValParam := False;
          end if;
          if CD.Sy = IDent then
            X := Locate_Identifier (CD, CD.Id, block_data.level);
            InSymbol;
            if X = CD.String_Id_Index then
              --  We could pass string literals as "in" parameter
              --  if we replaced String_Literals by a record wrapping
              --  the string length and the index into the string table.
              Error (CD, err_string_not_supported_as_parameter, severity => major);
            elsif X /= No_Id then
              if CD.IdTab (X).Entity = TypeMark then
                xTP := CD.IdTab (X).xTyp;
                if ValParam then
                  Sz := CD.IdTab (X).Adr_or_Sz;
                else
                  Sz := 1;
                end if;
              else
                Error (CD, err_missing_a_type_identifier, severity => major);
              end if;
            end if;  --  X /= No_Id
          else
            Error (CD, err_identifier_missing);
          end if;
          Test (CD, Comma_IDent_RParent_Semicolon, FSys, err_semicolon_missing, stop_on_error => True);
          while T0 < CD.Id_Count loop
            T0 := T0 + 1;
            declare
              r : IdTabEntry renames CD.IdTab (T0);
            begin
              r.xTyp      := xTP;
              r.Normal    := ValParam;
              r.Read_only := ValParam;
              r.Adr_or_Sz := block_data.data_allocation_index;
              r.LEV       := block_data.level;
              block_data.data_allocation_index := block_data.data_allocation_index + Sz;
            end;
          end loop;  --  while T0 < CD.Id_Count
        else
          Error (CD, err_colon_missing, severity => major);
        end if;
        if CD.Sy /= RParent then
          Need_Semicolon (CD);
          Test (CD, IDent_Set, FSys + RParent, err_incorrectly_used_symbol);
        end if;
      end loop;  --  while Sy = IDent
      --
      if CD.Sy = RParent then
        InSymbol;
        Test (CD, After_Subprogram_Parameters, FSys, err_incorrectly_used_symbol);
      else
        Error (CD, err_closing_parenthesis_missing);
      end if;
    end Formal_Parameter_List;

    ------------------------------------------------------------------
    --------------------------------------------------Var_Declaration-
    procedure Var_Declaration is
      --  This procedure processes both Variable and Constant declarations.
      procedure Initialized_Constant_or_Variable (
        explicit          : Boolean;
        id_first, id_last : Integer;
        var_typ           : Exact_Subtyp
      )
      is
        LC0 : Integer :=  CD.LC;
        LC1 : Integer;
      begin
        --  Create constant or variable initialization ObjCode
        --  The new variables Id's are in the range id_first .. id_last.
        if explicit then
          --  We do an assignment to the last one.
          --  Example:
          --     for:            "a, b, c : Real := F (x);"
          --     we do first:    "c := F (x)".
          Statements.Assignment (CD, FSys, block_data.level, id_last, Check_read_only => False);
          --  Id_Last has been assigned.
          --  Now, we copy the value of id_last to id_first .. id_last - 1.
          --  In the above example:  "a := c"  and  "b := c".
          for Var of CD.IdTab (id_first .. id_last - 1) loop
            --  Push destination address:
            Emit_2 (CD, k_Push_Address, Var.LEV, Operand_2_Type (Var.Adr_or_Sz));
            if var_typ.TYP in Composite_Typ then
              --  Push source address:
              Emit_2 (CD, k_Push_Address, CD.IdTab (id_last).LEV,
                Operand_2_Type (CD.IdTab (id_last).Adr_or_Sz)
              );
              case Composite_Typ (var_typ.TYP) is
                when Arrays =>
                  Emit_1 (CD, k_Copy_Block,
                    Operand_2_Type (CD.Arrays_Table (var_typ.Ref).Array_Size)
                  );
                when Records =>
                  Emit_1 (CD, k_Copy_Block,
                    Operand_2_Type (CD.Blocks_Table (var_typ.Ref).VSize)
                  );
              end case;
            else
              --  Non-composite type. We copy the value.
              Emit_2 (CD, k_Push_Value,
                CD.IdTab (id_last).LEV,
                Operand_2_Type (CD.IdTab (id_last).Adr_or_Sz)
              );
              Emit (CD, k_Store);
            end if;
          end loop;
        else
          --  Implicit initialization (for instance, VString's and File_Type's).
          for Var of CD.IdTab (id_first .. id_last) loop
            if Auto_Init_Typ (Var.xTyp.TYP) then
              Emit_2 (CD, k_Push_Address, Var.LEV, Operand_2_Type (Var.Adr_or_Sz));
              Emit_1 (CD, k_Variable_Initialization, Typen'Pos (Var.xTyp.TYP));
            end if;
            --  !!  TBD: Must handle composite types (arrays or records) containing
            --           initialized types, too...
          end loop;
        end if;
        --
        LC1 := CD.LC;
        --  Reset ObjCode pointer as if ObjCode had not been generated
        CD.LC := LC0;
        --  Copy ObjCode to end of ObjCode table in reverse order.
        --
        --  This buffering is needed for having the initialization code placed
        --  right after the "BEGIN" of current block (see Statements_Part_Setup).
        --  Nested subprograms have their own code and their eventual own
        --  initialization code coming before in the object code table.
        initialization_object_code_size := initialization_object_code_size + (LC1 - LC0);  --  Size of initialization ObjCode
        if LC0 + initialization_object_code_size >= CD.CMax - initialization_object_code_size then
          Fatal (Object_Code);  --  Collision during the copy (loop below). Garbage guaranteed.
        end if;
        while LC0 < LC1 loop
          CD.ObjCode (CD.CMax) := CD.ObjCode (LC0);
          CD.CMax              := CD.CMax - 1;
          LC0                  := LC0 + 1;
        end loop;
      end Initialized_Constant_or_Variable;
      --
      procedure Single_Var_Declaration is
        T0, T1, Sz, T0i           : Integer;
        xTyp                      : Exact_Subtyp;
        is_constant, is_typed,
        is_untyped_constant       : Boolean;
        C                         : Constant_Rec;
        Dummy_First               : HAC_Integer;
        Dummy_Last                : HAC_Integer;
      begin
        T0 := CD.Id_Count;
        Enter_Variables (CD, block_data.level);
        Need (CD, Colon, err_colon_missing);  --  ':'   in   "x, y : Integer;"
        T1 := CD.Id_Count;
        --
        Test (CD, Type_Begin_Symbol + CONSTANT_Symbol, Semicolon_Set, err_incorrectly_used_symbol);
        --
        is_constant := False;
        if CD.Sy = CONSTANT_Symbol then  --  Consume "constant" in "x : constant ...;"
          is_constant := True;
          InSymbol;
        end if;
        --
        is_typed := False;
        if Type_Begin_Symbol (CD.Sy) then  --  Here, a type name or an anonymous type definition
          is_typed := True;
          Type_Def.Type_Definition (CD, block_data.level, Becomes_Comma_IDent_Semicolon + FSys, xTyp, Sz);
        end if;
        Test (CD, Becomes_EQL_Semicolon, Empty_Symset, err_incorrectly_used_symbol);
        --
        if CD.Sy = EQL then
          --  Common mistake by BASIC or C programmers.
          Error (CD, err_EQUALS_instead_of_BECOMES);
          CD.Sy := Becomes;
        end if;
        --
        is_untyped_constant := is_constant and not is_typed;
        --
        if is_untyped_constant then
          --  Numeric constant: we parse the number here ("k : constant := 123.0").
          if CD.Sy = Becomes then
            InSymbol;
            Type_Def.Number_Declaration_or_Enum_Item_or_Literal_Char (CD, block_data.level, Comma_IDent_Semicolon + FSys, C);
          else
            Error (CD, err_BECOMES_missing);
          end if;
        end if;
        --
        T0i := T0;
        if is_constant or is_typed then  --  All correct cases: untyped variables were caught.
          --  Update identifier table
          while T0 < T1 loop
            T0 := T0 + 1;
            declare
              r : IdTabEntry renames CD.IdTab (T0);
            begin
              r.Read_only := is_constant;
              if is_untyped_constant then
                r.Entity := Declared_Number_or_Enum_Item;  --  r was initially a Variable.
                Exact_Typ (r.xTyp) := C.TP;
                case C.TP.TYP is
                  when Floats =>
                    Enter_or_find_Float (CD, C.R, r.Adr_or_Sz);
                  when Ints =>
                    r.Adr_or_Sz := Integer (C.I);
                  when others =>
                    Error (CD, err_numeric_constant_expected);
                    --  "boo : constant := True;" or "x: constant := 'a';" are wrong in Ada.
                    r.Adr_or_Sz := Integer (C.I);
                end case;
              else  --  A variable or a typed constant
                r.xTyp      := xTyp;
                r.Adr_or_Sz := block_data.data_allocation_index;
                block_data.data_allocation_index := block_data.data_allocation_index + Sz;
              end if;
            end;
          end loop;  --  While T0 < T1
        end if;
        --
        if CD.Sy = EQL and not is_untyped_constant then
          Error (CD, err_EQUALS_instead_of_BECOMES);
          CD.Sy := Becomes;
        end if;
        if is_constant and is_typed then
          --  For typed constants, the ":=" is required and consumed with the Assignment below.
          Test (CD, Becomes_Set, Empty_Symset, err_BECOMES_missing);
        end if;
        --
        if not is_untyped_constant then
          Initialized_Constant_or_Variable (
            explicit => CD.Sy = Becomes,
            id_first => T0i + 1,
            id_last  => T1,
            var_typ  => xTyp
          );
        end if;
        Test_Semicolon_in_Declaration (CD, FSys);
      end Single_Var_Declaration;
    begin
      while CD.Sy = IDent loop
        Single_Var_Declaration;
      end loop;
    end Var_Declaration;

    ------------------------------------------------------------------
    ----------------Subprogram_Declaration_or_Body - Ada RM 6.1, 6.3--
    procedure Subprogram_Declaration_or_Body is
      IsFun : constant Boolean := CD.Sy = FUNCTION_Symbol;
      sub_sub_prog_block_data : Block_Data_Type;
    begin
      InSymbol;
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
        CD.Id := Empty_Alfa;
      end if;
      declare
        Id_subprog_with_case : constant Alfa := CD.Id_with_case;
      begin
        if IsFun then
          Enter (CD, block_data.level, CD.Id, Id_subprog_with_case, Funktion);
        else
          Enter (CD, block_data.level, CD.Id, Id_subprog_with_case, Prozedure);
        end if;
        InSymbol;
        sub_sub_prog_block_data.level          := block_data.level + 1;
        sub_sub_prog_block_data.block_id_index := CD.Id_Count;
        sub_sub_prog_block_data.is_a_function  := IsFun;
        Block (CD, FSys, False, sub_sub_prog_block_data,
               CD.IdTab (CD.Id_Count).Name, Id_subprog_with_case);
      end;
      if IsFun then
        Emit_1 (CD, k_Exit_Function, End_Function_without_Return);
      else
        Emit_1 (CD, k_Exit_Call, Normal_Procedure_Call);
      end if;
    end Subprogram_Declaration_or_Body;

    procedure Declarative_Part is
    begin
      loop
        Test (  --  Added 17-Apr-2018 to avoid infinite loop on erroneous code
          CD, Declaration_Symbol + BEGIN_Symbol,
          Empty_Symset,
          err_incorrectly_used_symbol,
          stop_on_error => True  --  Exception is raised there if there is an error.
        );
        case CD.Sy is
          when IDent              => Var_Declaration;
          when TYPE_Symbol |
               SUBTYPE_Symbol     => Type_Def.Type_Declaration (CD, block_data.level, FSys);
          when TASK_Symbol        => Tasking.Task_Declaration (CD, FSys, block_data.level);
          when USE_Symbol         => Modularity.Use_Clause (CD, block_data.level);
          when PROCEDURE_Symbol |
               FUNCTION_Symbol    => Subprogram_Declaration_or_Body;
          when others => null;
        end case;
        CD.Blocks_Table (subprogram_block_index).VSize := block_data.data_allocation_index;
        exit when CD.Sy = BEGIN_Symbol;
      end loop;
    end Declarative_Part;

    procedure Statements_Part_Setup is
    begin
      block_data.max_data_allocation_index := block_data.data_allocation_index;
      CD.IdTab (block_data.block_id_index).Adr_or_Sz := CD.LC;
      --  Copy initialization (elaboration) ObjCode from end of ObjCode table
      for Init_Code_Idx in reverse CD.CMax + 1 .. CD.CMax + initialization_object_code_size loop
        CD.ObjCode (CD.LC) := CD.ObjCode (Init_Code_Idx);
        CD.LC              := CD.LC + 1;
      end loop;
      --  Restore CMax to the initial max.
      --  At nesting level 0, it will be CDMax.
      --  For higher levels, it will be CDMax minus the sum of
      --  current values of ICode for all lower levels.
      CD.CMax := CD.CMax + initialization_object_code_size;
    end Statements_Part_Setup;

    procedure Statements_Part_Closing is
    begin
      CD.Blocks_Table (subprogram_block_index).SrcTo := CD.CUD.line_count;
    end Statements_Part_Closing;

    procedure Function_Result_Profile is
      I_Res_Type : Integer;
    begin
      if CD.Sy = RETURN_Symbol then
        InSymbol;  --  FUNCTION TYPE
        if CD.Sy = IDent then
          I_Res_Type := Locate_Identifier (CD, CD.Id, block_data.level);
          InSymbol;
          if I_Res_Type /= 0 then
            if CD.IdTab (I_Res_Type).Entity /= TypeMark then
              Error (CD, err_missing_a_type_identifier, severity => major);
            elsif Standard_or_Enum_Typ (CD.IdTab (I_Res_Type).xTyp.TYP) then
              CD.IdTab (block_data.block_id_index).xTyp := CD.IdTab (I_Res_Type).xTyp;
            else
              Error (CD, err_bad_result_type_for_a_function, severity => major);
            end if;
          end if;
        else
          Error (CD, err_identifier_missing, severity => major);
        end if;
      else
        Error (CD, err_RETURN_missing, severity => major);
      end if;
    end Function_Result_Profile;

    Restore_Block_ID : constant HAL.VString := CD.Full_Block_Id;
    use HAL;

    procedure Check_ident_after_END is
      full_name : VString;
      --  ^ It can be a library unit name, like: "Parent_1.Child_3.Grandchild_5".
    begin
      pragma Assert (CD.Sy = IDent);
      loop
        full_name := full_name & To_String (CD.Id);
        InSymbol;
        exit when CD.Sy /= Period;
        full_name := full_name & '.';
        InSymbol;
        if CD.Sy /= IDent then
          Error (CD, err_identifier_missing);
        end if;
      end loop;
      if VStr_Pkg.To_String (full_name) /= To_String (Block_Id) then
        Error
          (CD, err_incorrect_block_name,
           hint => To_String (Block_Id_with_case),
           previous_symbol => True,
           --  ^ Ideally we would enclose the whole wrong full name, possibly on several lines.
           --  But it is correct on a single wrong identifier, the most frequent case.
           severity => minor
          );
      end if;
    end Check_ident_after_END;

  begin  --  Block
    if CD.error_count > 0 then
      return;
    end if;
    if CD.Full_Block_Id = Universe then
      CD.Full_Block_Id := HAL.To_VString (To_String (Block_Id_with_case));
    else
      CD.Full_Block_Id := CD.Full_Block_Id & '.' & To_String (Block_Id_with_case);
    end if;
    block_data.data_allocation_index := 5;  --  fixed area of the subprogram activation record.
    initialization_object_code_size := 0;
    if Is_a_block_statement then
      null;  --  We should be here with Sy = BEGIN_Symbol or Sy = DECLARE_Symbol.
    else
      Test (CD, Symbols_after_Subprogram_Identifier, FSys, err_incorrectly_used_symbol);
    end if;
    if CD.IdTab (block_data.block_id_index).Block_Ref > 0 then
      subprogram_block_index := CD.IdTab (block_data.block_id_index).Block_Ref;
    else
      Enter_Block (CD, block_data.block_id_index);
      subprogram_block_index := CD.Blocks_Count;
      CD.IdTab (block_data.block_id_index).Block_Ref := subprogram_block_index;
    end if;
    CD.Display (block_data.level) := subprogram_block_index;
    CD.IdTab (block_data.block_id_index).xTyp := Undefined;
    if CD.Sy = LParent then
      Formal_Parameter_List;
    end if;
    --
    if CD.error_count > 0 then
      return;
    end if;
    --
    CD.Blocks_Table (subprogram_block_index).Last_Param_Id_Idx := CD.Id_Count;
    CD.Blocks_Table (subprogram_block_index).PSize := block_data.data_allocation_index;
    --
    if block_data.is_a_function and not Is_a_block_statement then
      Function_Result_Profile;
    end if;
    --
    if CD.Sy = Semicolon then
      --  End of subprogram specification part ("forward", not yet available
      --  since ';' is blocked as symbol). Body declared later.
      --  Example:
      --  procedure A; procedure B is begin ... A ... end; procedure A is ... B ... end;
      CD.Blocks_Table (subprogram_block_index).VSize := block_data.data_allocation_index;
      CD.IdTab (block_data.block_id_index).Adr_or_Sz := -1;
      --  Address of body TBD (or, we could have an indirect call mechanism).
      return;
    end if;
    --
    if Is_a_block_statement then
      case CD.Sy is
        when DECLARE_Symbol => InSymbol;
        when BEGIN_Symbol   => null;
        when others         => raise Internal_error with "Unexpected " & KeyWSymbol'Image (CD.Sy);
      end case;
    elsif CD.Sy = IS_Symbol then  --  The "IS" in "procedure ABC (param : T_Type) IS"
      InSymbol;
    else
      Error (CD, err_IS_missing);
      return;
    end if;
    --
    if CD.Sy = NULL_Symbol and not Is_a_block_statement then
      --  RM 6.7 Null Procedures (Ada 2005)
      --  E.g.: "procedure Not_Yet_Done (a : Integer) is null;"
      InSymbol;  --  Consume NULL symbol.
      Statements_Part_Setup;
      if block_data.is_a_function then
        Error (CD, err_no_null_functions);  --  There are no null functions: what would be the result?
      else
        null;  --  No statement -> no instruction, like for the NULL statement.
      end if;
      Statements_Part_Closing;
    else
      Declarative_Part;
      InSymbol;  --  Consume BEGIN symbol.
      Statements_Part_Setup;
      Statements.Sequence_of_Statements (CD, END_Set, block_data);
      Statements_Part_Closing;
      --
      if CD.Sy = END_Symbol then
        InSymbol;
      elsif CD.error_count > 0 then
        return;  --  At this point the program is already FUBAR.
      else
        Error (CD, err_END_missing);
        return;
      end if;
      --
      if CD.Sy = IDent then  --  Verify that the name after "end" matches the unit name.
        Check_ident_after_END;
      elsif Is_a_block_statement and Block_Id /= Empty_Alfa then
        --  No identifier after "end", but "end [label]" is required in this case.
        Error (CD, err_incorrect_block_name, hint => To_String (Block_Id_with_case));
      end if;
    end if;
    --
    if CD.Sy /= Semicolon then
      Error (CD, err_semicolon_missing);
      return;
    end if;
    --
    if block_data.level <= 1 or Is_a_block_statement then
      --  Time to count the minor errors as errors.
      CD.error_count := CD.error_count + CD.minor_error_count;
      CD.minor_error_count := 0;
    else
      InSymbol;  --  Consume ';' symbol after END [Subprogram_Id].
      Ignore_Extra_Semicolons (CD);
      --
      --  Now we have either another declaration,
      --  or BEGIN or, if it's a package, END  .
      Test (
        CD, FSys + Declaration_Symbol + BEGIN_Symbol + END_Symbol,
        Empty_Symset,
        err_incorrectly_used_symbol
      );
    end if;
    CD.Full_Block_Id := Restore_Block_ID;
    if CD.error_count = 0 then
      pragma Assert (block_data.level = Initial_Block_Data.level);
    end if;
  end Block;

end HAC_Sys.Parser;
