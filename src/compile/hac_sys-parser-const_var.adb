with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Statements,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Const_Var is

  procedure Var_Declaration
    (CD         : in out Co_Defs.Compiler_Data;
     FSys       :        Defs.Symset;
     Block_Data : in out Block_Data_Type)
  is
    use Compiler.PCode_Emit, Co_Defs, Defs, Enter_Def, Helpers, PCode, Errors;
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;

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
        Statements.Assignment (CD, FSys, Block_Data.level, id_last, Check_read_only => False);
        --  Id_Last has been assigned.
        --  Now, we copy the value of id_last to id_first .. id_last - 1.
        --  In the above example:  "a := c"  and  "b := c".
        for Var of CD.IdTab (id_first .. id_last - 1) loop
          --  Push destination address:
          Emit_2 (CD, k_Push_Address, Var.lev, Operand_2_Type (Var.adr_or_sz));
          if var_typ.TYP in Composite_Typ then
            --  Push source address:
            Emit_2 (CD, k_Push_Address, CD.IdTab (id_last).lev,
              Operand_2_Type (CD.IdTab (id_last).adr_or_sz)
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
              CD.IdTab (id_last).lev,
              Operand_2_Type (CD.IdTab (id_last).adr_or_sz)
            );
            Emit_1 (CD, k_Store, Typen'Pos (var_typ.TYP));
          end if;
        end loop;
      else
        --  Implicit initialization (for instance, VString's and File_Type's).
        for Var of CD.IdTab (id_first .. id_last) loop
          if Auto_Init_Typ (Var.xtyp.TYP) then
            Emit_2 (CD, k_Push_Address, Var.lev, Operand_2_Type (Var.adr_or_sz));
            Emit_1 (CD, k_Variable_Initialization, Typen'Pos (Var.xtyp.TYP));
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
      Block_Data.initialization_object_code_size := Block_Data.initialization_object_code_size + (LC1 - LC0);  --  Size of initialization ObjCode
      if LC0 + Block_Data.initialization_object_code_size >= CD.CMax - Block_Data.initialization_object_code_size then
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
      T0, T1, Sz, T0i                            : Integer;
      xTyp                                       : Exact_Subtyp;
      is_constant, is_typed, is_untyped_constant : Boolean;
      C                                          : Constant_Rec;
      Dummy_First, Dummy_Last                    : HAC_Integer;
    begin
      T0 := CD.Id_Count;
      Enter_Variables (CD, Block_Data.level, True);
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
        Type_Def.Type_Definition (CD, Block_Data.level, Becomes_Comma_IDent_Semicolon + FSys, xTyp, Sz);
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
          Expressions.Static_Scalar_Expression (CD, Block_Data.level, Comma_IDent_Semicolon + FSys, C);
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
            r.read_only := is_constant;
            if is_untyped_constant then
              r.entity := Declared_Number_or_Enum_Item;  --  r was initially a Variable.
              r.xtyp := C.TP;
              case C.TP.TYP is
                when Floats =>
                  Enter_or_find_Float (CD, C.R, r.adr_or_sz);
                when Ints =>
                  r.adr_or_sz := Integer (C.I);
                when others =>
                  Error (CD, err_numeric_constant_expected);
                  --  "boo : constant := True;" or "x: constant := 'a';" are wrong in Ada.
                  r.adr_or_sz := Integer (C.I);
              end case;
            else  --  A variable or a typed constant
              r.xtyp      := xTyp;
              r.adr_or_sz := Block_Data.data_allocation_index;
              Block_Data.data_allocation_index := Block_Data.data_allocation_index + Sz;
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
      Need_Semicolon_after_Declaration (CD, FSys);
    end Single_Var_Declaration;
  begin
    while CD.Sy = IDent loop
      Single_Var_Declaration;
    end loop;
  end Var_Declaration;

end HAC_Sys.Parser.Const_Var;
