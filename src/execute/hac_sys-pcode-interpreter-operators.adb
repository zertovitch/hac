with HAC_Sys.PCode.Interpreter.Exceptions;

with HAC_Pack;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Exceptions,
     Ada.Numerics.Float_Random,
     Ada.Strings;

package body HAC_Sys.PCode.Interpreter.Operators is

  procedure Do_Unary_Operator (ND : in out Interpreter_Data) is
    Curr_TCB_Top : Integer renames ND.TCB (ND.CurTask).T;
    X : General_Register renames ND.S (Curr_TCB_Top);
    H1 : Defs.Index;
    use type Defs.HAC_Float, Defs.HAC_Integer;
    I_to_F : Defs.HAC_Float;
  begin
    case Unary_Operator_Opcode (ND.IR.F) is
      when k_Dereference         => X := ND.S (Defs.Index (X.I));  --  "[T] := ([T].I).all"
      when k_NOT_Boolean         => X.I := Boolean'Pos (not Boolean'Val (X.I));
      when k_Unary_MINUS_Integer => X.I := -X.I;
      when k_Unary_MINUS_Float   => X.R := -X.R;
      when k_Integer_to_Float =>
        H1 := Curr_TCB_Top - Defs.Index (ND.IR.Y);
        I_to_F := Defs.HAC_Float (ND.S (H1).I);
        ND.S (H1) := GR_Real (I_to_F);
    end case;
  end Do_Unary_Operator;

  procedure Do_Binary_Operator (ND : in out Interpreter_Data) is
    Curr_TCB_Top : Integer renames ND.TCB (ND.CurTask).T;
    X : General_Register renames ND.S (Curr_TCB_Top - 1);  --  X = [T-1]
    Y : General_Register renames ND.S (Curr_TCB_Top);      --  Y = [T]
    use Defs.VStrings_Pkg, Defs.REF;
    use type Defs.HAC_Float, Defs.HAC_Integer;
  begin
    --  We do  [T] <- ([T-1] operator [T])  and pop later.
    case Binary_Operator_Opcode (ND.IR.F) is
      when k_EQL_Float =>   X.I := Boolean'Pos (X.R =  Y.R);
      when k_NEQ_Float =>   X.I := Boolean'Pos (X.R /= Y.R);
      when k_LSS_Float =>   X.I := Boolean'Pos (X.R <  Y.R);
      when k_LEQ_Float =>   X.I := Boolean'Pos (X.R <= Y.R);
      when k_GTR_Float =>   X.I := Boolean'Pos (X.R >  Y.R);
      when k_GEQ_Float =>   X.I := Boolean'Pos (X.R >= Y.R);
      --
      when k_EQL_Integer => X.I := Boolean'Pos (X.I =  Y.I);
      when k_NEQ_Integer => X.I := Boolean'Pos (X.I /= Y.I);
      when k_LSS_Integer => X.I := Boolean'Pos (X.I <  Y.I);
      when k_LEQ_Integer => X.I := Boolean'Pos (X.I <= Y.I);
      when k_GTR_Integer => X.I := Boolean'Pos (X.I >  Y.I);
      when k_GEQ_Integer => X.I := Boolean'Pos (X.I >= Y.I);
      --
      when k_EQL_VString => X.I := Boolean'Pos (X.V =  Y.V);
      when k_NEQ_VString => X.I := Boolean'Pos (X.V /= Y.V);
      when k_LSS_VString => X.I := Boolean'Pos (X.V <  Y.V);
      when k_LEQ_VString => X.I := Boolean'Pos (X.V <= Y.V);
      when k_GTR_VString => X.I := Boolean'Pos (X.V >  Y.V);
      when k_GEQ_VString => X.I := Boolean'Pos (X.V >= Y.V);
      --
      when k_AND_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) and Boolean'Val (Y.I));
      when k_OR_Boolean  => X.I := Boolean'Pos (Boolean'Val (X.I) or  Boolean'Val (Y.I));
      when k_XOR_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) xor Boolean'Val (Y.I));
      --
      when k_ADD_Integer      => X.I := X.I + Y.I;
      when k_SUBTRACT_Integer => X.I := X.I - Y.I;
      when k_MULT_Integer     => X.I := X.I * Y.I;
      when k_DIV_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0; else X.I := X.I / Y.I; end if;
      when k_MOD_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0; else X.I := X.I mod Y.I; end if;
      when k_REM_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0; else X.I := X.I rem Y.I; end if;
      when k_Power_Integer    => X.I := X.I ** Natural (Y.I);
      --
      when k_ADD_Float           => X.R := X.R + Y.R;
      when k_SUBTRACT_Float      => X.R := X.R - Y.R;
      when k_MULT_Float          => X.R := X.R * Y.R;
      when k_DIV_Float           => X.R := X.R / Y.R;
      when k_Power_Float         => X.R := X.R ** Y.R;
      when k_Power_Float_Integer => X.R := X.R ** Natural (Y.I);
    end case;
    Pop (ND);
  end Do_Binary_Operator;

  procedure Do_SF_Operator (CD : Compiler_Data; ND : in out Interpreter_Data) is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      Top_Item : General_Register renames ND.S (Curr_TCB.T);
      temp : Defs.HAC_Float;
      Idx, Len, From, To : Integer;
      C : Character;
      Code : constant SF_Code := SF_Code'Val (ND.IR.Y);
      use Defs, Defs.VStrings_Pkg, Defs.REF,
          Exceptions,
          Ada.Calendar, Ada.Characters.Handling,
          Ada.Numerics.Float_Random, Ada.Strings;
      use type Defs.HAC_Integer;
    begin
      case Code is
        when SF_Abs_Int   => Top_Item.I := abs (Top_Item.I);
        when SF_Abs_Float => Top_Item.R := abs (Top_Item.R);
        when SF_T_Val =>   --  S'Val : RM 3.5.5 (5)
          if (Top_Item.I < Defs.OrdMinChar) or
            (Top_Item.I > Defs.OrdMaxChar)  --  !! Character range
          then
            raise VM_Out_of_Range;
          end if;
        when SF_T_Pos =>   --  S'Pos : RM 3.5.5 (2)
          null;
        when SF_T_Succ => Top_Item.I := Top_Item.I + 1;  --  S'Succ : RM 3.5 (22)
        when SF_T_Pred => Top_Item.I := Top_Item.I - 1;  --  S'Pred : RM 3.5 (25)
        when SF_in_discrete_Interval =>
          --  SF_in_discrete_Interval (x, a, b) is equivalent to: `x in a .. b`
          Pop (ND, 2);
          --  [T] := [T] in [T+1] .. [T+2]
          ND.S (Curr_TCB.T).I :=
            Boolean'Pos (
              ND.S (Curr_TCB.T).I in ND.S (Curr_TCB.T + 1).I .. ND.S (Curr_TCB.T + 2).I
            );
        when SF_Round_Float_to_Int =>
          Top_Item.I := HAC_Integer (Top_Item.R);
        when SF_Trunc_Float_to_Int =>
          Top_Item.I := HAC_Integer (Defs.HAC_Float'Floor (Top_Item.R));
        when SF_Float_to_Duration =>
          Top_Item := GR_Duration (Duration (Top_Item.R));
        when SF_Duration_to_Float =>
          Top_Item := GR_Real (Defs.HAC_Float (Top_Item.Dur));
        when SF_Int_to_Duration =>
          Top_Item := GR_Duration (Duration (Top_Item.I));
        when SF_Duration_to_Int =>
          Top_Item.I := HAC_Integer (Top_Item.Dur);
        when SF_Sin =>    Top_Item.R := Sin (Top_Item.R);
        when SF_Cos =>    Top_Item.R := Cos (Top_Item.R);
        when SF_Exp =>    Top_Item.R := Exp (Top_Item.R);
        when SF_Log =>    Top_Item.R := Log (Top_Item.R);
        when SF_Sqrt =>   Top_Item.R := Sqrt (Top_Item.R);
        when SF_Arctan => Top_Item.R := Arctan (Top_Item.R);
        when SF_Random_Int =>
          temp := Defs.HAC_Float (Random (ND.Gen)) *
                  Defs.HAC_Float ((Top_Item.I + 1));
          Top_Item.I := HAC_Integer (Defs.HAC_Float'Floor (temp));
        when SF_String_to_VString =>   --  Unary "+", equivalent to the call To_VString (S)
          Idx := Integer (ND.S (Curr_TCB.T).I);      --  Index in the stack
          Len := Integer (ND.IR.X);                  --  Length of string
          ND.S (Curr_TCB.T) := GR_VString (Get_String_from_Stack (ND, Idx, Len));
        when SF_Literal_to_VString =>  --  Unary "+", equivalent to the call To_VString ("abc")
          Pop (ND);
          Len := Integer (ND.S (Curr_TCB.T).I);      --  Length of string
          Idx := Integer (ND.S (Curr_TCB.T + 1).I);  --  Index to string table
          ND.S (Curr_TCB.T) := GR_VString (CD.Strings_Constants_Table (Idx .. Idx + Len - 1));
        when SF_Char_to_VString =>
          --  We create a 1-character temporary String: (1 => Character'Val (...)) and
          --  convert it to a VString
          ND.S (Curr_TCB.T) := GR_VString (To_VString ((1 => Character'Val (ND.S (Curr_TCB.T).I))));
        when SF_Two_VStrings_Concat =>
          Pop (ND);
          --  [T] := [T] & [T+1] :
          ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & ND.S (Curr_TCB.T + 1).V;
        when SF_VString_Char_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & Character'Val (ND.S (Curr_TCB.T + 1).I);
        when SF_Char_VString_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T) :=
            GR_VString (Character'Val (ND.S (Curr_TCB.T).I) & ND.S (Curr_TCB.T + 1).V);
        when SF_LStr_VString_Concat =>
          --  Literal: 2 items, VString: 1 item. Total, 3 items folded into 1 item.
          Pop (ND, 2);
          Len := Integer (ND.S (Curr_TCB.T).I);      --  Length of string
          Idx := Integer (ND.S (Curr_TCB.T + 1).I);  --  Index to string table
          ND.S (Curr_TCB.T) :=
            GR_VString (CD.Strings_Constants_Table (Idx .. Idx + Len - 1) & ND.S (Curr_TCB.T + 2).V);
        when SF_VString_Int_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & To_VString (HAC_Image (ND.S (Curr_TCB.T + 1).I));
        when SF_Int_VString_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T) := GR_VString ((HAC_Image (ND.S (Curr_TCB.T).I)) & ND.S (Curr_TCB.T + 1).V);
        when SF_VString_Float_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & To_VString (HAC_Image (ND.S (Curr_TCB.T + 1).R));
        when SF_Float_VString_Concat =>
          Pop (ND);
          ND.S (Curr_TCB.T) :=
            GR_VString (To_VString (HAC_Image (ND.S (Curr_TCB.T).R)) & ND.S (Curr_TCB.T + 1).V);
        when SF_Element =>
          Pop (ND);
          --  [T] := Element ([T], [T+1]) :
          C := Element (ND.S (Curr_TCB.T).V, Integer (ND.S (Curr_TCB.T + 1).I));
          ND.S (Curr_TCB.T).I := Character'Pos (C);
        when SF_Length =>
          --  [T] := Length ([T]) :
          Len := Length (Top_Item.V);
          --  !! Here: bound checking !!
          Top_Item.I := HAC_Integer (Len);
        when SF_Slice =>
          Pop (ND, 2);
          From := Integer (ND.S (Curr_TCB.T + 1).I);
          To   := Integer (ND.S (Curr_TCB.T + 2).I);
          --  !! Here: bound checking !!
          --  [T] := Slice ([T], [T+1], [T+2]) :
          ND.S (Curr_TCB.T).V := To_VString (Slice (ND.S (Curr_TCB.T).V, From, To));
        when SF_To_Lower_Char =>
          Top_Item.I := Character'Pos (To_Lower (Character'Val (Top_Item.I)));
        when SF_To_Upper_Char =>
          Top_Item.I := Character'Pos (To_Upper (Character'Val (Top_Item.I)));
        when SF_To_Lower_VStr =>
          Top_Item.V := To_VString (To_Lower (Defs.To_String (Top_Item.V)));
        when SF_To_Upper_VStr =>
          Top_Item.V := To_VString (To_Upper (Defs.To_String (Top_Item.V)));
        when SF_Index =>
          Pop (ND);
          --  [T] := Index ([T], [T+1]) :
          ND.S (Curr_TCB.T).I :=
            HAC_Integer (
              VStrings_Pkg.Index (
                ND.S (Curr_TCB.T).V,
                Defs.To_String (ND.S (Curr_TCB.T + 1).V)
              )
            );
        when SF_Index_Backward =>
          Pop (ND);
          --  [T] := Index ([T], [T+1]) :
          ND.S (Curr_TCB.T).I :=
            HAC_Integer (
              VStrings_Pkg.Index (
                ND.S (Curr_TCB.T).V,
                Defs.To_String (ND.S (Curr_TCB.T + 1).V),
                Backward
              )
            );
        when SF_Head =>
          Pop (ND);
          --  [T] := Head ([T], [T+1]) :
          ND.S (Curr_TCB.T).V :=
            VStrings_Pkg.Head (ND.S (Curr_TCB.T).V, Natural (ND.S (Curr_TCB.T + 1).I));
        when SF_Tail =>
          Pop (ND);
          --  [T] := Tail ([T], [T+1]) :
          ND.S (Curr_TCB.T).V :=
            VStrings_Pkg.Tail (ND.S (Curr_TCB.T).V, Natural (ND.S (Curr_TCB.T + 1).I));
        when SF_Starts_With =>
          Pop (ND);
          --  [T] := Starts_With ([T], [T+1]) :
          ND.S (Curr_TCB.T).I :=
            Boolean'Pos (HAC_Pack.Starts_With (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V));
        when SF_Ends_With =>
          Pop (ND);
          --  [T] := Ends_With ([T], [T+1]) :
          ND.S (Curr_TCB.T).I :=
            Boolean'Pos (HAC_Pack.Ends_With (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V));
        when SF_Year =>
          ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Year (ND.S (Curr_TCB.T).Tim));
        when SF_Month =>
          ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Month (ND.S (Curr_TCB.T).Tim));
        when SF_Day =>
          ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Day (ND.S (Curr_TCB.T).Tim));
        when SF_Seconds =>
          ND.S (Curr_TCB.T) := GR_Duration (Ada.Calendar.Seconds (ND.S (Curr_TCB.T).Tim));
        when SF_Int_Times_Char =>
          Pop (ND);
          if ND.S (Curr_TCB.T).I < 0 then raise VM_Out_of_Range; end if;
          --  [T] := [T] * [T+1] :
          ND.S (Curr_TCB.T) :=
            GR_VString (Natural (ND.S (Curr_TCB.T).I) * Character'Val (ND.S (Curr_TCB.T + 1).I));
        when SF_Int_Times_VStr =>
          Pop (ND);
          if ND.S (Curr_TCB.T).I < 0 then raise VM_Out_of_Range; end if;
          --  [T] := [T] * [T+1] :
          ND.S (Curr_TCB.T) :=
            GR_VString (Natural (ND.S (Curr_TCB.T).I) * ND.S (Curr_TCB.T + 1).V);
        when SF_Trim_Left  => Top_Item.V := Trim (Top_Item.V, Left);
        when SF_Trim_Right => Top_Item.V := Trim (Top_Item.V, Right);
        when SF_Trim_Both  => Top_Item.V := Trim (Top_Item.V, Both);
        --
        when SF_Time_Subtract =>
          Pop (ND);
          ND.S (Curr_TCB.T) := GR_Duration (ND.S (Curr_TCB.T).Tim - ND.S (Curr_TCB.T + 1).Tim);
        when SF_Duration_Subtract =>
          Pop (ND);
          ND.S (Curr_TCB.T).Dur := ND.S (Curr_TCB.T).Dur - ND.S (Curr_TCB.T + 1).Dur;
        when SF_Duration_Add =>
          Pop (ND);
          ND.S (Curr_TCB.T).Dur := ND.S (Curr_TCB.T).Dur + ND.S (Curr_TCB.T + 1).Dur;
        --
        when SF_Image_Ints             => Top_Item := GR_VString (HAC_Image (Top_Item.I));
        when SF_Image_Floats           => Top_Item := GR_VString (HAC_Image (Top_Item.R));
        when SF_Image_Attribute_Floats => Top_Item := GR_VString (HAC_Float'Image (Top_Item.R));
        when SF_Image_Times            => Top_Item := GR_VString (HAC_Image (Top_Item.Tim));
        when SF_Image_Durations        => Top_Item := GR_VString (Duration'Image (Top_Item.Dur));
        --
        when SF_Integer_Value =>
          begin
            Top_Item.I := HAC_Integer'Value (Defs.To_String (Top_Item.V));
          exception
            when E : Constraint_Error =>
              Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E));
              raise VM_Raised_Exception;
          end;
        when SF_Float_Value =>
          begin
            Top_Item := GR_Real (HAC_Float'Value (Defs.To_String (Top_Item.V)));
          exception
            when E : Constraint_Error =>
              Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E));
              raise VM_Raised_Exception;
          end;
        when SF_Get_Env =>
          declare
            use Ada.Environment_Variables;
            Name : constant String := Defs.To_String (Top_Item.V);
          begin
            if Exists (Name) then
              Top_Item.V := To_VString (Value (Name));
            else
              Top_Item.V := Null_VString;
            end if;
          end;
        when SF_Exists =>
          Top_Item.I := Boolean'Pos (Ada.Directories.Exists (Defs.To_String (Top_Item.V)));
        when SF_Niladic =>
          --  NILADIC functions need to push a new item (their own result).
          Push (ND);
          case SF_Niladic (Code) is
            when SF_Clock =>
              ND.S (Curr_TCB.T) := GR_Time (Ada.Calendar.Clock);
            when SF_Random_Float =>
              ND.S (Curr_TCB.T).R := Defs.HAC_Float (Random (ND.Gen));
            when SF_Argument_Count | SF_Directory_Separator |
                 SF_Current_Directory | SF_Get_Needs_Skip_Line |
                 SF_Command_Name =>
              --  Those functions have been already processed at an
              --  upper calling level by Do_Standard_Function.
              null;
          end case;
        when SF_EOF | SF_EOLN | SF_Argument =>
          --  Those functions have been already processed at an
          --  upper calling level by Do_Standard_Function.
          null;
      end case;
  end Do_SF_Operator;

end HAC_Sys.PCode.Interpreter.Operators;
