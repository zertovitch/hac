with HAC_Sys.PCode.Interpreter.Tasking;

with Ada.Calendar;

package body HAC_Sys.PCode.Interpreter.Calls is

  procedure Do_Calling_Operation (
    CD :        Co_Defs.Compiler_Data;
    ND : in out In_Defs.Interpreter_Data
  )
  is
    use Defs, In_Defs;
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    use type HAC_Integer;

    procedure Do_Mark_Stack is
      --  VSize is the maximum stack room needed by the subprogram to be called.
      VSize : constant Integer :=
        Integer (CD.Blocks_Table (CD.IdTab (Integer (IR.Y)).block_ref).VSize);
    begin
      if Curr_TCB.T + VSize > Curr_TCB.STACKSIZE then
        raise VM_Stack_Overflow;
      end if;
      Curr_TCB.T := Curr_TCB.T + 5;          --  Make room for fixed area
      ND.S (Curr_TCB.T - 1).I := HAC_Integer (VSize - 1);
      ND.S (Curr_TCB.T).I     := IR.Y;       --  CD.IdTab index of called procedure/entry
    end Do_Mark_Stack;

    trace_display : constant Boolean := False;

    procedure Show_Display (D : Co_Defs.Display_Type; L_Max : Nesting_level; T : String) is
      use HAL;
    begin
      if trace_display then
        New_Line;
        Put_Line ("Level   Stack base of variables  --  " & T);
        for i in 0 .. L_Max loop
          Put (Integer (i), 5);
          Put (D (i));
          New_Line;
        end loop;
      end if;
    end Show_Display;

    procedure Do_Call is
      use Ada.Calendar;
      F1 : HAC_Float;    --  Internal float registers
      Activation_Record_Base, Ident_Index_of_Called,
      New_Stack_Top, new_address : Index;
      Called_Level : Nesting_level;
      Task_Entered : Integer;
    begin
      --  Procedure and task entry CALL  --  Cramer
      if IR.X = Defs.Timed_Entry_Call then
        --  Timed entry call
        F1 := ND.S (Curr_TCB.T).R;  --  Pop delay time
        Pop (ND);
      end if;
      Activation_Record_Base := Curr_TCB.T - Integer (IR.Y);
      Ident_Index_of_Called := Index (ND.S (Activation_Record_Base + 4).I);
      Called_Level := CD.IdTab (Ident_Index_of_Called).lev;
      Show_Display (Curr_TCB.DISPLAY, Called_Level, "before call");
      Curr_TCB.DISPLAY (Called_Level + 1) := Activation_Record_Base;
      Show_Display (Curr_TCB.DISPLAY, Called_Level + 1, "on call");
      New_Stack_Top := Index (ND.S (Activation_Record_Base + 3).I) + Activation_Record_Base;
      --
      ND.S (Activation_Record_Base + 1).I := HAC_Integer (Curr_TCB.PC);                      --  return address
      ND.S (Activation_Record_Base + 2).I := HAC_Integer (Curr_TCB.DISPLAY (Called_Level));  --  static link
      ND.S (Activation_Record_Base + 3).I := HAC_Integer (Curr_TCB.B);                       --  dynamic link
      --
      Curr_TCB.B := Activation_Record_Base;
      Curr_TCB.T := New_Stack_Top;
      case IR.X is  --  Call type
        when Defs.Normal_Procedure_Call =>
          new_address := CD.IdTab (Ident_Index_of_Called).adr_or_sz;
          if new_address < 0 then
            raise VM_Subprogram_Spec;
          end if;
          Curr_TCB.PC := new_address;  --  Jump to subprogram start.
        when Defs.Normal_Entry_Call =>
          Tasking.Queue (CD, ND, Ident_Index_of_Called, ND.CurTask);  --  put self on entry queue
          Curr_TCB.TS  := WaitRendzv;
          Task_Entered := CD.IdTab (Ident_Index_of_Called).adr_or_sz;  --  Task being entered
          if ((ND.TCB (Task_Entered).TS = WaitRendzv) and (ND.TCB (Task_Entered).SUSPEND = Ident_Index_of_Called)) or
             (ND.TCB (Task_Entered).TS = TimedWait)
          then
            --  wake accepting task if necessary
            ND.TCB (Task_Entered).TS      := Ready;
            ND.TCB (Task_Entered).SUSPEND := 0;
          end if;
          ND.SWITCH := True;                 --  give up control

        when Defs.Timed_Entry_Call =>
          Tasking.Queue (CD, ND, Ident_Index_of_Called, ND.CurTask);  --  put self on entry queue
          Task_Entered := CD.IdTab (Ident_Index_of_Called).adr_or_sz;  --  Task being entered
          --
          if ((ND.TCB (Task_Entered).TS = WaitRendzv) and (ND.TCB (Task_Entered).SUSPEND = Ident_Index_of_Called)) or
             (ND.TCB (Task_Entered).TS = TimedWait)
          then
            --  wake accepting task if necessary
            Curr_TCB.TS := WaitRendzv;     --  suspend self
            ND.TCB (Task_Entered).TS := Ready;       --  wake accepting task
            ND.TCB (Task_Entered).SUSPEND := 0;
          else
            Curr_TCB.TS := TimedRendz;          --  Timed Wait For Rendezvous
            Curr_TCB.R1.I := 1;                 --  Init R1 to specify NO timeout
            Curr_TCB.R2.I := HAC_Integer (Ident_Index_of_Called);  --  Save address of queue for purge
            ND.SYSCLOCK := Clock;               --  update System Clock
            Curr_TCB.WAKETIME := ND.SYSCLOCK + Duration (F1);
          end if;
          ND.SWITCH := True;       --  give up control

        when Defs.Conditional_Entry_Call =>
          Task_Entered := CD.IdTab (Ident_Index_of_Called).adr_or_sz;              --  Task being entered
          if ((ND.TCB (Task_Entered).TS = WaitRendzv) and (ND.TCB (Task_Entered).SUSPEND = Ident_Index_of_Called)) or
             (ND.TCB (Task_Entered).TS = TimedWait)
          then
            Tasking.Queue (CD, ND, Ident_Index_of_Called, ND.CurTask);  --  put self on entry queue
            Curr_TCB.R1.I := 1;        --  Indicate entry successful
            Curr_TCB.TS := WaitRendzv;
            ND.TCB (Task_Entered).TS      := Ready;  --  wake accepting task if required
            ND.TCB (Task_Entered).SUSPEND := 0;
            ND.SWITCH              := True;   --  give up control
          else
            --  can't wait, forget about entry call
            Curr_TCB.R1.I := 0;   --  Indicate entry failed in R1 1
            --  failure will be acknowledged by next instruction, 32
          end if;
        when others =>
          null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;
    end Do_Call;

    procedure Do_Exit_Call is
    --  EXIT entry call or procedure call  --  Cramer
    begin
      --  Set back stack top as before call:
      Curr_TCB.T := Curr_TCB.B - 1;
      if IR.Y = Defs.Normal_Procedure_Call then
        --  Set back program counter to position of Call:
        Curr_TCB.PC := Integer (ND.S (Curr_TCB.B + 1).I);  --  Normal proc call return
      end if;
      if Curr_TCB.PC = 0 then
        ND.TActive  := ND.TActive - 1;
        Curr_TCB.TS := Completed;
        ND.SWITCH   := True;
      else
        --  Set back base of caller:
        Curr_TCB.B := Integer (ND.S (Curr_TCB.B + 3).I);
        if IR.Y = Defs.Timed_Entry_Call or IR.Y = Defs.Conditional_Entry_Call then
          if IR.Y = Defs.Timed_Entry_Call and Curr_TCB.R1.I = 0 then
            Push (ND);
          end if;
          --  A JMPC instruction always follows (?)
          --  timed and conditional entry call
          --  returns (32).  Push entry call
          ND.S (Curr_TCB.T).I := Curr_TCB.R1.I;    --  success indicator for JMPC.
        end if;
      end if;
    end Do_Exit_Call;

    procedure Do_Exit_Function is
    begin
      --  Set back stack top as before call, plus 1 item (the return value):
      Curr_TCB.T  := Curr_TCB.B;
      --  Set back program counter to position of Call:
      Curr_TCB.PC := Integer (ND.S (Curr_TCB.B + 1).I);
      --  Set back base of caller:
      Curr_TCB.B  := Integer (ND.S (Curr_TCB.B + 3).I);
      --
      if IR.Y = Defs.End_Function_without_Return and then ND.PS /= Exception_Raised then
        raise VM_Function_End_without_Return;
      end if;
    end Do_Exit_Function;

    procedure Do_Update_Display_Vector is
      --  Emitted at the end of Subprogram_or_Entry_Call, when the
      --  called subprogram's nesting level is *lower* than the
      --  caller's block level. This includes the case where P and Q are
      --  defined at the same level L: when Q calls P, Q's block level
      --  is L + 1, so it's calling P of level L and the update is
      --  needed after the call.
      Low_Level  : constant Nesting_level := Nesting_level (ND.IR.X);  --  Called.
      High_Level : constant Nesting_level := Nesting_level (ND.IR.Y);  --  Caller.
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      New_Base : Defs.Index := Curr_TCB.B;
      --  ^ initial value: stack base of caller (dynamic link) after call exit.
      Address_Base_Lower_Level : Defs.Index;
      New_Base_Value : HAC_Integer;
      L : Nesting_level := High_Level;
    begin
      pragma Assert (Low_Level < High_Level);
      loop
        --  At this point: L is always >= 1 since L > Low_Level >= 0.
        Curr_TCB.DISPLAY (L) := New_Base;
        L := L - 1;
        exit when L = Low_Level;
        Address_Base_Lower_Level := New_Base + 2;  --  Written by Do_Call.
        if Address_Base_Lower_Level not in ND.S'Range then
          raise Constraint_Error with
            "Address_Base_Lower_Level = " & Address_Base_Lower_Level'Image &
            " out of stack range";
        end if;
        New_Base_Value := ND.S (Address_Base_Lower_Level).I;
        if New_Base_Value not in 0 .. HAC_Integer (MaxINT) then
          raise Constraint_Error with
            "Invalid New_Base_Value =" & New_Base_Value'Image &
            " found on stack @ index" & Address_Base_Lower_Level'Image;
        end if;
        New_Base := Defs.Index (New_Base_Value);
      end loop;
      Show_Display (Curr_TCB.DISPLAY, High_Level, "after Update_Display_Vector");
    end Do_Update_Display_Vector;

  begin
    case Calling_Opcode (ND.IR.F) is
      when k_Mark_Stack            => Do_Mark_Stack;
      when k_Call                  => Do_Call;
      when k_Exit_Call             => Do_Exit_Call;
      when k_Exit_Function         => Do_Exit_Function;
      when k_Update_Display_Vector => Do_Update_Display_Vector;
    end case;
  end Do_Calling_Operation;

end HAC_Sys.PCode.Interpreter.Calls;
