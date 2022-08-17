with HAC_Sys.Errors;

with Ada.Strings.Fixed;

package body HAC_Sys.PCode is

  --    type ObjData is (
  --     TabT,
  --     ATabt,
  --     BTabt,
  --     CodeT,
  --     ECountT,
  --     EntryTABt,
  --     FatT,
  --     FcTabt,
  --     STabT,
  --     TCountT,
  --     TaskTabT);
  --
  --    type TArraysTab is array (1 .. AMax) of ATabEntry;
  --    type TBlockTab is array (1 .. BMax) of BTabEntry;
  --    type TObjCode is array (0 .. CDMax) of Order;
  --    type TEntryTAB is array (0 .. EntryMax) of Index;
  --    type TTskDefTab is array (0 .. TaskMax) of Index;
  --    type TIdTab is array (0 .. TMax) of TabEntry;
  --    type TStringTab is array (0 .. SMax) of Character;
  --    type TFloatPtTab is array (1 .. C2Max) of Float;

  --    type SMAObject (O : ObjData) is record
  --      case O is
  --        when ATabt =>      ArraysTab : TArraysTab;
  --        when BTabt =>      BlockTab : TBlockTab;
  --        when CodeT =>      ObjCode : TObjCode;
  --        when ECountT =>    ECount : Integer;
  --        when EntryTABt =>  EntryTAB : TEntryTAB;
  --        when FatT =>       FileIOTab : FilDescr;
  --        when FcTabt =>     FloatPtTab : TFloatPtTab;
  --        when STabT =>      StringTab : TStringTab;
  --        when TabT =>       IdTab : TIdTab;
  --        when TaskTabT =>   TskDefTab : TTskDefTab;
  --        when TCountT =>    TCount : Integer;
  --      end case;
  --    end record;

  --    package OAIO is new Sequential_IO(SMAObject);
  --    use OAIO;

  --  ** The next three functions are almost identical.
  --
  --  * Emit   store an object code for an instruction with no
  --  *      arguments,
  --  * Emit1  store an object code for an instruction with one
  --  *      argument (Y), and
  --  * Emit2  store an object code for an instruction with two
  --  *      arguments.
  --  *
  --  * Originally it was implemented as three procedures.
  --  *
  --  * Emit and Emit1 call Emit2 with 0 for unused arguments
  --  *
  --  * Manuel *

  procedure Emit_Instruction (
    OC          : in out Object_Code_Table;
    LC          : in out Integer;
    D           :        Debug_Info;
    FCT         :        Opcode;
    a           :        Operand_1_Type;
    B           :        Operand_2_Type;
    c           :        Operand_3_Type;
    folded      :    out Boolean;
    specialized :    out Boolean
  )
  is
    use type Defs.HAC_Integer;
    FCT_corr : Opcode := FCT;
    --
    procedure Try_Specialization is
      procedure Specialize (new_value : Opcode) with Inline is
      begin
        FCT_corr := new_value;
        specialized := True;
      end Specialize;
    begin
      case FCT_corr is
        when k_Store =>
          if Defs.Discrete_Typ (Defs.Typen'Val (B)) then
            Specialize (k_Store_Discrete);
          end if;
        when k_Dereference =>
          if Defs.Discrete_Typ (Defs.Typen'Val (B)) then
            Specialize (k_Dereference_Discrete);
          end if;
        when others => null;
      end case;
    end Try_Specialization;
    --
    --  Try folding two instruction into one.
    --  This technique is only doable within Ada statements,
    --  otherwise the jumps will be wrong...
    --
    procedure Try_Folding is
      old : Order renames OC (LC - 1);
      procedure Simple_Substitution
        (prev_old_value, prev_new_value : Opcode) is
      pragma Inline (Simple_Substitution);
      begin
        if (not folded) and old.F = prev_old_value then
          old.F := prev_new_value;
          folded := True;
        end if;
      end Simple_Substitution;
    begin
      case FCT_corr is
        when k_SUBTRACT_Integer => Simple_Substitution (k_Push_Discrete_Literal, k_SUBTRACT_Integer_Literal);
        when k_MULT_Integer     => Simple_Substitution (k_Push_Discrete_Literal, k_MULT_Integer_Literal);
        when k_DIV_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_DIV_Integer_Literal);
        when k_EQL_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_EQL_Integer_Literal);
        when k_NEQ_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_NEQ_Integer_Literal);
        when k_LSS_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_LSS_Integer_Literal);
        when k_LEQ_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_LEQ_Integer_Literal);
        when k_GTR_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_GTR_Integer_Literal);
        when k_GEQ_Integer      => Simple_Substitution (k_Push_Discrete_Literal, k_GEQ_Integer_Literal);
        when k_ADD_Integer =>
          case old.F is
            when k_ADD_Integer =>
              old.F := k_ADD_Integer_Multiple;
              old.Y := 3;
              folded := True;
            when k_ADD_Integer_Multiple =>
              old.Y := old.Y + 1;
              folded := True;
            when others => null;
          end case;
          Simple_Substitution (k_MULT_Integer, k_MULT_then_ADD_Integer);
          Simple_Substitution (k_Push_Discrete_Literal, k_ADD_Integer_Literal);
        when k_ADD_Float =>
          case old.F is
            when k_ADD_Float =>
              old.F := k_ADD_Float_Multiple;
              old.Y := 3;
              folded := True;
            when k_ADD_Float_Multiple =>
              old.Y := old.Y + 1;
              folded := True;
            when others => null;
          end case;
          Simple_Substitution (k_MULT_Float, k_MULT_then_ADD_Float);
        when k_Unary_MINUS_Integer =>
          if old.F = k_Push_Discrete_Literal
            and then old.Y > Defs.HAC_Integer'First
          then
            old.Y := -old.Y;
            folded := True;
          end if;
        when k_Push_Discrete_Literal =>
          if old.F = k_Push_Discrete_Literal then
            old.F := k_Push_Two_Discrete_Literals;
            old.X := old.Y;
            old.Y := B;
            folded := True;
          end if;
        when k_Push_Float_Literal =>
          if old.F = k_Push_Float_Literal then
            old.F := k_Push_Two_Float_Literals;
            old.X := old.Y;
            old.Y := B;
            folded := True;
          end if;
        when k_Store =>
          Simple_Substitution (k_Push_Float_Literal, k_Store_Float_Literal);
          --  ^ Operand B (for this opcode, special type info) is discarded
          --  since the TYP is Floats.
          --
          Simple_Substitution (k_ADD_Float,      k_ADD_Float_then_Store);
          Simple_Substitution (k_SUBTRACT_Float, k_SUBTRACT_Float_then_Store);
          Simple_Substitution (k_MULT_Float,     k_MULT_Float_then_Store);
        when k_Store_Discrete =>
          Simple_Substitution (k_Push_Discrete_Literal, k_Store_Discrete_Literal);
          --  ^ Operand B (for this opcode, special type info) is discarded
          --  since we have a (subtype-checked) discrete value.
          Simple_Substitution (k_ADD_Integer,      k_ADD_Integer_then_Store);
          Simple_Substitution (k_SUBTRACT_Integer, k_SUBTRACT_Integer_then_Store);
          Simple_Substitution (k_MULT_Integer,     k_MULT_Integer_then_Store);
        when k_NOT_Boolean =>
          Simple_Substitution (k_AND_Boolean, k_NAND_Boolean);
          Simple_Substitution (k_OR_Boolean,  k_NOR_Boolean);
        when others =>
          null;
      end case;
    end Try_Folding;
    --
  begin
    folded      := False;
    specialized := False;
    if LC = OC'Last then
      Errors.Fatal (Errors.Object_Code);
    end if;
    Try_Specialization;
    if LC > OC'First then
      Try_Folding;
    end if;
    if not folded then
      OC (LC).F := FCT_corr;
      OC (LC).X := a;
      OC (LC).Y := B;
      OC (LC).Z := c;
      OC (LC).D := D;
      LC        := LC + 1;
    end if;
  end Emit_Instruction;

  procedure Patch_Addresses (
    OC            : in out Object_Code_Table;
    dummy_address :        Operand_2_Type
  )
  is
    use Defs;
    use type HAC_Integer;
  begin
    for Op of OC (OC'First .. OC'Last - 1) loop
      if Op.F in Jump_Opcode and then Op.Y = dummy_address then
        Op.Y := HAC_Integer (OC'Last);
      end if;
    end loop;
  end Patch_Addresses;

  procedure Patch_Addresses (
    OC  : in out Object_Code_Table;
    PT  :        Patch_Table;
    Top : in out Natural
  )
  is
  begin
    for Instruction_Address of PT (PT'First .. Top) loop
      OC (Integer (Instruction_Address)).Y := Operand_2_Type (OC'Last);
    end loop;
    Top := 0;
  end Patch_Addresses;

  procedure Feed_Patch_Table (
    PT  : in out Patch_Table;
    Top : in out Natural;
    LC  :        Integer
  )
  is
  begin
    if Top < PT'Last then
      Top := Top + 1;
    else
      Errors.Fatal (Errors.PATCHING);
    end if;
    PT (Top) := Operand_2_Type (LC);
  end Feed_Patch_Table;

  procedure Dump (
    OC        : Object_Code_Table;
    Str_Const : String;
    Flt_Const : Defs.Float_Constants_Table_Type;
    Text      : Ada.Text_IO.File_Type
  )
  is
    use Ada.Strings.Fixed, Ada.Text_IO;
    package Opcode_IO   is new Enumeration_IO (Opcode);
    package Code_Pos_IO is new Integer_IO (Natural);
    package Operand1_IO is new Integer_IO (Operand_1_Type);
    package Operand2_IO is new Integer_IO (Operand_2_Type);
    function HAC_Image is new HAT.HAC_Generic_Image (Defs.HAC_Integer);
    SF_C : SF_Code;
    SP_C : SP_Code;
    Old_X1, Old_X2 : Operand_1_Type := 0;
    Old_Y1, Old_Y2 : Operand_2_Type := 0;
    --
    function Padded_Opcode (o : Opcode) return String is
      s : String (1 .. Opcode'Width);
    begin
      Opcode_IO.Put (s, o);
      return s;
    end Padded_Opcode;
    use Defs;
    use type Operand_2_Type;
  begin
    Put_Line
      (Text, "Position   : Opcode " & (Opcode'Width - 7) * ' ' &
             "Level/X        " &
             "Addr/Val/Y" &
             ";      Approx source location; Extra information");
    Put_Line (Text, 120 * '-');
    for i in OC'Range loop
      Code_Pos_IO.Put (Text, i);
      Put (Text, ": " & Padded_Opcode (OC (i).F));
      case OC (i).F is  --  Omit showing X for some 1-operand instructions
        when k_Exit_Call | k_Exit_Function |
          k_Mark_Stack | k_Push_Discrete_Literal |
          k_Push_Float_Literal .. k_Push_Float_Last |
          k_Store |
          k_Pop |
          Unary_Operator_Opcode |
          Binary_Operator_Opcode |
          Special_Operator_Opcode |
          Jump_Opcode
        =>
          Put (Text, "     ");
        when others =>
          Operand1_IO.Put (Text, OC (i).X, 5);
      end case;
      case OC (i).F is  --  Omit showing Y for some 0-operand instructions
        when k_Pop |
          k_Push_Float_First .. k_Push_Float_Last |
          Binary_Operator_Opcode |
          Special_Operator_Opcode
        =>
          Put (Text, "                    ");
        when others =>
          Operand2_IO.Put (Text, OC (i).Y);
      end case;
      Put (Text, "; ");
      Code_Pos_IO.Put (Text, OC (i).D.Line_Number);
      Put (Text, "  " & HAT.VStr_Pkg.To_String (OC (i).D.Full_Block_Id));
      case OC (i).F is  --  Show extra information
        when k_Push_Float_Literal =>
          Put (Text, "; " & HAT.HAC_Image (Flt_Const (Integer (OC (i).Y))));
        when k_Push_Float_First =>
          Put (Text, "; HAT.Real'First: " & HAC_Float'Image (HAT.Real'First));
        when k_Push_Float_Last =>
          Put (Text, "; HAT.Real'Last: " & HAC_Float'Image (HAT.Real'Last));
        when k_Variable_Initialization =>
          Put (Text, "; " & Defs.Typen'Image (Defs.Typen'Val (OC (i).Y)));
        when k_HAT_Function =>
          SF_C := SF_Code'Val (OC (i).Y);
          Put (Text, "; " & SF_Code'Image (SF_C));
        when k_HAT_Procedure =>
          SP_C := SP_Code'Val (OC (i).X);
          Put (Text, "; " & SP_Code'Image (SP_C));
          case SP_C is
            when SP_Get .. SP_Get_Line =>
              Put (Text, "; " & Defs.Typen'Image (Defs.Typen'Val (OC (i).Y)));
            when SP_Put .. SP_Put_Line =>
              if Defs.Typen'Val (OC (i).Y) = Defs.String_Literals then
                Put (Text, "; """ & Str_Const (Integer (Old_Y2) .. Integer (Old_Y2 + Old_X2 - 1)) & '"');
              end if;
            when others =>
              null;
          end case;
        when k_FOR_Release_Stack_After_End =>
          Put (Text, "; Cleanup after END LOOP of a FOR loop");
        when k_CASE_Switch =>
          Put (Text, "; (CASE) Jump to the switch block");
        when k_CASE_Choice_Value =>
          Put (Text, "; (CASE) WHEN " & HAC_Image (OC (i).Y) & " =>");
        when k_CASE_Choice_Range =>
          Put (Text, "; (CASE) WHEN " &
            HAC_Image (OC (i).X) & " .." &
            HAC_Image (OC (i).Y) & " =>");
        when k_CASE_Choice_Others =>
          Put (Text, "; (CASE) WHEN OTHERS =>");
        when k_Mark_Stack =>
          Put (Text, "; Mark stack for calling subprogram with ID #" & HAC_Image (OC (i).Y));
        when k_Call =>
          Put (Text, "; Call with PSize = " & HAC_Image (OC (i).Y + 1));
        when k_Update_Display_Vector =>
          Put (Text, "; Update: low level (called) = " & HAC_Image (OC (i).X) &
                     ", high level (caller) = " & HAC_Image (OC (i).Y));
        when others =>
          null;
      end case;
      New_Line (Text);
      --
      Old_X2 := Old_X1;
      Old_X1 := OC (i).X;
      --
      Old_Y2 := Old_Y1;
      Old_Y1 := OC (i).Y;
    end loop;
  end Dump;

  -------------------------------------------------------------SaveOBJ----

  procedure SaveOBJ (FileName : String) is
  --   ObjFile:  OAIO.File_Type;
  --   Buffer: SMAObject;
  begin
    --   Create( ObjFile, name => FileName & ".Obj");
    --
    --   FOR  I  IN   1.. AMax  LOOP
    --     Buffer.ArraysTab(I) := ArraysTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   1.. BMax  LOOP
    --     Buffer.BlockTab(I) := BlockTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   0.. CDMax  LOOP
    --     Buffer.ObjCode(I) := ObjCode(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   Buffer.ECount := ECount;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   0.. EntryMax  LOOP
    --     Buffer.EntryTAB(I) := EntryTAB(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   --{Buffer.FileIOTab := FileIOTab;} --{ Error assigning file }
    --
    --   FOR  I  IN   1.. C2Max  LOOP
    --     Buffer.FloatPtTab(I) := FloatPtTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   0.. SMax  LOOP
    --     Buffer.StringTab(I) := StringTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   0.. TMax  LOOP
    --     Buffer.IdTab(I) := IdTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   FOR  I  IN   0.. TaskMax  LOOP
    --     Buffer.TskDefTab(I) := TskDefTab(I);
    --   END LOOP;
    --   Write(ObjFile, Buffer);
    --
    --   Buffer.TCount := TCount;
    --   Write(ObjFile, Buffer);
    --
    --   Close(ObjFile);

    null; -- will be streamed...

  end SaveOBJ;

  ----------------------------------------------------------RestoreOBJ----}

  procedure RestoreOBJ (FileName : String) is
  --   ObjFile:  OAIO.File_Type;
  --   Buffer: SMAObject;
  begin
    --   BEGIN
    --     Open(ObjFile, in_file, FileName & ".Obj");
    --   EXCEPTION
    --     when others=>
    --       IF qDebug THEN
    --         Text_IO.Put_Line("Cannot find file : " & FileName & ".Obj");
    --       END IF;
    --       raise Failure_1_0;
    --   END;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   1.. AMax  LOOP
    --     ArraysTab(I) := Buffer.ArraysTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   1.. BMax  LOOP
    --     BlockTab(I) := Buffer.BlockTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   0.. CDMax  LOOP
    --     ObjCode(I) := Buffer.ObjCode(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   ECount := Buffer.ECount;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   0.. EntryMax  LOOP
    --     EntryTAB(I) := Buffer.EntryTAB(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   --{FileIOTab := Buffer.FileIOTab;} --{ Error assigning file }
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   1.. C2Max  LOOP
    --     FloatPtTab(I) := Buffer.FloatPtTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   0.. SMax  LOOP
    --     StringTab(I) := Buffer.StringTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   0.. TMax  LOOP
    --     IdTab(I) := Buffer.IdTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   FOR  I  IN   0.. TaskMax  LOOP
    --     TskDefTab(I) := Buffer.TskDefTab(I);
    --   END LOOP;
    --
    --   Read(ObjFile, Buffer);
    --   TCount := Buffer.TCount;
    --
    --   Close(ObjFile);

    null; -- will be streamed...

  end RestoreOBJ;

end HAC_Sys.PCode;
