with HAC_Sys.UErrors;

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

  function For_END (for_BEGIN : Opcode) return Opcode is
  begin
    case for_BEGIN is
      when k_FOR_Forward_Begin => return k_FOR_Forward_End;
      when k_FOR_Reverse_Begin => return k_FOR_Reverse_End;
      when others => return for_BEGIN;
    end case;
  end For_END;

  procedure Emit_Instruction (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  )
  is
  begin
    if LC = OC'Last then
      UErrors.Fatal (UErrors.Object_Code);
    end if;
    OC (LC).F := FCT;
    OC (LC).X := a;
    OC (LC).Y := B;
    OC (LC).D := D;
    LC        := LC + 1;
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
      UErrors.Fatal (UErrors.PATCHING);
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
    SF_C : SF_Code;
    SP_C : SP_Code;
    Old_Y1, Old_Y2, Old_Y3, Old_Y4 : Operand_2_Type := 0;
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
             ";      Approx. source location; Extra information");
    Put_Line (Text, 90 * '-');
    for i in OC'Range loop
      Code_Pos_IO.Put (Text, i);
      Put (Text, ": " & Padded_Opcode (OC (i).F));
      case OC (i).F is  --  Omit showing X for some 1-operand instructions
        when k_Exit_Call | k_Exit_Function |
          k_Mark_Stack | k_Push_Discrete_Literal |
          k_Pop | Jump_Opcode
        =>
          Put (Text, "     ");
        when others =>
          Operand1_IO.Put (Text, OC (i).X, 5);
      end case;
      case OC (i).F is  --  Omit showing Y for some 0-operand instructions
        when k_Pop
        =>
          Put (Text, "                    ");
        when others =>
          Operand2_IO.Put (Text, OC (i).Y);
      end case;
      Put (Text, "; ");
      Code_Pos_IO.Put (Text, OC (i).D.Line_Number);
      Put (Text, "  " & HAL.VStr_Pkg.To_String (OC (i).D.Full_Block_Id));
      case OC (i).F is  --  Show extra information
        when k_Push_Float_Literal =>
          Put (Text, "; " & HAL.HAC_Image (Flt_Const (Integer (OC (i).Y))));
        when k_Variable_Initialization =>
          Put (Text, "; " & Defs.Typen'Image (Defs.Typen'Val (OC (i).Y)));
        when k_Standard_Functions =>
          SF_C := SF_Code'Val (OC (i).Y);
          Put (Text, "; " & SF_Code'Image (SF_C));
        when k_File_I_O =>
          SP_C := SP_Code'Val (OC (i).X);
          Put (Text, "; " & SP_Code'Image (SP_C));
          case SP_C is
            when SP_Get .. SP_Get_Line =>
              Put (Text, "; " & Defs.Typen'Image (Defs.Typen'Val (OC (i).Y)));
            when SP_Put .. SP_Put_Line =>
              if Defs.Typen'Val (OC (i).Y) = Defs.String_Literals then
                Put (Text, "; """ & Str_Const (Integer (Old_Y3) .. Integer (Old_Y3 + Old_Y4 - 1)) & '"');
              end if;
            when others =>
              null;
          end case;
        when k_FOR_Release_Stack_After_End =>
          Put (Text, "; after END LOOP of a FOR loop");
        when k_Mark_Stack =>
          Put (Text, "; Mark stack for calling" & OC (i).Y'Image);
        when k_Call =>
          Put (Text, "; Call with PSize =" & Operand_2_Type'Image (OC (i).Y + 1));
        when k_Update_Display_Vector =>
          Put (Text, "; Update: low level (called) =" & OC (i).X'Image &
                     ", high level (caller) =" & OC (i).Y'Image);
        when others =>
          null;
      end case;
      New_Line (Text);
      Old_Y4 := Old_Y3;
      Old_Y3 := Old_Y2;
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
