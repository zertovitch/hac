with HAC.UErrors; use HAC.UErrors;
-- with Sequential_IO;
-- with Text_IO;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body HAC.PCode is

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

  -- ** The next three functions are almost identical.
  --
  -- * Emit   store an object code for an instruction with no
  -- *      arguments,
  -- * Emit1  store an object code for an instruction with one
  -- *      argument (Y), and
  -- * Emit2  store an object code for an instruction with two
  -- *      arguments.
  -- *
  -- * Originally it was implemented as three procedures.
  -- *
  -- * Emit and Emit1 call Emit2 with 0 for unused arguments
  -- *
  -- * Manuel *

  function For_END (for_BEGIN: Opcode) return Opcode is
  begin
    case for_BEGIN is
      when k_FOR_Forward_Begin => return k_FOR_Forward_End;
      when k_FOR_Reverse_Begin => return k_FOR_Reverse_End;
      when others => return for_BEGIN;
    end case;
  end For_END;

  ----------------------------------------------------------------Emit----

  procedure Emit (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode)
  is
  begin
    Emit2 (OC, LC, D, FCT, 0, 0);    --  Order's X, Y are not used
  end Emit;

  ---------------------------------------------------------------Emit1----

  procedure Emit1 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode;
    B    :        Integer)
  is
  begin
    Emit2 (OC, LC, D, FCT, 0, B);    --  Order's X is not used
  end Emit1;

  ---------------------------------------------------------------Emit2----

  procedure Emit2 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode;
    a, B :        Integer)
  is
  begin
    if LC = OC'Last then
      Fatal (OBJECTS);
    end if;
    OC (LC).F := FCT;
    OC (LC).X := a;
    OC (LC).Y := B;
    OC (LC).D := D;
    LC        := LC + 1;
  end Emit2;

  procedure Patch_Addresses (OC : in out Object_Code_Table) is
    LC0 : Integer := OC'First;
  begin
    while LC0 < OC'Last loop
      if OC (LC0).F in Jump_Opcode and then OC (LC0).Y = dummy_address then
        OC (LC0).Y := OC'Last;
      end if;
      LC0 := LC0 + 1;
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
      OC (Instruction_Address).Y := OC'Last;
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
      Fatal (PATCHING);
    end if;
    PT (Top) := LC;
  end Feed_Patch_Table;

  procedure Dump (OC : Object_Code_Table; Text : Ada.Text_IO.File_Type) is
    use Ada.Text_IO;
    package Opcode_IO   is new Enumeration_IO (Opcode);
    package Code_Pos_IO is new Integer_IO (Natural);
    package Operand1_IO is new Integer_IO (Operand1);
    package Operand2_IO is new Integer_IO (Operand2);
    --
    function Padded_Opcode (o: Opcode) return String is
      s : String (1 .. Opcode'Width);
    begin
      Opcode_IO.Put(s, o);
      return s;
    end Padded_Opcode;
  begin
    Put_Line
      (Text, "Position   : Opcode " & (Opcode'Width - 7) * ' ' &
             "Lvl X " &
             "Addr/Val Y" &
             ";      Approx. source location");
    Put_Line (Text, 90 * '-');
    for i in OC'Range loop
      Code_Pos_IO.Put (Text, i);
      Put (Text, ": " & Padded_Opcode (OC (i).F));
      Operand1_IO.Put (Text, OC (i).X, 5);
      Operand2_IO.Put (Text, OC (i).Y);
      Put (Text, "; ");
      Code_Pos_IO.Put (Text, OC (i).D.Line);
      Put (Text, "  " & HAC.Data.To_String (OC (i).D.Block));
      New_Line (Text);
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

end HAC.PCode;
