with SMAda_Data;                   use SMAda_Data;
with UErrors;                      use UErrors;
-- with Sequential_IO;
-- with Text_IO;

PACKAGE BODY PCode IS

TYPE
    ObjData IS ( TabT,   ATabt,   BTabt,
                 CodeT,  ECountT, EntryTABt, FatT,
                 FcTabt, STabT,   TCountT,   TaskTabT );

    type TArraysTab  is ARRAY ( 1..AMax )     OF ATabEntry;
    type TBlockTab   is ARRAY ( 1..BMax )     OF BTabEntry;
    type TObjCode    is ARRAY ( 0..CDMax )    OF Order;
    type TEntryTAB   is ARRAY ( 0..EntryMax ) OF Index;
    type TTskDefTab  is ARRAY ( 0..TaskMax )  OF Index;
    type TIdTab      is ARRAY ( 0..TMax )     OF TabEntry;
    type TStringTab  is ARRAY ( 0..SMax )     OF Character;
    type TFloatPtTab is ARRAY ( 1..C2Max )    OF Float;

    TYPE SMAObject(O:ObjData) IS RECORD

        CASE O IS
            WHEN ATabt =>      ArraysTab:  TArraysTab;
            WHEN BTabt =>      BlockTab:   TBlockTab;
            WHEN CodeT =>      ObjCode:    TObjCode;
            WHEN ECountT =>    ECount:     Integer;
            WHEN EntryTABt =>  EntryTAB:   TEntryTAB;
            WHEN FatT =>       FileIOTab:  FilDescr;
            WHEN FcTabt =>     FloatPtTab: TFloatPtTab;
            WHEN STabT =>      StringTab:  TStringTab;
            WHEN TabT =>       IdTab:      TIdTab;
            WHEN TaskTabT =>   TskDefTab:  TTskDefTab;
            WHEN TCountT =>    TCount:     Integer;
        END CASE;

   END RECORD;

--    package OAIO is new Sequential_IO(SMAObject);
--    use OAIO;

-- ** The next three functions are almost identical.
-- * Emit    store an object code for an instruction with no
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


----------------------------------------------------------------Emit----

PROCEDURE Emit(FCT: Integer) IS
  BEGIN
    Emit2(FCT, 0, 0);    -- X, Y are not used
  END;

---------------------------------------------------------------Emit1----

PROCEDURE Emit1(FCT, b: Integer) IS
  BEGIN
    Emit2(FCT, 0, B);    -- X is not used
  END;

---------------------------------------------------------------Emit2----

PROCEDURE Emit2(FCT, a, b: Integer) IS
  BEGIN
    IF  LC = CMax THEN Fatal(6); END IF;
    ObjCode(LC).F := FCT;
    ObjCode(LC).X := a;
    ObjCode(LC).Y := b;
    LC := LC + 1;
  END Emit2;

-------------------------------------------------------------SaveOBJ----

PROCEDURE SaveOBJ(FileName: String) IS
--   ObjFile:  OAIO.File_Type;
--   Buffer: SMAObject;
  BEGIN
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

  END;

----------------------------------------------------------RestoreOBJ----}

PROCEDURE RestoreOBJ(FileName: String) IS
--   ObjFile:  OAIO.File_Type;
--   Buffer: SMAObject;
 BEGIN
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

 END;

END PCode;

