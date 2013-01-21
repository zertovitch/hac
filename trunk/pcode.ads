-- This unit is used to store object codes in the ObjCode table.
-- The three procedures Emit, Emit1, and Emit2 are called from
-- the compiler and the parser.

Package PCode is

 -- Store PCode object in the object code table
	PROCEDURE Emit(FCT: Integer);
	PROCEDURE Emit1(FCT, B: Integer);
	PROCEDURE Emit2(FCT, a, B: Integer);

  -- Save and restore an object file (currently not used in the Mac) }
	PROCEDURE SaveOBJ(FileName: String);
	PROCEDURE RestoreOBJ(FileName: String);

end;
