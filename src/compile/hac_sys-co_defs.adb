with Ada.Unchecked_Deallocation;

package body HAC_Sys.Co_Defs is

  overriding procedure Finalize (CD : in out Compiler_Data) is
    procedure Free is
      new Ada.Unchecked_Deallocation (HAC_Sys.PCode.Object_Code_Table, Object_Code_Table_Access);
  begin
    Free (CD.ObjCode);
  end Finalize;

end HAC_Sys.Co_Defs;
