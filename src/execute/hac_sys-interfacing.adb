with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with System;

package body HAC_Sys.Interfacing is

  procedure Register
    (BD : Builder.Build_Data; Callback : Exported_Procedure; Name : String)
  is
    function Convert is new Ada.Unchecked_Conversion (Exported_Procedure, System.Address);
    use Ada.Characters.Handling;
  begin
    BD.CD.Exported_Procedures.Include (To_Upper (Name), Convert (Callback));
  end Register;

end HAC_Sys.Interfacing;
