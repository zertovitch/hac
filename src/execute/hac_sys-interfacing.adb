with HAC_Sys.Defs;

with HAL;

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with System;

package body HAC_Sys.Interfacing is

  use HAL, Defs;

  function To_HAC (Data : Integer) return HAC_Element is
    new_element : HAC_Element;
  begin
    new_element.I := HAC_Integer (Data);
    return new_element;
  end To_HAC;

  function To_HAC (Data : Long_Float) return HAC_Element is
  begin
    return GR_Real (HAL.Real (Data));
  end To_HAC;

  function To_HAC (Data : String) return HAC_Element is
  begin
    return GR_VString (Data);
  end To_HAC;

  function To_Native (Data : HAC_Element) return Integer is
  begin
    return Integer (Data.I);
  end To_Native;

  function To_Native (Data : HAC_Element) return Long_Float is
  begin
    if Data.Special = Floats then
      return Long_Float (Data.R);
    end if;
    raise HAC_Type_Error with "Expected a HAL.Real, found Integer or " & Typen'Image (Data.Special);
  end To_Native;

  function To_Native (Data : HAC_Element) return String is
  begin
    if Data.Special = VStrings then
      return To_String (Data.V);
    end if;
    raise HAC_Type_Error with "Expected a VString, found Integer or " & Typen'Image (Data.Special);
  end To_Native;

  procedure Register
    (BD : Builder.Build_Data; Callback : Exported_Procedure; Name : String)
  is
    function Convert is new Ada.Unchecked_Conversion (Exported_Procedure, System.Address);
    use Ada.Characters.Handling;
  begin
    BD.CD.Exported_Procedures.Include (To_Upper (Name), Convert (Callback));
  end Register;

end HAC_Sys.Interfacing;
