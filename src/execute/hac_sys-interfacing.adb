with HAC_Sys.Defs;

with HAT;

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with System;

package body HAC_Sys.Interfacing is

  use HAT, Defs;

  function To_HAC (Data : Integer) return HAC_Element is
    new_element : HAC_Element;
  begin
    new_element.I := HAC_Integer (Data);
    return new_element;
  end To_HAC;

  function To_HAC (Data : Long_Float) return HAC_Element is
  begin
    return GR_Real (HAT.Real (Data));
  end To_HAC;

  function To_HAC (Data : String) return HAC_Element is
  begin
    return GR_VString (Data);
  end To_HAC;

  function To_HAC_Any_Integer (Data : Any_Integer) return HAC_Element is
    new_element : HAC_Element;
  begin
    new_element.I := HAC_Integer (Data);
    return new_element;
  end To_HAC_Any_Integer;

  function To_HAC_Any_Enum (Data : Any_Enum) return HAC_Element is
    new_element : HAC_Element;
  begin
    new_element.I := Any_Enum'Pos (Data);
    return new_element;
  end To_HAC_Any_Enum;

  function To_HAC_Any_Float (Data : Any_Float) return HAC_Element is
  begin
    return GR_Real (HAT.Real (Data));
  end To_HAC_Any_Float;

  function To_Native (Data : HAC_Element) return Integer is
  begin
    return Integer (Data.I);
  end To_Native;

  function To_Native (Data : HAC_Element) return Long_Float is
  begin
    if Data.Special = Floats then
      return Long_Float (Data.R);
    end if;
    raise HAC_Type_Error with "Expected a HAT.Real, found Integer or " & Typen'Image (Data.Special);
  end To_Native;

  function To_Native (Data : HAC_Element) return String is
  begin
    if Data.Special = VStrings then
      return To_String (Data.V);
    end if;
    raise HAC_Type_Error with "Expected a VString, found Integer or " & Typen'Image (Data.Special);
  end To_Native;

  function To_Native_Any_Integer (Data : HAC_Element) return Any_Integer is
  begin
    return Any_Integer (Data.I);
  end To_Native_Any_Integer;

  function To_Native_Any_Enum (Data : HAC_Element) return Any_Enum is
  begin
    return Any_Enum'Val (Data.I);
  end To_Native_Any_Enum;

  function To_Native_Any_Float (Data : HAC_Element) return Any_Float is
  begin
    if Data.Special = Floats then
      return Any_Float (Data.R);
    end if;
    raise HAC_Type_Error with "Expected a HAT.Real, found Integer or " & Typen'Image (Data.Special);
  end To_Native_Any_Float;

  function Get_VM_Variable (BD : Builder.Build_Data; Name : String) return String is
    cur : constant Builder.String_Maps.Cursor := BD.global_VM_variables.Find (HAT.To_VString (Name));
    use Builder.String_Maps;
  begin
    return
      (if cur = Builder.String_Maps.No_Element then
         ""
       else
         HAT.To_String (Builder.String_Maps.Element (cur)));
  end Get_VM_Variable;

  procedure Set_VM_Variable (BD : in out Builder.Build_Data; Name : String; Value : String) is
  begin
    BD.global_VM_variables.Include (HAT.To_VString (Name), HAT.To_VString (Value));
  end Set_VM_Variable;

  procedure Register
    (BD : Builder.Build_Data; Callback : Exported_Procedure; Name : String)
  is
    function Convert is new Ada.Unchecked_Conversion (Exported_Procedure, System.Address);
    use Ada.Characters.Handling;
  begin
    if Callback /= null then
      BD.CD.Exported_Procedures.Include (To_Upper (Name), Convert (Callback));
    end if;
  end Register;

  procedure Deregister (BD : Builder.Build_Data; Name : String)
  is
    use Ada.Characters.Handling;
  begin
    BD.CD.Exported_Procedures.Exclude (To_Upper (Name));
  end Deregister;

end HAC_Sys.Interfacing;
