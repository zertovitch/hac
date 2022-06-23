-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--
--  This package defines the interfacing mechanism between HAC and a "full Ada" app.
--  Demo in: src/apps/exchange_native_side.adb

with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.Interfacing is

  type HAC_Element is private;

  function To_HAC (Data : Integer)    return HAC_Element;
  function To_HAC (Data : Long_Float) return HAC_Element;
  function To_HAC (Data : String)     return HAC_Element;

  function To_Native (Data : HAC_Element) return Integer;
  function To_Native (Data : HAC_Element) return Long_Float;
  function To_Native (Data : HAC_Element) return String;

  HAC_Type_Error : exception;

  type HAC_Element_Array is array (Positive range <>) of HAC_Element;

  type Exported_Procedure is access procedure (Data : in out HAC_Element_Array);

  procedure Register
    (BD       : Builder.Build_Data;
     Callback : Exported_Procedure;
     Name     : String);

  procedure Deregister
    (BD       : Builder.Build_Data;
     Name     : String);

private

  type HAC_Element is new HAC_Sys.PCode.Interpreter.In_Defs.Data_Type;

end HAC_Sys.Interfacing;
