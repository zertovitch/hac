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

  ------------------------------------
  --  Native  ->  HAC  conversions  --
  ------------------------------------

  function To_HAC (Data : Integer)    return HAC_Element;
  function To_HAC (Data : Long_Float) return HAC_Element;
  function To_HAC (Data : String)     return HAC_Element;

  generic
    type Any_Integer is range <>;
  function To_HAC_Any_Integer (Data : Any_Integer) return HAC_Element;

  generic
    type Any_Enum is (<>);
  function To_HAC_Any_Enum (Data : Any_Enum) return HAC_Element;

  generic
    type Any_Float is digits <>;
  function To_HAC_Any_Float (Data : Any_Float) return HAC_Element;

  ------------------------------------
  --  HAC  ->  Native  conversions  --
  ------------------------------------

  function To_Native (Data : HAC_Element) return Integer;
  function To_Native (Data : HAC_Element) return Long_Float;
  function To_Native (Data : HAC_Element) return String;

  generic
    type Any_Integer is range <>;
  function To_Native_Any_Integer (Data : HAC_Element) return Any_Integer;

  generic
    type Any_Enum is (<>);
  function To_Native_Any_Enum (Data : HAC_Element) return Any_Enum;

  generic
    type Any_Float is digits <>;
  function To_Native_Any_Float (Data : HAC_Element) return Any_Float;

  HAC_Type_Error : exception;

  type HAC_Element_Array is array (Positive range <>) of HAC_Element;

  ----------------------
  --  Call-back type  --
  ----------------------

  type Exported_Procedure is access procedure (Data : in out HAC_Element_Array);

  --  Registration. You can register call-backs any time: before compilation,
  --  after compilation, or even during HAC's run-time if you can control,
  --  from the Native program, when a call-back is called. For instance,
  --  the registration of a call-back to some Native procedure or another one
  --  might be done during another call-back.
  --
  procedure Register
    (BD       : Builder.Build_Data;
     Callback : Exported_Procedure;
     Name     : String);

  procedure Deregister
    (BD       : Builder.Build_Data;
     Name     : String);

  ---------------------------------
  --  Virtual Machine Variables  --
  ---------------------------------

  --  The strings set as Virtual Machine Variables can be read and written
  --  from a HAC program via the HAT.Get_VM_Variable and HAT.Set_VM_Variable
  --  subprograms. This way, data can be exchanged, before and after running
  --  a program through the HAC VM interpreter, between that program and the
  --  program that runs HAC. The strings are as persistent as an object
  --  of type Builder.Build_Data is.
  --  See src/apps/exchange_native_side_pkg.adb for an example.

  function Get_VM_Variable (BD : Builder.Build_Data; Name : String) return String;
  procedure Set_VM_Variable (BD : in out Builder.Build_Data; Name : String; Value : String);

private

  type HAC_Element is new HAC_Sys.PCode.Interpreter.In_Defs.Data_Type;

end HAC_Sys.Interfacing;
