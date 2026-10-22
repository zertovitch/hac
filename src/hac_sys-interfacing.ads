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
--  Demo in: demo/data_exchange/exchange_native_side.adb

with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter.In_Defs;

with System;

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

  --  The imported subprogram on the HAC side can be a procedure
  --  or a function, declared with the "with Import => True" aspect.
  --  For an imported *function*, the Data array received by the
  --  call-back contains, after the items of the (possibly empty)
  --  parameter list, the item(s) of the returned value, which the
  --  call-back must fill, using the To_HAC functions.
  --  Example: this HAC-side function:
  --
  --    function Double (i : Integer) return Integer with Import => True;
  --
  --  is matched by a call-back doing, on the native side:
  --
  --    Data (Data'Last) := To_HAC (2 * To_Native (Data (1)));
  --
  --  The result type on the HAC side is subject to the same
  --  restriction as for any HAC function: an atomic type
  --  (scalar, enumeration or VString).

  type Exported_Procedure_With_Context is access procedure
    (Data : in out HAC_Element_Array; Context : System.Address);
  --  ^ Same as Exported_Procedure, plus a context, given at
  --    registration time (see Register below). This allows a
  --    call-back to be shared by several interpreter sessions,
  --    each with its own context, without global variables.
  --    For a more type-safe context than System.Address, see the
  --    Contextual_Call_Back generic below.

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

  procedure Register
    (BD       : Builder.Build_Data;
     Callback : Exported_Procedure_With_Context;
     Name     : String;
     Context  : System.Address);
  --  ^ The Context must remain valid (not be freed) for as long as
  --    the call-back is registered. The context is per registration:
  --    the same Callback can be registered under the same Name in
  --    different Build_Data objects, with different contexts.

  --  Typed, context-aware registration. Example:
  --
  --    type Session is limited record ... end record;
  --    procedure Set_Color (Data : in out HAC_Element_Array; S : in out Session);
  --    package Session_CB is new Contextual_Call_Back
  --      (Context   => Session,
  --       Call_Back => Set_Color);
  --    ...
  --    S_1, S_2 : aliased Session;
  --    Session_CB.Register (BD_1, "Set_Color", S_1'Access);
  --    Session_CB.Register (BD_2, "Set_Color", S_2'Access);
  --
  --    The same Set_Color mutates a different Session per Build_Data.
  --
  --  NB: the instance must be at library level (not inside a
  --    subprogram), for taking 'Access of the internal thunk.
  --
  generic
    type Context is limited private;
    with procedure Call_Back
      (Data : in out HAC_Element_Array; Ctx : in out Context);
  package Contextual_Call_Back is
    type Context_Access is access all Context;
    procedure Register
      (BD      : in     Builder.Build_Data;
       Name    : in     String;
       Context : in     Context_Access);
  end Contextual_Call_Back;

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
  --  See demo/data_exchange/exchange_native_side_pkg.adb for an example.

  function Get_VM_Variable (BD : Builder.Build_Data; Name : String) return String;
  procedure Set_VM_Variable (BD : in out Builder.Build_Data; Name : String; Value : String);

private

  type HAC_Element is new HAC_Sys.PCode.Interpreter.In_Defs.Data_Type;

end HAC_Sys.Interfacing;
