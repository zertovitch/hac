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

  --  TBD: conversion functions from & to integer, floats, vstrings...

  type HAC_Element_Array is array (Positive range <>) of HAC_Element;

  type Exported_Procedure is access procedure (Data : in out HAC_Element_Array);

  procedure Register
    (BD       : Builder.Build_Data;
     Callback : Exported_Procedure;
     Name     : String);

private

  type HAC_Element is new HAC_Sys.PCode.Interpreter.In_Defs.General_Register;

end HAC_Sys.Interfacing;
