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

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs;

package HAC_Sys.Scanner is

  --  Source code scanning for the compiler

  procedure In_Symbol (CD : in out Co_Defs.Compiler_Data);

  procedure Skip_Blanks (CD : in out Co_Defs.Compiler_Data);

  --  One-symbol lookahead:
  --  save the scanner state, consume one symbol with In_Symbol,
  --  then either commit to the new state, or restore the saved
  --  state and let the normal parsing process the symbol.
  --  Used for disambiguating, e.g., a parameter name in a named
  --  association "Formal => Actual" from a positional actual
  --  starting with the same identifier.
  --
  type Scanner_State is private;

  procedure Save_Scanner_State
    (CD    :        Co_Defs.Compiler_Data;
     State :    out Scanner_State);

  procedure Restore_Scanner_State
    (CD    : in out Co_Defs.Compiler_Data;
     State : in     Scanner_State);

private

  type Scanner_State is record
    --  Scanner part of Current_Unit_Data:
    buffer          : Co_Defs.Source_Buffer_String;
    buffer_length   : Natural;
    buffer_position : Positive;
    input_line      : Co_Defs.Source_Line_String;
    c, prev_c       : Character;
    CC              : Integer;
    LL              : Natural;
    location        : Defs.Symbol_Location;
    --  Symbol-level state, as written by In_Symbol:
    Sy              : Defs.Symbol;
    prev_sy         : Defs.Symbol;
    prev_sy_loc     : Defs.Symbol_Location;
    Id              : Defs.Alfa;
    Id_with_case    : Defs.Alfa;
    Id_location     : Integer;
    INum            : Defs.HAC_Integer;
    RNum            : Defs.HAC_Float;
    SLeng           : Integer;
  end record;

end HAC_Sys.Scanner;
