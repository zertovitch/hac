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

private package HAC_Sys.Parser.Type_Def is  --  Package around type definitions.

  ------------------------------------------------------------------
  --------------------------------------Type_or_Subtype_Declaration-
  --  Type Declarations   : RM 3.2.1
  --  Subtype Declarations: RM 3.2.2
  --
  --  Parses "[sub]type T is ..." right after the "[sub]type" symbol.
  --  Depending on the symbol (SUBTYPE or TYPE), the parser will
  --  look for either a Subtype_Indication or a Type_Definition.
  --
  procedure Type_or_Subtype_Declaration
    (CD         : in out Co_Defs.Compiler_Data;
     Level      : in     Defs.Nesting_Level;
     FSys_NTD   : in     Defs.Symset);

  ------------------------------------------------------------------
  -----------------------------------Type_Definition - RM 3.2.1 (4)-
  --
  procedure Type_Definition
    (CD            : in out Co_Defs.Compiler_Data;
     Initial_Level : in     Defs.Nesting_Level;
     FSys_TD       : in     Defs.Symset;
     xTP           :    out Co_Defs.Exact_Subtyp;
     Size          :    out Integer);

  ------------------------------------------------------------------
  --------------------------------Subtype_Indication - RM 3.2.2 (2)-
  --
  procedure Subtype_Indication
    (CD      : in out Co_Defs.Compiler_Data;
     Level   : in     Defs.Nesting_Level;
     FSys_TD : in     Defs.Symset;
     xTP     :    out Co_Defs.Exact_Subtyp;
     Size    :    out Integer);

end HAC_Sys.Parser.Type_Def;
