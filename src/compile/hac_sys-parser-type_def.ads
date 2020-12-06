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

with HAC_Sys.Defs, HAC_Sys.PCode;

private package HAC_Sys.Parser.Type_Def is  --  Package around type definitions.

  use Defs;

  ------------------------------------------------------------------
  ------------------Number_Declaration_or_Enum_Item_or_Literal_Char-
  --
  procedure Number_Declaration_or_Enum_Item_or_Literal_Char (
    CD      : in out Compiler_Data;
    Level   : in     PCode.Nesting_level;
    FSys_ND : in     Symset;
    C       :    out Constant_Rec
  );

  ------------------------------------------------------------------
  -------------------------------------------------Type_Declaration-
  --
  --  Parses "type T is ..." and "subtype T is ..."
  --
  procedure Type_Declaration (
    CD       : in out Compiler_Data;
    Level    : in     PCode.Nesting_level;
    FSys_NTD : in     Symset
  );

  ------------------------------------------------------------------
  -----------------------------------Type_Definition - RM 3.2.1 (4)-
  --
  procedure Type_Definition (
    CD            : in out Compiler_Data;
    Initial_Level : in     PCode.Nesting_level;
    FSys_TD       : in     Symset;
    xTP           :    out Exact_Typ;
    Size          :    out Integer;
    First         :    out HAC_Integer;  --  T'First value if discrete
    Last          :    out HAC_Integer   --  T'Last value if discrete
  );

end HAC_Sys.Parser.Type_Def;
