--  The subtype name has just been parsed, its identifier is in Type_ID.
--  This identification method works only for standard types,
--  which is currently the only option.
--  The symbol "(" has been parsed too.

private procedure HAC_Sys.Parser.Type_Conversion (  --  Ada RM 4.6
  CD      : in out Co_Defs.Compiler_Data;
  Level   :        Defs.Nesting_level;
  FSys    :        Defs.Symset;
  Typ_ID  : in     Co_Defs.IdTabEntry;
  X       : in     Co_Defs.Exact_Subtyp
);
