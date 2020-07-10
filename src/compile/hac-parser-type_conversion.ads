with HAC.Defs;

procedure HAC.Parser.Type_Conversion (  --  Ada RM 4.6
  CD    : in out Compiler_Data;
  Level :        PCode.Nesting_level;
  FSys  :        Defs.Symset;
  X     :    out Exact_Typ
);
