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
--  Aggregates (RM 4.3): "(1, 2, 3)", "(Field => Value, ...)", "(others => 0)", ...
--
--  Called only from the two contexts where the destination's composite type
--  is already known before any of the aggregate's contents are parsed:
--  object declaration initializers (Const_Var) and assignment statements
--  (Statements.Assignment). Since HAC only supports constrained array and
--  record types, every component's position (array index, record field) is
--  known at compile time, so aggregates are compiled by pure compile-time
--  unrolling into the same k_Push_Address / k_Store / k_Copy_Block
--  instructions used for ordinary assignments -- no new P-code opcodes or
--  VM changes are needed.

private package HAC_Sys.Parser.Aggregates is

  --  Precondition: CD.Sy = LParent, not yet consumed.
  --  Consumes the aggregate through its closing ')'.
  --
  --  Expected       : the destination's (or, when recursing, a component's)
  --                   type; must be Arrays or Records.
  --  Block_Data     : the enclosing block's data (nesting level and
  --                   flow-analysis state).
  --  Follow_Symbols : the set of symbols allowed to follow this aggregate,
  --                   used for error recovery and to bound sub-expression
  --                   parsing (the classic recursive-descent "follow set").
  --  Dest_Level     : the destination's lexical level.
  --  Dest_Base      : the destination's compile-time-constant base address
  --                   (adr_or_sz); every component is written at
  --                   Dest_Base + <compile-time offset>.
  --
  procedure Parse_Aggregate
    (CD             : in out Co_Defs.Compiler_Data;
     Block_Data     : in out Block_Data_Type;
     Follow_Symbols :        Defs.Symset;
     Expected       :        Co_Defs.Exact_Subtyp;
     Dest_Level     :        Defs.Nesting_Level;
     Dest_Base      :        Defs.HAC_Integer);

end HAC_Sys.Parser.Aggregates;
