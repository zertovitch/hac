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
--  Package around entering definitions, from the parser or "by hand",
--  for built-in packages.

package HAC_Sys.Parser.Enter_Def is

  procedure Enter_Block
    (CD    : in out Co_Defs.Compiler_Data;
     Tptr  :        Integer);

  procedure Enter_Simple
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural);

  --  Enter a definition with package prefix.
  procedure Enter_Prefixed
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural);

  procedure Enter
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     prefixed         :        Boolean;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural);

  procedure Enter_Array
    (CD        : in out Co_Defs.Compiler_Data;
     Index_STP :        Co_Defs.Exact_Subtyp);

  procedure Enter_Variables
    (CD       : in out Co_Defs.Compiler_Data;
     Level    :        Defs.Nesting_Level;
     prefixed :        Boolean);
     --  For instance, fields in a record must be non-prefixed.

end HAC_Sys.Parser.Enter_Def;
