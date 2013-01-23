  


--  *********************************************************************

     --  Small Ada PCODE interpreter.                                        

     --  *********************************************************************



     -- Translated on 23-Jan-2013 by (New) P2Ada v. 28-Oct-2009
     -- The following with/use clauses are put graciously by P2Ada.
     -- Some of them may be useless, your Ada compiler will tell it you.
     --   (GNAT: with '-gnatwa')
     with Ada.Text_IO;                       use Ada.Text_IO;
     with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
     with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
     with Ada.Long_Float_Text_IO;            use Ada.Long_Float_Text_IO;
     with Ada.Direct_IO;
     with Ada.Command_Line;                  use Ada.Command_Line; -- ParamStr,...
     with Ada.Characters.Handling;           use Ada.Characters.Handling; -- UpCase
     with Interfaces;                        use Interfaces; -- For Shift_Left/Right
     -- This is for Pi :
     with Ada.Numerics;                      use Ada.Numerics;
     -- This is for Sqrt, Sin, Cos, etc. :
     with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
     with Ada.Numerics.Long_Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;
     -- This is for Dispose. P2Ada writes automatically:
     --   "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
     with Ada.Unchecked_Deallocation;
     
     


  with
    SMDATA,       --  Small Ada global data unit 
    
     Crt,          --  for GotoXY, ClrEol, ClrScr 
    
     Dos,          --  for GetTime, GetDate 
    
     ERRORS,
    SMERRORS,     --  Small Ada error handling procs 
    
     MSSCRN1,      --  ROUTINES FOR TASK MONITORING 
    
     MSuser,       --  ROUTINES FOR TASK MONITORING 
    
     AVL,          --  ROUTINES FOR TASK MONITORING 
    
     InterDef,
    ErInter; -- [P2Ada]: place it before main procedure
     



  package SMINTER is
     procedure  INTERPRET;


end SMINTER;
