--  This is the pseudo-package containing all specifications of all support
--  routines for HAC. So far all HAC programs must have "with" and "use"
--  of this package.
--
--  Note: this is kept for early stages. At some point HAC_Pack won't
--  be used anymore. The package is compilable by a real Ada compiler like
--  GNAT, so the test programs can be run on both HAC and GNAT.

package HAC_Pack is

   type Semaphore is new Integer; -- private;

   -- Absolute Value - SMA used ABS instead of "ABS" (operator)... TBA !!
   -- function   ABS       ( I : INTEGER   ) return INTEGER;
   -- function   ABS       ( F : FLOAT     ) return FLOAT;

   -- Square
   function   Sqr       ( I : Integer   ) return Integer;
   function   Sqr       ( F : Float     ) return Float;

   -- Square Root
   function   Sqrt      ( I : Integer   ) return Float;
   function   Sqrt      ( F : Float     ) return Float;

   -- Odd Valued
   function   Odd       ( I : Integer   ) return Boolean;

   -- Integer to Character
   function   ASCII     ( I : Integer   ) return Character;

   -- Character to Integer
   function   Ord       ( C : Character ) return Integer;

   -- Next Character
   function   Succ      ( C : Character ) return Character;

   -- Previous Character
   function   Pred      ( C : Character ) return Character;

   -- Round to an Integer
   function   Round     ( F : Float     ) return Integer;

   -- Truncate
   function   Trunc     ( F : Float     ) return Integer;

   -- Trigonometric Functions     w/ arguments in radians
   function   Sin       ( I : Integer   ) return Float;
   function   Sin       ( F : Float     ) return Float;
   function   Cos       ( I : Integer   ) return Float;
   function   Cos       ( F : Float     ) return Float;
   function   ArcTan    ( I : Integer   ) return Float;
   function   ArcTan    ( F : Float     ) return Float;

   -- Exponential Functions
   function   Log       ( I : Integer   ) return Float;
   function   Log       ( F : Float     ) return Float;
   function   Exp       ( I : Integer   ) return Float;
   function   Exp       ( F : Float     ) return Float;

   -- Random Integer      from 0 to I
   function   Random    ( I : Integer   ) return Integer;
   function   Random    ( I : Integer   ) return Float;

   -- Get
   procedure  Get       ( C : out Character);
   procedure  Get       ( I : out Integer  );
   procedure  Get       ( F : out Float    );
   procedure  Get       ( B : out Boolean  );

   -- Get and then move file pointer to next line
   procedure  Get_Line  ( C : out Character);
   procedure  Get_Line  ( I : out Integer  );
   procedure  Get_Line  ( F : out Float    );
   procedure  Get_Line  ( B : out Boolean  );

   subtype Width is Positive;
   subtype Decimals is Positive;

   -- Put
   procedure  Put       ( C : in  Character);
   procedure  Put       ( I : in  Integer  );
   procedure  Put       ( I : in  Integer;  W:  Width);
   procedure  Put       ( F : in  Float    );
   procedure  Put       ( F : in  Float;    W:  Width; D : Decimals);
   procedure  Put       ( B : in  Boolean  );
   procedure  Put       ( S : in  String   );

   -- Put and then NEW_LINE ( !! it is the same as Ada.Text_IO only for S )
   procedure  Put_Line  ( C : in  Character);
   procedure  Put_Line  ( I : in  Integer  );
   procedure  Put_Line  ( I : in  Integer; W:  Width);
   procedure  Put_Line  ( F : in  Float    );
   procedure  Put_Line  ( F : in  Float;   W:  Width; D : Decimals);
   procedure  Put_Line  ( B : in  Boolean  );
   procedure  Put_Line  ( S : in  String   );

   -- Mark End of Line
   procedure  New_Line                      ;

   procedure  CursorAt (X, Y: Integer);

   -- Semaphore Procedures
   procedure  Wait      ( S : Semaphore    );
   procedure  Signal    ( S : Semaphore    );

private

   -- type       SEMAPHORE is new INTEGER;

end HAC_Pack;
