-- This is the pseudo-package containing all specifications of all support
-- routines for SmallAda. All SmallAda programs must have "with" and "use"
-- of this package.
--
-- Note for HAC: this is kept for early stages. At some point SMALL_SP won't
-- be used anymore. The package is compilable by a real Ada compiler like
-- GNAT, so the test programs can be run on both HAC and GNAT.

package SMALL_SP is

   type SEMAPHORE is new Integer; -- private;

   -- Absolute Value - SMA used ABS instead of "ABS" (operator)... TBA !!
   -- function   ABS       ( I : INTEGER   ) return INTEGER;
   -- function   ABS       ( F : FLOAT     ) return FLOAT;

   -- Square
   function   SQR       ( I : INTEGER   ) return INTEGER;
   function   SQR       ( F : FLOAT     ) return FLOAT;

   -- Square Root
   function   SQRT      ( I : INTEGER   ) return FLOAT;
   function   SQRT      ( F : FLOAT     ) return FLOAT;

   -- Odd Valued
   function   ODD       ( I : INTEGER   ) return BOOLEAN;

   -- Integer to Character
   function   ASCII     ( I : INTEGER   ) return CHARACTER;

   -- Character to Integer
   function   ORD       ( C : CHARACTER ) return INTEGER;

   -- Next Character
   function   SUCC      ( C : CHARACTER ) return CHARACTER;

   -- Previous Character
   function   PRED      ( C : CHARACTER ) return CHARACTER;

   -- Round to an Integer
   function   ROUND     ( F : FLOAT     ) return INTEGER;

   -- Truncate
   function   TRUNC     ( F : FLOAT     ) return INTEGER;

   -- Trigonometric Functions     w/ arguments in radians
   function   SIN       ( I : INTEGER   ) return FLOAT;
   function   SIN       ( F : FLOAT     ) return FLOAT;
   function   COS       ( I : INTEGER   ) return FLOAT;
   function   COS       ( F : FLOAT     ) return FLOAT;
   function   ARCTAN    ( I : INTEGER   ) return FLOAT;
   function   ARCTAN    ( F : FLOAT     ) return FLOAT;

   -- Exponential Functions
   function   LN        ( I : INTEGER   ) return FLOAT;
   function   LN        ( F : FLOAT     ) return FLOAT;
   function   EXP       ( I : INTEGER   ) return FLOAT;
   function   EXP       ( F : FLOAT     ) return FLOAT;

   -- Random Integer      from 0 to I
   function   RANDOM    ( I : INTEGER   ) return INTEGER;
   function   RANDOM    ( I : INTEGER   ) return Float;


   -- Get
   procedure  GET       ( C : OUT CHARACTER);
   procedure  GET       ( I : OUT INTEGER  );
   procedure  GET       ( F : OUT FLOAT    );
   procedure  GET       ( B : OUT BOOLEAN  );

   -- Get and then move file pointer to next line
   procedure  GET_LINE  ( C : OUT CHARACTER);
   procedure  GET_LINE  ( I : OUT INTEGER  );
   procedure  GET_LINE  ( F : OUT FLOAT    );
   procedure  GET_LINE  ( B : OUT BOOLEAN  );

   subtype WIDTH is Positive;
   subtype DECIMALS is Positive;

   -- Put
   procedure  PUT       ( C : IN  CHARACTER);
   procedure  PUT       ( I : IN  INTEGER  );
   procedure  PUT       ( I : IN  INTEGER;  W:  WIDTH);
   procedure  PUT       ( F : IN  FLOAT    );
   procedure  PUT       ( F : IN  FLOAT;    W:  WIDTH; D : DECIMALS);
   procedure  PUT       ( B : IN  BOOLEAN  );
   procedure  PUT       ( S : IN  String   );

   -- Put and then NEW_LINE ( !! same as Ada.TExt_IO only for S )
   procedure  PUT_LINE  ( C : IN  CHARACTER);
   procedure  PUT_LINE  ( I : IN  INTEGER  );
   procedure  PUT_LINE  ( I : IN  INTEGER; W:  WIDTH);
   procedure  PUT_LINE  ( F : IN  FLOAT    );
   procedure  PUT_LINE  ( F : IN  FLOAT;   W:  WIDTH; D : DECIMALS);
   procedure  PUT_LINE  ( B : IN  BOOLEAN  );
   procedure  PUT_LINE  ( S : IN  String   );

   -- Mark End of Line
   procedure  NEW_LINE                      ;

   procedure  CURSORAT (X, Y: Integer);

   -- Semaphore Procedures
   procedure  WAIT      ( S : SEMAPHORE    );
   procedure  SIGNAL    ( S : SEMAPHORE    );

private

   -- type       SEMAPHORE is new INTEGER;

end SMALL_SP;
