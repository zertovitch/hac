--  Single-build version of All_Silent_Tests.
--  Differences:
--    - The test framework targets entirely the compiler used for the build around
--        `silent_tests_single_build.adb`.
--    - Reversely, All_Silent_Tests calls specifically HAC for individual tests.

with HAT;

with Attributes_Test,
     Case_Statement,
     Constants,
     Declarations,
     Enumerations,
     Floats,
     Forward,
     Integers,
     Loops,
     Object_Init,
     Recursion,
     Sorting_Tests,
     Strings,
     Type_Conversion;

procedure Silent_Tests_Single_Build is

  use HAT;

begin
  Put_Line ("Silent_Tests_Single_Build can be used for testing any Ada compiler.");
  New_Line;
  Put_Line (+"Please use All_Silent_Tests for specifically " &
             "and throughfully testing HAC (actual regression test).");
  New_Line;
  Put_Line ("Attributes_Test");   Attributes_Test;
  Put_Line ("Case_Statement");    Case_Statement;
  Put_Line ("Constants");         Constants;
  Put_Line ("Declarations");      Declarations;
  Put_Line ("Enumerations");      Enumerations;
  Put_Line ("Floats");            Floats;
  Put_Line ("Forward");           Forward;
  Put_Line ("Integers");          Integers;
  Put_Line ("Loops");             Loops;
  Put_Line ("Object_Init");       Object_Init;
  Put_Line ("Recursion");         Recursion;
  Put_Line ("Sorting_Tests");     Sorting_Tests;
  Put_Line ("Strings");           Strings;
  Put_Line ("Type_Conversion");   Type_Conversion;
end Silent_Tests_Single_Build;
