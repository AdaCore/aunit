with AUnit; use AUnit;

--  Unit tests for AUnit.Test_Suites.
package Test_Test_Suite is
   use Test_Results;

   type The_Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   procedure Register_Tests (T : in out The_Test_Case);

   --  Provide name identifying the test case:
   function Name (T : The_Test_Case) return Test_String;

private
   --  Test Routines:
   procedure Test_Inherited_Tests (T : in out Test_Cases.Test_Case'Class);
   procedure Test_No_Test_Case (T : in out Test_Cases.Test_Case'Class);
   procedure Test_No_Test_Routines (T : in out Test_Cases.Test_Case'Class);
   procedure Test_One_Test_Case (T : in out Test_Cases.Test_Case'Class);

end Test_Test_Suite;
