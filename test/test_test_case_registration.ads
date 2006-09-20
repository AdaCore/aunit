with AUnit; use AUnit;

--  Unit tests for AUnit.Test_Cases.Registration.
package Test_Test_Case_Registration is
   use Test_Results;

   type The_Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   procedure Register_Tests (T : in out The_Test_Case);

   --  Provide name identifying the test case:
   function Name (T : The_Test_Case) return Test_String;

private
   --  Test Routines:
   procedure Test_Register_Routine (T : in out Test_Cases.Test_Case'Class);

end Test_Test_Case_Registration;
