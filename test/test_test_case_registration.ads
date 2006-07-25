with Framework; use Framework;

--  Unit tests for AUnit.Test_Cases.Registration.
package Test_Test_Case_Registration is
   use Test_Results;

   type Test_Case is new Framework.Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;

private
   --  Test Routines:
   procedure Test_Register_Routine (T : in out Test_Case);

end Test_Test_Case_Registration;
