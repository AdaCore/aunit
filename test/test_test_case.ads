with AUnit; use AUnit;
--  Unit tests for AUnit.Test_Cases.
package Test_Test_Case is
   use Test_Results;

   type Test_Case is new Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;
private
   --  Test Routines:
   procedure Test_Register_Tests (T : in out Test_Case);
   procedure Test_Run (T : in out Test_Case);
   procedure Test_Set_Up (T : in out Test_Case);
   procedure Test_Torn_Down (T : in out Test_Case);
   procedure Test_Multiple_Failures (T : in out Test_Case);

   --  Exclude when run-time library does not support exception handling
   procedure Test_Exceptions (T : in out Test_Case);

end Test_Test_Case;
