with AUnit; use AUnit;

--  Test case with no routines.
package Empty_Test_Case is
   pragma Ada_05;
   use Test_Results;

   type The_Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out The_Test_Case);

   --  Provide name identifying the test case:
   function Name (T : The_Test_Case) return Test_String;

private

   type The_Test_Case is new AUnit.Test_Cases.Test_Case with null record;

end Empty_Test_Case;
