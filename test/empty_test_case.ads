with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with  AUnit.Test_Cases;

-- Test case with no routines.
package Empty_Test_Case is
   type Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

private

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

end Empty_Test_Case;
