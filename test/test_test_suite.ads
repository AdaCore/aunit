with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with AUnit.Test_Cases;
use AUnit.Test_Cases;

--  Unit tests for AUnit.Test_Suites.
package Test_Test_Suite is
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

end Test_Test_Suite;
