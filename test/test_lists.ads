with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with AUnit.Test_Cases;
use AUnit.Test_Cases;
package Test_Lists is
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Override:

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

   -- Setup before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T :  in out Test_Case);

end Test_Lists;
