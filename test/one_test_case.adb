with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

--  Test case with one routine
package body One_Test_Case is


   --  Test Routines:
   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_1;


   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin

      Register_Routine
        (T, Test_1'Access, "Test Routine 1");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("One Test Case");
   end Name;

end One_Test_Case;
