with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

--  Test case that inherits a routine.
package body Inherited_Test_Case is


   --  Test Routines:
   procedure Test_2 is
   begin
      null;
   end Test_2;


   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      One_Test_Case.Register_Tests (One_Test_Case.Test_Case (T));
      Register_Routine
        (T, Test_2'Access, "Test Routine 2");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Inherited Test Case");
   end Name;

end Inherited_Test_Case;
