with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

--  Simple test case
package body Empty_Test_Case is


   --  Test Routines:

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      null;
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Empty Test Case");
   end Name;

end Empty_Test_Case;
