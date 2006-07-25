with AUnit.Tests.Test_Cases.Registration;

--  Test case with one routine
package body One_Test_Case is

   use Assertions;

   --  Test Routines:
   procedure Test_1 (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Test_1;

   --  Register test routines to call:
   package Registration is new Framework.Test_Cases.Registration (Test_Case);
   use Registration;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_1'Access, "Test Routine 1");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("One Test Case");
   end Name;

end One_Test_Case;
