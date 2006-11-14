--  Simple test case
package body Empty_Test_Case is
   pragma Ada_05;

   --  Test Routines:

   --  Register test routines to call:
   procedure Register_Tests (T : in out The_Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : The_Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Empty Test Case");
   end Name;

end Empty_Test_Case;
