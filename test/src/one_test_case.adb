--
--  Copyright (C) 2008, AdaCore
--
--  Test case with one routine
package body One_Test_Case is

   --  Test Routines:
   procedure Test_1 (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      null;
   end Test_1;

   --  Register test routines to call:
   use AUnit.Test_Cases.Registration;

   procedure Register_Tests (T : in out The_Test_Case) is
   begin
      Register_Routine (T, Test_1'Access, "Test Routine 1");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : The_Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("One Test Case");
   end Name;

end One_Test_Case;
