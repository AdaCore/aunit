with AUnit.Tests.Test_Cases.Registration;

--  Simple test case
package body Empty_Test_Case is
   pragma Ada_05;

   package Registration is new Framework.Test_Cases.Registration (Test_Case);
   use Registration;

   function Routine_Count (T : Test_Case'Class) return Count_Type is
   begin
      return Registration.Routine_Count (T);
   end Routine_Count;

   procedure Register_Routine
     (T : in out Test_Case;
      Routine : access procedure (Test : in out Test_Case);
      Name    : String) renames Registration.Register_Routine;

   --  Test Routines:

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Empty Test Case");
   end Name;

end Empty_Test_Case;
