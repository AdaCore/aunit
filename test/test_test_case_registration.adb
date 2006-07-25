with Empty_Test_Case;
with Ada_Containers; use Ada_Containers;
with AUnit.Tests.Test_Cases.Registration;

--  Unit tests for AUnit.Test_Cases.Registration.
package body Test_Test_Case_Registration is
   use Assertions;

   procedure Dummy_Test_Routine (T : in out Empty_Test_Case.Test_Case);

   procedure Dummy_Test_Routine (T : in out Empty_Test_Case.Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Dummy_Test_Routine;

   procedure Test_Register_Routine (T : in out Test_Case) is
      use Empty_Test_Case;
      E : aliased Empty_Test_Case.Test_Case;
      pragma Warnings (Off);
      Initial_Count : constant Count_Type := Routine_Count (E);
      pragma Warnings (On);
   begin
      Register_Routine (E, Dummy_Test_Routine'Access, "Dummy");

      Assert
        (T'Access,
         Routine_Count (E) = Initial_Count + 1,
         "Register failed to update routine count");
   end Test_Register_Routine;

   --  Register test routines to call:
   package Registration is new Framework.Test_Cases.Registration (Test_Case);
   use Registration;
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Test_Register_Routine'Access, "Test Register Routine");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Test AUnit.Test_Cases.Registration");
   end Name;

end Test_Test_Case_Registration;
