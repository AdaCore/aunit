with AUnit.Test_Cases.Registration, Empty_Test_Case;
use AUnit.Test_Cases.Registration;

with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Assertions; use AUnit.Assertions;

--  Unit tests for AUnit.Test_Cases.Registration.
package body Test_Test_Case_Registration is

   --  Test Routines:

   procedure Dummy_Test_Routine is
   begin
      null;
   end Dummy_Test_Routine;

   procedure Test_Register_Routine is
      T : aliased Empty_Test_Case.Test_Case;
      Initial_Count : Natural := Routine_Count (T);
   begin
      Register_Routine (T, Dummy_Test_Routine'Access, "Dummy");

      Assert
        (Routine_Count (T) = Initial_Count + 1,
         "Register failed to update routine count");
   end Test_Register_Routine;


   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Test_Register_Routine'Access, "Test Register Routine");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Test AUnit.Test_Cases.Registration");
   end Name;

end Test_Test_Case_Registration;
