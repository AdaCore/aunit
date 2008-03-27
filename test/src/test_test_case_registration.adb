--
--  Copyright (C) 2008, AdaCore
--
with Empty_Test_Case;
with Ada_Containers; use Ada_Containers;

with AUnit.Assertions;
--  Unit tests for AUnit.Test_Cases.Registration.
package body Test_Test_Case_Registration is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;

   procedure Dummy_Test_Routine (T : in out Test_Cases.Test_Case'Class);

   procedure Dummy_Test_Routine (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      null;
   end Dummy_Test_Routine;

   procedure Test_Register_Routine (T : in out Test_Cases.Test_Case'Class) is
      use Empty_Test_Case;
      E : aliased Empty_Test_Case.The_Test_Case;
      pragma Warnings (Off);
      Initial_Count : constant Count_Type :=
                        AUnit.Test_Cases.Registration.Routine_Count (E);
      pragma Warnings (On);
      pragma Unreferenced (T);
   begin
      Register_Routine (E, Dummy_Test_Routine'Access, "Dummy");

      Assert
        (Test_Cases.Registration.Routine_Count (E) = Initial_Count + 1,
         "Register failed to update routine count");
   end Test_Register_Routine;

   --  Register test routines to call:
   procedure Register_Tests (T : in out The_Test_Case) is
   begin
      Register_Routine
        (T, Test_Register_Routine'Access, "Test Register Routine");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : The_Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Test AUnit.Test_Cases.Registration");
   end Name;

end Test_Test_Case_Registration;
