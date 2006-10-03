with Simple_Test_Case;
with Ada_Containers; use Ada_Containers;

--  Unit tests for AUnit.Test_Cases.
package body Test_Test_Case is
   use Assertions;

   Simple :  aliased Simple_Test_Case.The_Test_Case;
   R : aliased Result;

   procedure Test_Register_Tests (T : in out Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Simple_Test_Case;
      Old_Count : constant Count_Type :=
                   AUnit.Test_Cases.Registration.Routine_Count (Simple);
      Routines_In_Simple : constant := 3;
   begin
      Simple_Test_Case.Register_Tests (Simple);

      Assert
        (AUnit.Test_Cases.Registration.Routine_Count (Simple) =
           Old_Count + Routines_In_Simple,
         "Routine not properly registered");
   end Test_Register_Tests;

   procedure Test_Set_Up (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      use Simple_Test_Case;
      Was_Reset : constant Boolean := not Is_Set_Up (Simple);
   begin
      Set_Up (Simple);

      Assert
        (Was_Reset and Is_Set_Up (Simple),
         "Not set up correctly");
   end Test_Set_Up;

   procedure Test_Torn_Down (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      use Simple_Test_Case;
      Was_Reset : constant Boolean := not Is_Torn_Down (Simple);
   begin
      Tear_Down (Simple);

      Assert
        (Was_Reset and Is_Torn_Down (Simple),
         "Not torn down correctly");
   end Test_Torn_Down;

   procedure Test_Run (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      use Simple_Test_Case;
      Count  : constant Count_Type :=
                 AUnit.Test_Cases.Registration.Routine_Count (Simple);
      Old_Count : constant Count_Type := Test_Count (R);

   begin
      Run (Simple'Access, R'Access);

      Assert
        (Count  = 3,
         "Not enough routines in simple test case");

      Assert
        (Test_Count (R) = Count + Old_Count,
         "Not all requested routines were run");

      --  There are supposed to be two failed assertions for one routine
      --  in R, so we expect Count + Old_Count + 1:
      Assert
        (Success_Count (R) + Failure_Count (R) + Error_Count (R)
         = Count + Old_Count + 1,
         "Not all requested routines are recorded");

      Assert (Is_Torn_Down (Simple), "Not torn down correctly");
      Assert (Success_Count (R) = 1, "Wrong success count");
      Assert (Failure_Count (R) = 3, "Wrong failure count");
   end Test_Run;

   procedure Test_Multiple_Failures_Wrapper (T : in out The_Test_Case'Class);

   procedure Test_Multiple_Failures_Wrapper (T : in out The_Test_Case'Class)
   is
   begin
      Test_Multiple_Failures (T);

   end Test_Multiple_Failures_Wrapper;

   procedure Test_Multiple_Failures (T : in out The_Test_Case) is
      Dummy : Boolean;
      pragma Unreferenced (T, Dummy);
   begin
      Dummy := Assert (False, "expected failure 1");
      Assert (False, "expected failure 2");
      Assert (False,
              "* UNEXPECTED FAILURE * except in 'no exception' run-time case");
   end Test_Multiple_Failures;

   --  Exclude when run-time library does not support exception handling
   procedure Test_Exceptions (T : in out Test_Cases.Test_Case'Class)
   is separate;

   --  Register test routines to call:
   use AUnit.Test_Cases.Registration;

   procedure Register_Tests (T : in out The_Test_Case) is
      package Register_Specific is
        new AUnit.Test_Cases.Specific_Test_Case_Registration (The_Test_Case);
      use Register_Specific;
   begin
      Register_Routine
        (T, Test_Register_Tests'Access, "Test Routine Registration");

      Register_Routine
        (T, Test_Set_Up'Access, "Test Set Up");

      Register_Routine
        (T, Test_Torn_Down'Access, "Test Tear Down");

      Register_Routine
        (T, Test_Run'Access, "Test Run");

      Register_Wrapper
        (T,
         Test_Multiple_Failures_Wrapper'Access,
         "Test for two failed assertions");

      --  Exclude when run-time library does not support exception handling
      Register_Routine
        (T, Test_Exceptions'Access, "Test Exceptions - * Error Expected *");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : The_Test_Case) return Test_String is
   pragma Unreferenced (T);
   begin
      return  Format ("Test AUnit.Test_Cases");
   end Name;

end Test_Test_Case;
