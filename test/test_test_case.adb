with AUnit.Test_Cases.Registration, Simple_Test_Case;
use AUnit.Test_Cases.Registration;

with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Assertions; use AUnit.Assertions;

--  Unit tests for AUnit.Test_Cases.
package body Test_Test_Case is

   --  Test Routines:

   procedure Test_Register_Tests (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Simple : Simple_Test_Case.Test_Case;
      Old_Count : Positive := Routine_Count (Simple);
   begin
      Simple_Test_Case.Register_Tests (Simple);

      Assert
        (Routine_Count (Simple) = 2 * Old_Count,
         "Routine not properly registered");
   end Test_Register_Tests;

   procedure Test_Set_Up (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Simple :  Simple_Test_Case.Test_Case;
      use Simple_Test_Case;
      Was_Reset : Boolean := not Is_Set_Up (Simple);
   begin
      Set_Up (Simple);

      Assert
        (Was_Reset and Is_Set_Up (Simple),
         "Not set up correctly");
   end Test_Set_Up;

   procedure Test_Torn_Down (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Simple :  Simple_Test_Case.Test_Case;
      use Simple_Test_Case;
      Was_Reset : Boolean := not Is_Torn_Down (Simple);
   begin
      Tear_Down (Simple);

      Assert
        (Was_Reset and Is_Torn_Down (Simple),
         "Not torn down correctly");
   end Test_Torn_Down;

   procedure Test_Run (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use Simple_Test_Case;
      Simple :  Simple_Test_Case.Test_Case;
      R : Result;
      Count : Natural := Routine_Count (Simple);
   begin
      Run (Simple, R);

      Assert
        (Count  >= 3,
         "Not enough routines in simple test case");

      Assert
        (Test_Count (R) = Count,
         "Not all requested routines were run");

      Assert
        (Success_Count (R) + Failure_Count (R) + Error_Count (R) =
         Count,
         "Not all requested routines are recorded");

      Assert (Success_Count (R) = 1, "Wrong success count");
      Assert (Failure_Count (R) = 1, "Wrong failure count");
      Assert (Error_Count (R) = 1, "Wrong error count");

      Assert
        (Is_Torn_Down (Simple),
         "Not torn down correctly");
   end Test_Run;

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Test_Register_Tests'Access, "Test Routine Registration");

      Register_Routine
        (T, Test_Set_Up'Access, "Test Set Up");

      Register_Routine
        (T, Test_Torn_Down'Access, "Test Tear Down");

      Register_Routine
        (T, Test_Run'Access, "Test Run");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Test AUnit.Test_Cases");
   end Name;

end Test_Test_Case;
