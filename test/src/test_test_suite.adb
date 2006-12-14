with Empty_Test_Case;
with One_Test_Case;
with One_Test_Case.Inherited_Test_Case;
with Ada_Containers; use Ada_Containers;
with AUnit_Framework;

--  Unit tests for AUnit.Test_Suites
package body Test_Test_Suite is
   use Assertions, Test_Suites;

   S : aliased Test_Suite;
   R : aliased Result;
   O : aliased One_Test_Case.The_Test_Case;
   E : aliased Empty_Test_Case.The_Test_Case;
   I : aliased One_Test_Case.Inherited_Test_Case.The_Test_Case;

   procedure Test_No_Test_Case (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Outcome : AUnit_Framework.Status;
   begin
      Run (S'Access, R'Access, Outcome);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = 0, "Wrong number of tests recorded");
   end Test_No_Test_Case;

   procedure Test_No_Test_Routines (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Outcome : AUnit_Framework.Status;
   begin
      Add_Test (S'Access, E'Access);
      Run (S'Access, R'Access, Outcome);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = 0, "Wrong number of tests recorded");
   end Test_No_Test_Routines;

   procedure Test_One_Test_Case (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Outcome : AUnit_Framework.Status;
   begin
      Add_Test (S'Access, O'Access);
      Run (S'Access, R'Access, Outcome);

      Assert (Test_Count (R) = 1, "Wrong number of tests run");
      Assert (Failure_Count (R) = 0, "Wrong number of failures");
      Assert (Successful (R), "Suite did not run successfully");
   end Test_One_Test_Case;

   procedure Test_Inherited_Tests (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Old_Count : constant Count_Type := Test_Count (R);
      Outcome   : AUnit_Framework.Status;

   begin
      Add_Test (S'Access, I'Access);
      Run (S'Access, R'Access, Outcome);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = Old_Count + 4,
              "Wrong number of tests run");
   end Test_Inherited_Tests;

   --  Register test routines to call:
   use AUnit.Test_Cases.Registration;

   procedure Register_Tests (T : in out The_Test_Case) is
   begin
      Register_Routine
        (T, Test_No_Test_Case'Access, "Test No Test Case");

      Register_Routine
        (T, Test_No_Test_Routines'Access, "Test No Test Routines");

      Register_Routine
        (T, Test_One_Test_Case'Access, "Test One Test Routine");

      Register_Routine
        (T, Test_Inherited_Tests'Access, "Test Inherited Test Case");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : The_Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return  Format ("Test AUnit.Test_Suites");
   end Name;

end Test_Test_Suite;
