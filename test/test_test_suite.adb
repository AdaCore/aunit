with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Assertions; use AUnit.Assertions;

with AUnit.Test_Suites; use AUnit.Test_Suites;

with Empty_Test_Case;
with One_Test_Case;
with One_Test_Case.Inherited_Test_Case;

--  Unit tests for AUnit.Test_Suites
package body Test_Test_Suite is

   --  Test Routines:
   procedure Test_No_Test_Case (T : in out AUnit.Test_Cases.Test_Case'Class) is
      S : Access_Test_Suite := new Test_Suite;
      R : Result;
   begin
      Run (S.all, R);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = 0, "Wrong number of tests recorded");
   end Test_No_Test_Case;


   procedure Test_No_Test_Routines (T : in out AUnit.Test_Cases.Test_Case'Class) is
      S :  Access_Test_Suite := new Test_Suite;
      R : Result;
   begin
      Add_Test (S, new Empty_Test_Case.Test_Case);
      Run (S.all, R);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = 0, "Wrong number of tests recorded");
   end Test_No_Test_Routines;


   procedure Test_One_Test_Case (T : in out AUnit.Test_Cases.Test_Case'Class) is
      S : Access_Test_Suite := new Test_Suite;
      R : Result;
   begin
      Add_Test (S, new One_Test_Case.Test_Case);
      Run (S.all, R);

      Assert (Test_Count (R) = 1, "Wrong number of tests run");
      Assert (Failure_Count (R) = 0, "Wrong number of failures");
      Assert (Error_Count (R) = 0, "Wrong number of unexpected errors");
      Assert (Successful (R), "Suite did not run successfully");
   end Test_One_Test_Case;

   procedure Test_Inherited_Tests (T : in out AUnit.Test_Cases.Test_Case'Class) is
      S :  Access_Test_Suite := new Test_Suite;
      R : Result;
   begin
      Add_Test (S, new One_Test_Case.Inherited_Test_Case.Test_Case);
      Run (S.all, R);

      Assert (Successful (R), "Suite did not run successfully");
      Assert (Test_Count (R) = 4, "Wrong number of tests run");
   end Test_Inherited_Tests;


   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Test_No_Test_Case'Access, "Test No Test Case");

      Register_Routine
        (T, Test_No_Test_Routines'Access, "Test No Test Routines");

      Register_Routine
        (T, Test_One_Test_Case'Access, "Test One Test Routine");

      Register_Routine
        (T, Test_Inherited_tests'Access, "Test Inherited Test Case");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Test AUnit.Test_Suites");
   end Name;

end Test_Test_Suite;
