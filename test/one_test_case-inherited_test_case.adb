with AUnit.Tests.Test_Cases.Registration;

--  Test case that inherits a routine
package body One_Test_Case.Inherited_Test_Case is

   use Assertions;

   procedure Test_2 (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Test_2;

   --  Check ability to access parent and child instance-specific data.
   --  Derived test_case must be declared in a child unit:
   procedure Test_Data_Access (T : in out Test_Case) is
   begin
      Assert
        (T'Access,
         T.Parent_Data = 0 and
         T.Child_Data = 1,
         "Parent and Child data not correctly accessed");
   end Test_Data_Access;

   --  Register test routines to call.  Total test routines = 4:
   package Registration is new Framework.Test_Cases.Registration (Test_Case);
   use Registration;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      --  Register parent routines:
      Register_Tests (One_Test_Case.Test_Case (T));

      --  Register tests of derived Test_Case type:
      Register_Routine
        (T, Test_2'Access, "Test Routine 2");
      Register_Routine
        (T, Test_Data_Access'Access, "Test Data Access");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return  Format ("Inherited Test Case");
   end Name;

end One_Test_Case.Inherited_Test_Case;
