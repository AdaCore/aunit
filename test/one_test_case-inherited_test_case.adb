with AUnit.Test_Cases.Registration, AUnit.Assertions;
use AUnit.Test_Cases.Registration, Aunit.Assertions;

--  Test case that inherits a routine. Overriding parent test routines
--  isn't possible.  Access to inherited parent Test_Case per-instance
--  data is.
package body One_Test_Case.Inherited_Test_Case is


   --  Test Routines:

   procedure Test_2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_2;

   -- Check ability to access parent and child instance-specific data.
   -- Downward conversion is necessary to access specific type data,
   -- and derived test_case must be declared in a child unit:
   procedure Test_Data_Access (T: in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      Assert
        (One_Test_Case.Test_Case (T).Parent_Data = 0 and
         Test_Case (T).Child_Data = 1,
         "Parent and Child data not correctly accessed");
   end Test_Data_Access;


   --  Register test routines to call.  Total test routines = 4:
   procedure Register_Tests (T : in out Test_Case) is
   begin
      -- Register all tests from parent Test_Case type:
      One_Test_Case.Register_Tests (One_Test_Case.Test_Case (T));

      -- Register one parent routine (must be declared in parent spec):
      Register_Routine (T, Test_1'Access, "Parent Test Routine");

      -- Register tests of derived Test_Case type:
      Register_Routine
        (T, Test_2'Access, "Test Routine 2");
      Register_Routine
        (T, Test_Data_Access'Access, "Test Data Access");

   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Inherited Test Case");
   end Name;

end One_Test_Case.Inherited_Test_Case;
