with AUnit.Test_Cases.Registration;
use AUnit.Test_Cases.Registration;

with AUnit.Assertions; use AUnit.Assertions;

--  Simple test case
package body Simple_Test_Case is

   procedure Set_Up (T : in out Test_Case) is
   begin
      T.Is_Set_Up := True;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      T.Is_Torn_Down := True;
   end Tear_Down;


   --  Test Routines:
   procedure Succeed (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end Succeed;

   procedure Fail (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
      Assert (False, "Failure test failed");
   end Fail;

   procedure Error (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      raise Constraint_Error;
   end Error;

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin

      Register_Routine
        (T, Succeed'Access, "Success Test");

      Register_Routine
        (T, Fail'Access, "Failure Test");

      Register_Routine
        (T, Error'Access, "Error Test");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return String_Access is
   begin
      return  new String'("Dummy Test Case");
   end Name;

   --  Set up?
   function Is_Set_Up (T : Test_Case) return Boolean is
   begin
      return T.Is_Set_Up;
   end Is_Set_Up;

   --  Torn down?
   function Is_Torn_Down (T : Test_Case) return Boolean is
   begin
      return T.Is_Torn_Down;
   end Is_Torn_Down;


end Simple_Test_Case;
