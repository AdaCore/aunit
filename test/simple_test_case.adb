with AUnit.Tests.Test_Cases.Registration;

--  Simple test case
package body Simple_Test_Case is
   use Assertions;

   procedure Double_Failure_Wrapper (T : in out Test_Case'Class);

   package Registration is new Framework.Test_Cases.Registration (Test_Case);
   use Registration;

   function Routine_Count (T : Test_Case'Class)
                           return Ada_Containers.Count_Type
   is
   begin
      return Registration.Routine_Count (T);
   end Routine_Count;

   procedure Set_Up (T : in out Test_Case) is
   begin
      T.Is_Set_Up := True;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      T.Is_Torn_Down := True;
   end Tear_Down;

   procedure Succeed (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Succeed;

   procedure Fail (T : in out Test_Case) is
   begin
      null;
      Assert (T'Access, False, "Failure test failed");
   end Fail;

   procedure Double_Failure_Wrapper (T : in out Test_Case'Class) is
   begin
      Double_Failure (T);
   end Double_Failure_Wrapper;

   procedure Double_Failure (T : in out Test_Case) is
      pragma Warnings (Off);
      Dummy : Boolean;
      pragma Warnings (On);
   begin
      --  Fail two assertions. Will be checked in Test_Test_Case.Test_Run
      Dummy := Assert (T'Access, False, "first failure");
      Assert (T'Access, False, "second failure");
   end Double_Failure;

   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
   begin

      Register_Routine
        (T, Succeed'Access, "Success Test");

      Register_Routine
        (T, Fail'Access, "Failure Test");

      Register_Wrapper
        (T,
         Double_Failure_Wrapper'Access,
         "Multiple assertion failures");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Dummy Test Case");
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
