with AUnit; use AUnit;

--  Simple test case.
package Simple_Test_Case is
   use Test_Results;

   type The_Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out The_Test_Case);

   --  Provide name identifying the test case:
   function Name (T : The_Test_Case) return Test_String;

   --  Preparation performed before each routine:
   procedure Set_Up (T : in out The_Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T : in out The_Test_Case);

   --  Set up?
   function Is_Set_Up (T : The_Test_Case) return Boolean;

   --  Torn down?
   function Is_Torn_Down (T : The_Test_Case) return Boolean;

   --  Test Routines:
   procedure Fail (T : in out Test_Cases.Test_Case'Class);
   procedure Succeed (T : in out Test_Cases.Test_Case'Class);
   procedure Double_Failure (T : in out The_Test_Case);

private
   type The_Test_Case is new AUnit.Test_Cases.Test_Case with record
      Is_Set_Up,
      Is_Torn_Down : Boolean := False;
   end record;

end Simple_Test_Case;
