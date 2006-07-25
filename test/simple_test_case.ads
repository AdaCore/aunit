with Framework; use Framework;
with Ada_Containers; use Ada_Containers;

--  Simple test case.
package Simple_Test_Case is
   use Test_Results;

   type Test_Case is new Framework.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;

   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T : in out Test_Case);

   --  Set up?
   function Is_Set_Up (T : Test_Case) return Boolean;

   --  Torn down?
   function Is_Torn_Down (T : Test_Case) return Boolean;

   --  Test Routines:
   procedure Fail (T : in out Test_Case);
   procedure Succeed (T : in out Test_Case);
   procedure Double_Failure (T : in out Test_Case);

   function Routine_Count
     (T : Test_Case'Class)
      return Ada_Containers.Count_Type;

private
   type Test_Case is new Framework.Test_Cases.Test_Case with record
      Is_Set_Up,
      Is_Torn_Down : Boolean := False;
   end record;

end Simple_Test_Case;
