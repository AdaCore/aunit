with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with  AUnit.Test_Cases;

--  Simple test case.
package Simple_Test_Case is
   type Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine:
   procedure Tear_Down (T : in out Test_Case);

   --  Set up?
   function Is_Set_Up (T : Test_Case) return Boolean;

   --  Torn down?
   function Is_Torn_Down (T : Test_Case) return Boolean;

private
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Is_Set_Up,
      Is_Torn_Down : Boolean := False;
   end record;

end Simple_Test_Case;
