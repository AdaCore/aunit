with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with  AUnit.Test_Cases;

--  Test case with one routine.
package One_Test_Case is
   type Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

private
   --  Test_1 will be inherited in another test case:
   procedure Test_1 (T : in out AUnit.Test_Cases.Test_Case'Class);
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Parent_Data : Integer := 0;
   end record;

end One_Test_Case;
