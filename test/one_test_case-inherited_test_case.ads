with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with One_Test_Case;

package One_Test_Case.Inherited_Test_Case is
   type Test_Case is new One_Test_Case.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return String_Access;

private

   type Test_Case is new One_Test_Case.Test_Case with record
      Child_Data : Integer := 1;
   end record;
   

end One_Test_Case.Inherited_Test_Case;
