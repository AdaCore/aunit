with One_Test_Case;

package One_Test_Case.Inherited_Test_Case is
   type Test_Case is new One_Test_Case.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;

private

   --  Test Routines:
   procedure Test_2 (T : in out Test_Case);
   procedure Test_Data_Access (T : in out Test_Case);

   type Test_Case is new One_Test_Case.Test_Case with record
      Child_Data : Integer := 1;
   end record;
end One_Test_Case.Inherited_Test_Case;
