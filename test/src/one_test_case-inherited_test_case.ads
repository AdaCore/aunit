with One_Test_Case;

package One_Test_Case.Inherited_Test_Case is
   type The_Test_Case is new One_Test_Case.The_Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out The_Test_Case);

   --  Provide name identifying the test case:
   function Name (T : The_Test_Case) return Test_String;

private

   --  Test Routines:
   procedure Test_2 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Data_Access (T : in out Test_Cases.Test_Case'Class);

   type The_Test_Case is new One_Test_Case.The_Test_Case with record
      Child_Data : Integer := 1;
   end record;
end One_Test_Case.Inherited_Test_Case;
