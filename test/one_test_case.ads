with AUnit; use AUnit;

--  Test case with one routine.
package One_Test_Case is
   use Test_Results;

   type Test_Case is new AUnit.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;

private
   --  Test_1 will be inherited in another test case:
   procedure Test_1 (T : in out Test_Case);
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Parent_Data : Integer := 0;
   end record;

end One_Test_Case;
