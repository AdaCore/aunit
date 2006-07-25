with  Framework; use Framework;
with Ada_Containers; use Ada_Containers;

--  Test case with no routines.
package Empty_Test_Case is
   pragma Ada_05;
   use Test_Results;

   type Test_Case is new Framework.Test_Cases.Test_Case with private;

   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);

   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Test_String;

   function Routine_Count (T : Test_Case'Class) return Count_Type;

   procedure Register_Routine
     (T : in out Test_Case;
      Routine : access procedure (Test : in out Test_Case);
      Name : String);

private

   type Test_Case is new Framework.Test_Cases.Test_Case with null record;

end Empty_Test_Case;
