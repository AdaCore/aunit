--  Unit tests for AUnit:
with Test_Test_Case;
with Test_Test_Case_Registration;
with Test_Test_Suite;

package body AUnit_Suite is
   use Test_Suites;

   Result : aliased Test_Suite;
   Initialized : Boolean := False;

   Test_Case : aliased Test_Test_Case.The_Test_Case;
   Test_Case_Registration : aliased Test_Test_Case_Registration.The_Test_Case;
   Test_Suite : aliased Test_Test_Suite.The_Test_Case;

   function Suite return Access_Test_Suite is
   begin
      if not Initialized then
         Add_Test (Result'Access, Test_Case'Access);
         Add_Test (Result'Access, Test_Case_Registration'Access);
         Add_Test (Result'Access, Test_Suite'Access);
         Initialized := True;
      end if;

      return Result'Access;
   end Suite;
end AUnit_Suite;
