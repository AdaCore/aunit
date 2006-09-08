--  Unit tests for AUnit:
with Test_Test_Case;
with Test_Test_Case_Registration;
with Test_Test_Suite;

package body AUnit_Suite is
   use Test_Suites;

   Result : aliased Test_Suite;

   Test_Case : aliased Test_Test_Case.Test_Case;
   Test_Case_Registration : aliased Test_Test_Case_Registration.Test_Case;
   Test_Suite : aliased Test_Test_Suite.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case'Access);
      Add_Test (Result'Access, Test_Case_Registration'Access);
      Add_Test (Result'Access, Test_Suite'Access);
      return Result'Access;
   end Suite;
end AUnit_Suite;
