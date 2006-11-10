with AUnit; use AUnit;

--  List of tests and suites to compose:
with PR_XXXX_XXX;
function Sample_Suite return Test_Suites.Access_Test_Suite is
   use Test_Suites;
   Result : Access_Test_Suite := new Test_Suite;
   Test_Case : aliased PR_XXXX_XXX.Test_Case;
begin
   --  You may add multiple tests or suites here:
   Add_Test (Result, Test_Case'Access);
   return Result;
end Sample_Suite;


