with AUnit.Test_Suites; use AUnit.Test_Suites;

--  List of tests and suites to compose:
with PR_XXXX_XXX;
function Sample_Suite return Access_Test_Suite is
   Result : Access_Test_Suite := new Test_Suite;
begin
   --  You may add multiple tests or suites here:
   Add_Test (Result, new PR_XXXX_XXX.Test_Case);
   return Result;
end Sample_Suite;


