with AUnit.Test_Suites; use AUnit.Test_Suites;

-- Unit tests for AUnit:
with Test_Test_Case;
with Test_Test_Case_Registration;
with Test_Test_Suite;
with Test_Lists;
function AUnit_Suite return Access_Test_Suite is
   Result : Access_Test_Suite := new Test_Suite;

begin
   Add_Test (Result, new Test_Test_Case.Test_Case);
   Add_Test (Result, new Test_Test_Case_Registration.Test_Case);
   Add_Test (Result, new Test_Test_Suite.Test_Case);
   Add_Test (Result, new Test_Lists.Test_Case);
   return Result;
end AUnit_Suite;

