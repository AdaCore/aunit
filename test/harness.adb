with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

-- List of tests and suites to run:
with Test_Test_Case;
with Test_Test_Case_Registration;
with Test_Test_Suite;
with Test_Lists;

procedure Harness is
   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;

   begin
      --  You may add multiple tests or suites here:
      Add_Test (Result, new Test_Test_Case.Test_Case);
      Add_Test (Result, new Test_Test_Case_Registration.Test_Case);
      Add_Test (Result, new Test_Test_Suite.Test_Case);
      Add_Test (Result, new Test_Lists.Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Run;
end Harness;


