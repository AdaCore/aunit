with AUnit.Test_Runner;

-- Suite for this level of tests:
with Sample_Suite;

procedure Harness is

   procedure Run is new AUnit.Test_Runner (Sample_Suite);

begin
   Run;
end Harness;


