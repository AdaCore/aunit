with AUnit.Test_Runner;
with AUnit_Suite;

procedure Timed_Harness is
   procedure Run is new AUnit.Test_Runner (AUnit_Suite);
begin
   Run;
end Timed_Harness;


