with AUnit.Test_Runner;
with AUnit_Suite;
procedure Harness is
   procedure Run is new AUnit.Test_Runner (AUnit_Suite);
begin
   Run (Timed => False);
end Harness;


