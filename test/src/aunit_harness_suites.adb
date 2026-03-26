--
--  Copyright (C) 2009-2026, AdaCore
--

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;

with AUnit_Suite; use AUnit_Suite;

procedure AUnit_Harness_Suites is

   procedure Harness is new AUnit.Run.Test_Runner (Suite_Suites);
   --  The full test harness

   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Options  : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => True,
      Capture_Standard => False,
      Report_Successes => True,
      Filter           => null);
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Harness (Reporter, Options);
end AUnit_Harness_Suites;
