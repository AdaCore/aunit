--
--  Copyright (C) 2009-2026, AdaCore
--

with AUnit.Options;
with AUnit.Reporter.JUnit;
with AUnit.Run;
with AUnit.Test_Filters; use AUnit.Test_Filters;

with AUnit_Suite; use AUnit_Suite;

procedure AUnit_Harness_Cases_Filter is

   procedure Harness is new AUnit.Run.Test_Runner (Suite_Cases);
   --  The full test harness

   Reporter : AUnit.Reporter.JUnit.JUnit_Reporter;
   Filter   : aliased AUnit.Test_Filters.Name_Filter;
   Options  : AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => True,
      Capture_Standard => False,
      Report_Successes => True,
      Filter           => null);
begin
   --  Test the filter
   --  This filter should be initialized from the command line arguments. In
   --  this example, we don't do it to support limited runtimes with no support
   --  for Ada.Command_Line

   Options.Filter := Filter'Unchecked_Access;
   Set_Name (Filter, "Test Routines");
   Harness (Reporter, Options);

end AUnit_Harness_Cases_Filter;
