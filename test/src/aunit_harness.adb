--
--  Copyright (C) 2009, AdaCore
--

with AUnit.Reporter.Text;
with AUnit.Tests;          use AUnit.Tests;
with AUnit.Test_Filters;   use AUnit.Test_Filters;
with Harness;

procedure AUnit_Harness is
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Filter   : aliased AUnit.Test_Filters.Name_Filter;
   Options  : AUnit_Options :=
      (Global_Timer => False, Test_Case_Timer => False, Filter => null);
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Harness (Reporter, Options);

   --  Test the filter
   --  This filter should be initialized from the command line arguments. In
   --  this example, we don't do it to support limited runtimes with no support
   --  for Ada.Command_Line

   Options.Filter := Filter'Unchecked_Access;
   Set_Name (Filter, "Test AUnit.Test_Cases : Test Routine Registration");
   Harness (Reporter, Options);

end AUnit_Harness;
