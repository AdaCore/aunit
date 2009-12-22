--
--  Copyright (C) 2009, AdaCore
--
with AUnit.Reporter.Text;
with Harness;

procedure AUnit_Harness is
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Harness (Reporter);
end AUnit_Harness;
