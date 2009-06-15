--
--  Copyright (C) 2009, AdaCore
--
with AUnit.Reporter.Text;
with Harness;

--  with Last_Chance_Handler, Dummy_SS_Get;
--  For ZFP only

procedure AUnit_Harness is
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Harness (Reporter, Timed => False);
end AUnit_Harness;
