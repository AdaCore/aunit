with AUnit.Reporter.XML;
with Harness;

--  with Last_Chance_Handler, Dummy_SS_Get;
--  For ZFP only

procedure AUnit_Harness is
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Harness (Reporter);
end AUnit_Harness;
