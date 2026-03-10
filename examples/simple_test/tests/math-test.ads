--
--  Copyright (C) 2008, AdaCore
--
with AUnit;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Info; use AUnit.Test_Info;

package Math.Test is

   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   function Name (T : Test) return AUnit.Message_String;
   function Test_File (T : Test) return AUnit.Message_String;
   function Package_Name (T : Test) return AUnit.Message_String;
   function Location (T : Test) return Tested_Location;
   function Suffix (T : Test) return Test_Suffix_Access;

   procedure Run_Test (T : in out Test);

end Math.Test;
