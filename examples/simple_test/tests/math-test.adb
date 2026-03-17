--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Math.Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Math package");
   end Name;

   function Test_File (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("/tests/math_suite.ads");
   end Test_File;

   function Package_Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Math_Suite");
   end Package_Name;

   function Location (T : Test) return Tested_Location is
      pragma Unreferenced (T);
   begin
      return
        (Tested_File   => AUnit.Format ("math.ads"),
         Tested_Line   => 8,
         Tested_Column => 4,
         Tested_Name   => AUnit.Format ("""+"""));
   end Location;

   function Suffix (T : Test) return Test_Suffix_Access is
      pragma Unreferenced (T);
   begin
      return null;
   end Suffix;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := 5;
      I2 : constant Int := 3;
   begin
      Assert (I1 + I2 = 8, "Incorrect result after addition");
      Assert (I1 - I2 = 2, "Incorrect result after subtraction");
   end Run_Test;

end Math.Test;
