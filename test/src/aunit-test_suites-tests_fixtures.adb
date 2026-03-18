--
--  Copyright (C) 2009-2010, AdaCore
--

with Ada.Exceptions;
with AUnit.Assertions; use AUnit.Assertions;

package body AUnit.Test_Suites.Tests_Fixtures is

   ----------
   -- Name --
   ----------

   function Name (Test : Simple_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple test case");
   end Name;

   ---------------
   -- Test_File --
   ---------------

   function Test_File (Test : Simple_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("simple_test.ads");
   end Test_File;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Test : Simple_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple.Test");
   end Package_Name;

   --------------
   -- Location --
   --------------

   function Location (Test : Simple_Test_Case) return Tested_Location is
      pragma Unreferenced (Test);
   begin
      return (AUnit.Format ("simple.ads"), 4, 4, AUnit.Format ("simple"));
   end Location;

   ------------
   -- Suffix --
   ------------

   function Suffix (Test : Simple_Test_Case) return Test_Suffix_Access is
      pragma Unreferenced (Test);
   begin
      return null;
   end Suffix;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Simple_Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Failure) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with failure");
   end Name;

   ---------------
   -- Test_File --
   ---------------

   function Test_File (Test : TC_With_Failure) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("simple_test.ads");
   end Test_File;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Test : TC_With_Failure) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple.Test");
   end Package_Name;

   --------------
   -- Location --
   --------------

   function Location (Test : TC_With_Failure) return Tested_Location is
      pragma Unreferenced (Test);
   begin
      return (AUnit.Format ("simple.ads"), 4, 4, AUnit.Format ("simple"));
   end Location;

   ------------
   -- Suffix --
   ------------

   function Suffix (Test : TC_With_Failure) return Test_Suffix_Access is
      pragma Unreferenced (Test);
   begin
      return null;
   end Suffix;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Failure) is
      pragma Unreferenced (Test);
   begin
      Assert (False, "A failed assertion");
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Two_Failures) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with 2 failures");
   end Name;

   ---------------
   -- Test_File --
   ---------------

   function Test_File (Test : TC_With_Two_Failures) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("simple_test.ads");
   end Test_File;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Test : TC_With_Two_Failures) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple.Test");
   end Package_Name;

   --------------
   -- Location --
   --------------

   function Location (Test : TC_With_Two_Failures) return Tested_Location is
      pragma Unreferenced (Test);
   begin
      return (AUnit.Format ("simple.ads"), 4, 4, AUnit.Format ("simple"));
   end Location;

   ------------
   -- Suffix --
   ------------

   function Suffix (Test : TC_With_Two_Failures) return Test_Suffix_Access is
      pragma Unreferenced (Test);
   begin
      return null;
   end Suffix;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Two_Failures) is
      pragma Unreferenced (Test);
   begin
      if not Assert (False, "A first failure") then
         Assert (False, "A second failure");
         Assert (False, "Third failure, should not appear");
      end if;
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Exception) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with exception");
   end Name;

   ---------------
   -- Test_File --
   ---------------

   function Test_File (Test : TC_With_Exception) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("simple_test.ads");
   end Test_File;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Test : TC_With_Exception) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple.Test");
   end Package_Name;

   --------------
   -- Location --
   --------------

   function Location (Test : TC_With_Exception) return Tested_Location is
      pragma Unreferenced (Test);
   begin
      return (AUnit.Format ("simple.ads"), 4, 4, AUnit.Format ("simple"));
   end Location;

   ------------
   -- Suffix --
   ------------

   function Suffix (Test : TC_With_Exception) return Test_Suffix_Access is
      pragma Unreferenced (Test);
   begin
      return null;
   end Suffix;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Exception) is
      pragma Unreferenced (Test);
   begin
      Ada.Exceptions.Raise_Exception (My_Exception'Identity, "A message");
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Setup) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with set_up/tear_down defined)");
   end Name;

   ---------------
   -- Test_File --
   ---------------

   function Test_File (Test : TC_With_Setup) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("simple_test.ads");
   end Test_File;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Test : TC_With_Setup) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple.Test");
   end Package_Name;

   --------------
   -- Location --
   --------------

   function Location (Test : TC_With_Setup) return Tested_Location is
      pragma Unreferenced (Test);
   begin
      return (AUnit.Format ("simple.ads"), 4, 4, AUnit.Format ("simple"));
   end Location;

   ------------
   -- Suffix --
   ------------

   function Suffix (Test : TC_With_Setup) return Test_Suffix_Access is
      pragma Unreferenced (Test);
   begin
      return null;
   end Suffix;


   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out TC_With_Setup) is
   begin
      if Test.Setup then
         Test.Error := True;
      end if;

      Test.Setup := True;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out TC_With_Setup) is
   begin
      if not Test.Setup then
         Test.Error := True;
      end if;

      Test.Setup := False;
   end Tear_Down;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Setup) is
   begin
      Assert (Test.Setup, "Set up not done correctly");
   end Run_Test;

end AUnit.Test_Suites.Tests_Fixtures;
