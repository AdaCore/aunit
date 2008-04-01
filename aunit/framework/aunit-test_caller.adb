------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ C A L L E R                     --
--                                                                          --
--                                 S o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

package body AUnit.Test_Caller is

   ------------
   -- Create --
   ------------

   procedure Create
     (TC   : out Test_Case'Class;
      Name : String;
      Test : Test_Method)
   is
   begin
      TC.Name   := Format (Name);
      TC.Method := Test;
   end Create;

   ----------
   -- Name --
   ----------

   function Name (Test : Test_Case) return Message_String is
   begin
      return Test.Name;
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Test_Case) is
   begin
      Test.Method (Test_Fixture (Test.Fixture));
   end Run_Test;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
   begin
      Test.Fixture.Set_Up;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
   begin
      Test.Fixture.Tear_Down;
   end Tear_Down;

end AUnit.Test_Caller;
