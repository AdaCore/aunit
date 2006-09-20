------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T. T E S T _ C A S E S . R E G I S T R A T I O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2006, AdaCore                   --
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

--  Test routine registration
with Ada.Unchecked_Conversion;
with System;

separate (AUnit_Framework.Tests.Test_Cases)
package body Registration is

   pragma Ada_05;

   ----------------------
   -- Register_Routine --
   ----------------------

   procedure Register_Routine
     (Test    : in out Test_Case'Class;
      Routine : Test_Routine;
      Name    : String) is

      Formatted_Name : Routine_String := (others => ' ');
      Length : constant Natural := Name'Length;
      Val : Routine_Spec;
      use Routine_Lists;

   begin
      if Length > Formatted_Name'Length then
         Formatted_Name := Name
           (Name'First .. Name'First + Formatted_Name'Length - 1);
      else
         Formatted_Name
           (Formatted_Name'First .. Formatted_Name'First + Length - 1) := Name;
      end if;

      Val  := (Routine, Formatted_Name);
      Add_Routine (Test, Val);
   end Register_Routine;

   ----------------------
   -- Register_Wrapper --
   ----------------------

   procedure Register_Wrapper
     (Test    : in out Specific_Test_Case'Class;
      Routine : access procedure (Test : in out Specific_Test_Case'Class);
      Name    : String) is

      function Conv is
        new Ada.Unchecked_Conversion (System.Address, Test_Routine);

   begin
      Register_Routine (Test_Case'Class (Test),
                        Conv (Routine.all'Address),
                        Name);
   end Register_Wrapper;

   -------------------
   -- Routine_Count --
   -------------------

   function Routine_Count (Test : Test_Case'Class) return Count_Type is
   begin
      return Routine_Lists.Length (Test.Routines);
   end Routine_Count;

end Registration;
