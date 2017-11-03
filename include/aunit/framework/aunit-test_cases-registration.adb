------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        A U N I T . T E S T _ C A S E S . R E G I S T R A T I O N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2017, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

--  Test routine registration

separate (AUnit.Test_Cases)
package body Registration is

   ----------------------
   -- Register_Routine --
   ----------------------

   procedure Register_Routine
     (Test    : in out Test_Case'Class;
      Routine : Test_Routine;
      Name    : String) is

      Formatted_Name : constant Message_String := Format (Name);
      Val : Routine_Spec;

   begin
      Val  := (Routine, Formatted_Name);
      Add_Routine (Test, Val);
   end Register_Routine;

   -------------------
   -- Routine_Count --
   -------------------

   function Routine_Count (Test : Test_Case'Class) return Count_Type is
   begin
      return Routine_Lists.Length (Test.Routines);
   end Routine_Count;

end Registration;
