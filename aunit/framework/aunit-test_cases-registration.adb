------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         A U N I T . T E S T _ C A S E S . R E G I S T R A T I O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2000-2005, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by AdaCore  (http://www.adacore.com).                 --
--                                                                          --
------------------------------------------------------------------------------
--  Test routine registration
package body AUnit.Test_Cases.Registration is

   pragma Ada_05;

   --  Register a test routine.
   procedure Register_Routine
     (Test    : in out Test_Case'Class;
      Routine : access procedure (Test : in out Test_Case'Class);
      Name    : String)
   is
   begin
      Extend
        (Test.Routines,
         Routine_Spec'(Routine, new String'(Name)));
   end Register_Routine;

   --  Count of registered routines in test case:
   function Routine_Count (Test : Test_Case'Class) return Natural is
   begin
      return Count (Test.Routines);
   end Routine_Count;

end AUnit.Test_Cases.Registration;
