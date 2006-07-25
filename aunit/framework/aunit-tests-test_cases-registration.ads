------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T . T E S T _ C A S E S . R E G I S T R A T I O N       --
--                                                                          --
--                                 S p e c                                  --
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

--  Registration interface for test cases.
generic
   type Specific_Case is new Test_Case with private;
package AUnit.Tests.Test_Cases.Registration is

   pragma Ada_05;

   use Ada_Containers;

   procedure Register_Routine
     (Test    : in out Specific_Case;
      Routine : access procedure (Test : in out Specific_Case);
      Name    : String);
   --  Add test routine to test case

   procedure Register_Wrapper
     (Test    : in out Specific_Case'Class;
      Routine : access procedure (Test : in out Specific_Case'Class);
      Name    : String);
   --  Add wrapper for overridable test routine

   function Routine_Count (Test : Test_Case'Class) return Count_Type;
   --  Count of registered routines in test case

end AUnit.Tests.Test_Cases.Registration;
