------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 A U N I T _ F R A M E W O R K . T E S T S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2006, AdaCore                     --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with AUnit_Framework.Test_Results;

--  Base Test Case or Test Suite
--
--  This base type allows composition of both test cases and sub-suites into a
--  test suite (Composite pattern)

generic
   with package Results is new AUnit_Framework.Test_Results (<>);
package AUnit_Framework.Tests is

   use Results, Results.Message_Strings;

   type Test is abstract tagged limited private;
   type Test_Access is access all Test'Class;

   procedure Run (T : access Test;
                  R : Result_Access;
                  S : out Status) is abstract;
   --  Run a test case or suite

private

   type Test is abstract tagged limited null record;

end AUnit_Framework.Tests;
