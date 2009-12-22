------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2009, AdaCore                   --
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

with Ada_Containers;
with Ada_Containers.AUnit_Lists;
with AUnit.Tests;        use AUnit.Tests;
with AUnit.Test_Results; use AUnit.Test_Results;
--  A collection of test cases.
package AUnit.Test_Suites is

   type Test_Suite is new AUnit.Tests.Test with private;
   type Access_Test_Suite is access all Test_Suite'Class;

   procedure Add_Test (S : access Test_Suite'Class; T : access Test'Class);
   --  Add a test case or suite to this suite

   procedure Run (Suite   : access Test_Suite;
                  Options :        AUnit_Options;
                  R       : in out Result;
                  Outcome :    out Status);
   --  Run all tests collected into this suite

   function New_Suite return Access_Test_Suite;
   --  Create a new test suite

private

   use Ada_Containers;

   package Test_Lists is new Ada_Containers.AUnit_Lists (Test_Access);
   use Test_Lists;
   --  Containers for test cases and sub-suites

   type Test_Suite is new Test with record
      Tests : aliased Test_Lists.List;
   end record;

end AUnit.Test_Suites;
