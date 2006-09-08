------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
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

pragma Restrictions (No_Implicit_Dynamic_Code);
with AUnit_Framework.Tests.Test_Cases;

with Ada_Containers;
with Ada_Containers_Restricted_Doubly_Linked_Lists;

--  A collection of test cases.
generic
   with package Tests is new AUnit_Framework.Tests.Test_Cases (<>);
package AUnit_Framework.Tests.Test_Suites is

   use Results, Tests;

   type Test_Suite is new Test with private;
   type Access_Test_Suite is access all Test_Suite'Class;

   procedure Add_Test (S : access Test_Suite'Class; T : access Test'Class);
   --  Add a test case or suite to this suite

   procedure Run (S : access Test_Suite; R : Result_Access);
   --  Run all tests collected into this suite

private

   use Ada_Containers;

   subtype Test_Range is Count_Type range
     Count_Type'(1) .. Count_Type (Max_Tests_Per_Harness);

   package Test_Lists is
     new Ada_Containers_Restricted_Doubly_Linked_Lists (Test_Access);
   use Test_Lists;
   --  Containers for test cases and sub-suites

   type Test_Suite is new Test with record
      Tests : aliased Test_Lists.List (Count_Type (Max_Tests_Per_Harness));
   end record;

end AUnit_Framework.Tests.Test_Suites;
