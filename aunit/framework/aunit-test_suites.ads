------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2000 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------
with AUnit.Tests; use AUnit.Tests;
with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Lists;

--  A collection of Test cases.
--  Internal node in Composite pattern.
package AUnit.Test_Suites is

   type Test_Suite is new Test with private;

   type Access_Test_Suite is access all Test_Suite;

   --  Run all tests collected into this suite:
   procedure Run (S : in out Test_Suite; R : in out Result);

   --  Add a test case or sub-suite into this suite:
   procedure Add_Test (S : access Test_Suite; T : access Test'Class);

private

   --  List of sub-suites and test cases:
   package Test_Lists is new Lists (Test'Class);
   use Test_Lists;

   type Test_Suite is new Test with record
      Tests : List;
   end record;
end AUnit.Test_Suites;
