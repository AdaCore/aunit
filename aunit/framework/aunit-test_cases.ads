------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

--  Test case: A unit with a collection of test routines.
--  Leaf node in composite pattern.
package AUnit.Test_Cases is
   type Test_Case is abstract new Test with private;

   --  All test routines are parameterless procedures:
   type Test_Routine is access procedure;

   --  Register test methods with test suite. Each test case has its
   --  own version of this routine.
   procedure Register_Tests (Test : in out Test_Case) is abstract;

   --  Test Case name
   function Name (Test : Test_Case) return String_Access is abstract;

   --  Set up performed before each test routine
   procedure Set_Up (Test : in out Test_Case);

   --  Tear down performed after each test routine
   procedure Tear_Down (Test : in out Test_Case);

   --  Run one test case
   procedure Run (Test : in out Test_Case; R : in out Result);

private
   --  Test case initialization
   procedure Initialize (Test : in out Test_Case);

   --  Routine info used at invocation and for error recording:
   type Routine_Spec is record
      Routine : Test_Routine;
      Routine_Name : String_Access;
   end record;

   package Routine_Lists is new Lists (Routine_Spec);
   use Routine_Lists;

   type Test_Case is abstract new Test with record
      Routines : List;
   end record;
end AUnit.Test_Cases;
