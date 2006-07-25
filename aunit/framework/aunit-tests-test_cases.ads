------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
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

with Ada_Containers;
with Ada_Containers_Restricted_Doubly_Linked_Lists;

--  Test case: a collection of test routines
generic
   Max_Routines_Per_Test : Natural;
   Max_Failures_Per_Harness : Natural;
   Max_Failure_Message_Size : Natural;
package AUnit.Tests.Test_Cases is

   type Test_Case is abstract new Test with private;
   type Test_Case_Access is access all Test_Case'Class;

   type Test_Routine is access procedure (Test : in out Test_Case'Class);

   Assertion_Error : exception;
   --  For run-time libraries that support exception handling, raised when an
   --  assertion fails in order to abandon execution of a test routine

   type Routine_Spec is record
      Routine      : Test_Routine;
      Routine_Name : Routine_String;
   end record;

   procedure Add_Routine (T : in out Test_Case'Class; Val : Routine_Spec);

   function Name (Test : Test_Case) return Test_String is abstract;
   --  Test case name

   procedure Register_Failure (T : access Test_Case'Class; S : String);
   --  Record test routine failure message

   procedure Register_Tests (Test : in out Test_Case) is abstract;
   --  Register test methods with test suite

   procedure Run (Test : access Test_Case; R : Result_Access);
   --  Run test case

   procedure Set_Name (Test : in out Test_Case'Class; Name : Test_String);
   --  Set name of test

   procedure Set_Up (Test : in out Test_Case);
   --  Set up performed before each test routine

   procedure Set_Up_Case (Test : in out Test_Case);
   --  Set up performed before each test case (set of test routines)

   procedure Tear_Down (Test : in out Test_Case);
   --  Tear down performed after each test routine

   procedure Tear_Down_Case (Test : in out Test_Case);
   --  Tear down performed after each test case

private

   type Routine_Access is access all Routine_Spec;
   --  Test routine description

   subtype Routine_Range is Natural range 1 .. Max_Routines_Per_Test;

   package Routine_Lists is
     new Ada_Containers_Restricted_Doubly_Linked_Lists (Routine_Spec);
   --  Container for test routines

   subtype Message_String is String (1 .. Max_Failure_Message_Size);
   --  Failure message

   package Message_Lists is
     new Ada_Containers_Restricted_Doubly_Linked_Lists (Message_String);
   --  Container for failed assertion messages per routine

   use Ada_Containers;

   type Test_Case is abstract new Test with record
      Name     : Test_String;
      Routines : aliased Routine_Lists.List
        (Count_Type (Max_Routines_Per_Test));
      Failures : aliased Message_Lists.List
        (Count_Type (Max_Failures_Per_Harness));
   end record;

end AUnit.Tests.Test_Cases;
