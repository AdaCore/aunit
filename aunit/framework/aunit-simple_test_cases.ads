------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008-2009, AdaCore                  --
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

--  This type is used to implement a simple test case: define a derived type
--  that overrides the Run_Test and Name methods.
--
--  You don't usually need to use that type, but Test_Fixture/Test_Caller
--  or Test_Case instead.

with Ada_Containers.AUnit_Lists;
with AUnit.Tests;
with AUnit.Test_Results; use AUnit.Test_Results;
with GNAT.Source_Info;

package AUnit.Simple_Test_Cases is

   type Test_Case is abstract new AUnit.Tests.Test with private;
   type Test_Case_Access is access all Test_Case'Class;

   function Name (Test : Test_Case) return Message_String is abstract;
   --  Test case name

   function Routine_Name (Test : Test_Case) return Message_String;
   --  Routine name. By default return a null Message_String

   procedure Run_Test
     (Test          : in out Test_Case) is abstract;
   --  Perform the test.

   procedure Set_Up (Test : in out Test_Case);
   --  Set up performed before each test

   procedure Tear_Down (Test : in out Test_Case);
   --  Tear down performed after each test

   -----------------------
   -- Simple assertions --
   -----------------------
   --  The following subprograms provide wrapper around AUnit.Assertions
   --  to compare simple types. In case of failure, the error message will
   --  contain both the expected and actual values.
   --  These are primitive operations so that you can override them in your
   --  own types, possibly to have Source and Line point somewhere else than
   --  in the code (in the case the test code is automatically generated for
   --  instance). For such a behavior, it is enough to override the general
   --  version that takes a Condition parameter.

   procedure Assert
     (Test      : in out Test_Case;
      Condition : Boolean;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   procedure Assert
     (Test      : in out Test_Case;
      Actual    : String;
      Expected  : String;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);
   --  Specialized versions of Assert, they call the general version that
   --  takes a Condition as a parameter

   ----------------------------------------------
   --  Below are internal routines. Do not use --
   ----------------------------------------------

   procedure Register_Failure
     (S           : String;
      Source_Name : String;
      Source_Line : Natural);
   --  Record test routine failure message

   procedure Run (Test    : access Test_Case;
                  Options :        AUnit_Options;
                  R       : in out Result'Class;
                  Outcome :    out Status);
   --  Run test case. Do not override

private

   package Failure_Lists is
     new Ada_Containers.AUnit_Lists (Test_Failure);
   --  Container for failed assertion messages per routine

   type Test_Case is abstract new AUnit.Tests.Test with record
      Failures : aliased Failure_Lists.List;
   end record;

end AUnit.Simple_Test_Cases;
