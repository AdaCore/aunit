------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T  . A S S E R T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2010, AdaCore                   --
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

with GNAT.Source_Info;
with AUnit.Tests;
with AUnit.Test_Results;
with Ada_Containers.AUnit_Lists;

package AUnit.Assertions is

   Assertion_Error : exception;
   --  For run-time libraries that support exception handling, raised when an
   --  assertion fails in order to abandon execution of a test routine.

   type Test is abstract new AUnit.Tests.Test with private;
   --  Test is used as root type for all Test cases, but also for Test fixtures
   --  This allows easy access to all Assert procedures from user tests.
   type Test_Access is access all Test'Class;

   procedure Init_Test (T : in out Test);
   --  Init a new test

   procedure Assert
     (T         : Test;
      Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);
   --  Test "Condition" and record "Message" if false.
   --  If the Ada run-time library supports exception handling, a failed
   --  condition passed to this routine causes the calling routine to be
   --  abandoned. Otherwise, a failed assertion returns and continues the
   --  caller.
   --  These are primitive operations so that you can override them in your
   --  own types, possibly to have Source and Line point somewhere else than
   --  in the code (in the case the test code is automatically generated for
   --  instance). For such a behavior, it is enough to override the general
   --  version that takes a Condition parameter.

   function Assert
     (T         : Test;
      Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line) return Boolean;
   --  Functional version to allow the calling routine to decide whether to
   --  continue or abandon the execution.
   --  This is now obsolete and will be removed in later versions of AUnit.

   type Throwing_Exception_Proc is access procedure;

   procedure Assert_Exception
     (T       : Test;
      Proc    : Throwing_Exception_Proc;
      Message : String;
      Source  : String := GNAT.Source_Info.File;
      Line    : Natural := GNAT.Source_Info.Line);
   --  Test that Proc throws an exception and record "Message" if not.
   --  This is now obsolete and will be removed in later versions of AUnit.
   --  Please use the dispatching call below, using Test as dispatching
   --  argument

   -----------------------
   -- Simple assertions --
   -----------------------
   --  The following subprograms provide specialized version of Assert
   --  to compare simple types. In case of failure, the error message will
   --  contain both the expected and actual values.

   procedure Assert
     (T         : Test;
      Actual    : String;
      Expected  : String;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);
   --  Specialized versions of Assert, they call the general version that
   --  takes a Condition as a parameter

   -----------------------------
   -- OBSELESCENT SUBPROGRAMS --
   -----------------------------

   --  The following Assert subprograms are now obsolescent.
   --  The reason behind this decision is that the following Assert methods
   --  rely on global variables (Replacing the missing 'Test' parameter).
   --  Such global variable is of course incompatible with any multitasking
   --  use, so should not be used in a multitasking environment.

   procedure Assert
     (Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);
   pragma Obsolescent;
   --  Test "Condition" and record "Message" if false.
   --  If the Ada run-time library supports exception handling, a failed
   --  condition passed to this routine causes the calling routine to be
   --  abandoned. Otherwise, a failed assertion returns and continues the
   --  caller.
   --  This is now obsolete and will be removed in later versions of AUnit.
   --  Please use the dispatching call below, using Test as dispatching
   --  argument

   function Assert
     (Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line) return Boolean;
   pragma Obsolescent;
   --  Functional version to allow the calling routine to decide whether to
   --  continue or abandon the execution.
   --  This is now obsolete and will be removed in later versions of AUnit.
   --  Please use the dispatching call below, using Test as dispatching
   --  argument

   procedure Assert_Exception
     (Proc    : Throwing_Exception_Proc;
      Message : String;
      Source  : String := GNAT.Source_Info.File;
      Line    : Natural := GNAT.Source_Info.Line);
   pragma Obsolescent;
   --  Test that Proc throws an exception and record "Message" if not.
   --  This is now obsolete and will be removed in later versions of AUnit.
   --  Please use the dispatching call below, using Test as dispatching
   --  argument

   ------------------------------------------------------------
   --  The following declarations are for internal use only  --
   ------------------------------------------------------------

   procedure Clear_Failures (T : Test);
   --  Clear all failures related to T

   function Has_Failures (T : Test) return Boolean;
   --  The number of failures reported by test

   type Failure_Iter is private;
   --  Iterator used to retrieve failures.

   function First_Failure (T : Test) return Failure_Iter;
   function Has_Failure (I : Failure_Iter) return Boolean;
   function Get_Failure
     (I : Failure_Iter) return AUnit.Test_Results.Test_Failure;
   procedure Next (I : in out Failure_Iter);
   --  Failures list handling

   --  The following is used for the non-dispatching Assert methods.
   --  This uses global variables, and thus is incompatible with multitasking.
   function Current_Test return Test_Access;
   procedure Set_Current_Test (T : Test_Access);

   procedure Copy_Id (From : Test'Class; To : in out Test'Class);
   --  Copy From's Id to To so that failures reported via To are identified as
   --  belonging to From.

private
   use AUnit.Test_Results;

   type Test_Id is new Natural;
   Null_Id : constant Test_Id := 0;

   type Failure_Elt is record
      Failure : Test_Failure;
      Id      : Test_Id := Null_Id;
   end record;

   package Failure_Lists is
     new Ada_Containers.AUnit_Lists (Failure_Elt);
   --  Container for failed assertion messages per routine

   type Failure_Iter is new Failure_Lists.Cursor;

   type Test is abstract new AUnit.Tests.Test with record
      Id : Test_Id;
   end record;

end AUnit.Assertions;
