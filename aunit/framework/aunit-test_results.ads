------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . T E S T _ R E S U L T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2000-2005 AdaCore                      --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------
with AUnit.Lists;

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Test reporting
package AUnit.Test_Results is

   type Result is private;

   --  Information about a test failure or error
   type Test_Failure is record
      Test_Name    : String_Access;
      Routine_Name : String_Access;
      E            : Exception_Occurrence_Access;
   end record;

   --  Information about a test success
   type Test_Success is record
      Test_Name    : String_Access;
      Routine_Name : String_Access;
   end record;

   --  Record of failures or errors
   package Success_Lists is new Lists (Test_Success);

   --  Record of failures or errors
   package Failure_Lists is new Lists (Test_Failure);

   --  Count a test run
   procedure Start_Test (R : in out Result; Subtest_Count : Natural);

   --  Record an assertion violation
   procedure Add_Success
     (R                       : in out Result;
      Test_Name, Routine_Name : String_Access);

   --  Record an assertion violation
   procedure Add_Failure
     (R                       : in out Result;
      Test_Name, Routine_Name : String_Access;
      E                       : Exception_Occurrence);

   --  Record an unexpected exception
   procedure Add_Error
     (R                       : in out Result;
      Test_Name, Routine_Name : String_Access;
      E                       : Exception_Occurrence);

   --  Set Elapsed time for reporter:
   procedure Set_Elapsed (R : in out Result; D : Duration);

   --  Number of tests run
   function Test_Count (R : Result) return Natural;

   --  Number of successes
   function Success_Count (R : Result) return Natural;

   --  Number of failures
   function Failure_Count (R : Result) return Natural;

   --  Number of errors
   function Error_Count (R : Result) return Natural;

   --  All tests successful?
   function Successful (R : Result) return Boolean;

   --  List of successful tests
   function Successes (R : Result) return Success_Lists.List;

   --  List of failed tests
   function Failures (R : Result) return Failure_Lists.List;

   --  List of error tests
   function Errors (R : Result) return Failure_Lists.List;

   --  Elapsed time for test execution:
   function Elapsed (R : Result) return Duration;

private

   type Result is record
      Tests_Run      : Natural  := 0;
      Failures_List  : Failure_Lists.List;
      Errors_List    : Failure_Lists.List;
      Successes_List : Success_Lists.List;
      Elapsed_Time   : Duration := 0.0;
   end record;

   pragma Inline (Test_Count, Failure_Count, Error_Count);

end AUnit.Test_Results;
