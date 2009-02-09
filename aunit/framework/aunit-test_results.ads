------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ R E S U L T S                   --
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

pragma Ada_2005;

with Ada_Containers;
with Ada_Containers.AUnit_Lists;

with AUnit.Time_Measure;

--  Test reporting.
--
package AUnit.Test_Results is

   type Result is limited private;
   type Result_Access is access all Result;
   --  Record result. A result object is associated with the execution of a
   --  top-level test suite.

   type Test_Failure is record
      Message      : Message_String;
      Source_Name  : Message_String;
      Line         : Natural;
   end record;
   type Test_Failure_Access is access all Test_Failure;
   --  Description of a test routine failures and unexpected exceptions
   --  Unexpected exceptions are only logged when exception handling is
   --  available in the run-time library

   type Test_Result is record
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Failure      : Test_Failure_Access;
      Error        : Test_Failure_Access;
   end record;
   --  Decription of a test routine result

   use Ada_Containers;

   package Result_Lists is new Ada_Containers.AUnit_Lists (Test_Result);
   --  Containers for all test results

   procedure Add_Error
     (R                       : in out Result;
      Test_Name               : Message_String;
      Routine_Name            : Message_String;
      Failure                 : Test_Failure);
   --  Record an unexpected exception

   procedure Add_Failure
     (R                       : in out Result;
      Test_Name               : Message_String;
      Routine_Name            : Message_String;
      Failure                 : Test_Failure);
   --  Record a test routine failure

   procedure Add_Success
     (R                       : in out Result;
      Test_Name               : Message_String;
      Routine_Name            : Message_String);
   --  Record a test routine success

   procedure Set_Elapsed (R : in out Result;
                          T : Time_Measure.Time);
   --  Set Elapsed time for reporter:

   function Error_Count (R : Result) return Count_Type;
   --  Number of routines with unexpected exceptions

   procedure Errors (R : in out Result;
                     E : in out Result_Lists.List);
   --  List of routines with unexpected exceptions

   function Failure_Count (R : Result) return Count_Type;
   --  Number of failed routines

   procedure Failures (R : in out Result;
                       F : in out Result_Lists.List);
   --  List of failed routines

   function Elapsed (R : Result) return Time_Measure.Time;
   --  Elapsed time for test execution:

   procedure Start_Test (R : in out Result; Subtest_Count : Count_Type);
   --  Set count for a test run

   function Success_Count (R : Result) return Count_Type;
   --  Number of successful routines

   procedure Successes (R : in out Result;
                        S : in out Result_Lists.List);
   --  List of successful routines

   function Successful (R : Result) return Boolean;
   --  All routines successful?

   function Test_Count (R : Result) return Ada_Containers.Count_Type;
   --  Number of routines run

   procedure Clear (R : in out Result);
   --  Clear the results

private

   pragma Inline (Errors, Failures, Successes);

   type Result is limited record
      Tests_Run      : Count_Type := 0;
      Result_List    : Result_Lists.List;
      Elapsed_Time   : Time_Measure.Time := Time_Measure.Null_Time;
   end record;

end AUnit.Test_Results;
