------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . T E S T _ R E S U L T S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2000-2005, AdaCore                     --
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
--  Record test results.
package body AUnit.Test_Results is

   --  Increment count by number of test routines in a case
   procedure Start_Test
     (R : in out Result; Subtest_Count : Natural) is
   begin
      R.Tests_Run := R.Tests_Run + Subtest_Count;
   end Start_Test;

   --  Record an assertion violation
   procedure Add_Success
     (R : in out Result; Test_Name, Routine_Name : String_Access) is
   begin
      Success_Lists.Extend
        (R.Successes_List,
         Test_Success'(Test_Name, Routine_Name));
   end Add_Success;

   --  Record a test failure
   procedure Add_Failure
     (R : in out Result; Test_Name, Routine_Name : String_Access;
      E : Exception_Occurrence) is
   begin
      Failure_Lists.Extend
        (R.Failures_List,
         Test_Failure'(Test_Name, Routine_Name, Save_Occurrence (E)));
   end Add_Failure;

   --  Record a test error
   procedure Add_Error
     (R : in out Result; Test_Name, Routine_Name : String_Access;
      E : Exception_Occurrence) is
   begin
      Failure_Lists.Extend
        (R.Errors_List,
         Test_Failure'(Test_Name, Routine_Name, Save_Occurrence (E)));
   end Add_Error;

   --  Set Elapsed time for reporter:
   procedure Set_Elapsed (R : in out Result; D : Duration) is
   begin
      R.Elapsed_Time := D;
   end Set_Elapsed;

   --  Total tests run
   function Test_Count (R : Result) return Natural is
   begin
      return R.Tests_Run;
   end Test_Count;

   --  Number of successes
   function Success_Count (R : Result)  return Natural is
   begin
      return Success_Lists.Count (R.Successes_List);
   end Success_Count;

   --  Number of failures
   function Failure_Count (R : Result) return Natural is
   begin
      return Failure_Lists.Count (R.Failures_List);
   end Failure_Count;

   --  Number of errors
   function Error_Count (R : Result) return Natural is
   begin
      return Failure_Lists.Count (R.Errors_List);
   end Error_Count;

   --  All tests successful?
   function Successful (R : Result) return Boolean is
   begin
      return Success_Count (R) = Test_Count (R);
   end Successful;

   --  List of successful tests
   function Successes (R : Result) return Success_Lists.List is
   begin
      return R.Successes_List;
   end Successes;

   --  List of failed tests
   function Failures (R : Result) return Failure_Lists.List is
   begin
      return R.Failures_List;
   end Failures;

   --  List of error tests
   function Errors (R : Result) return Failure_Lists.List is
   begin
      return R.Errors_List;
   end Errors;

   --  Elapsed time for test execution:
   function Elapsed (R : Result) return Duration is
   begin
      return R.Elapsed_Time;
   end Elapsed;

end AUnit.Test_Results;
