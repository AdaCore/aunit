------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ R E S U L T S                   --
--                                                                          --
--                                 B o d y                                  --
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

--  Record test results.
package body AUnit_Framework.Test_Results is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Swap (Left, Right : in out Error_Lists.List);
   procedure Swap (Left, Right : in out Failure_Lists.List);
   procedure Swap (Left, Right : in out Success_Lists.List);
   procedure Transfer (Target : in out Error_Lists.List;
                       Source : in out Error_Lists.List);
   procedure Transfer (Target : in out Failure_Lists.List;
                       Source : in out Failure_Lists.List);
   procedure Transfer (Target : in out Success_Lists.List;
                       Source : in out Success_Lists.List);

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (R : in out Result;
      Test_Name      : Test_String;
      Routine_Name   : Routine_String;
      Message        : Message_String) is

      Val : constant Test_Failure := (Test_Name, Routine_Name, Message);
      use Error_Lists;

   begin
      Append (R.Errors_List, Val);
   end Add_Error;

   -----------------
   -- Add_Failure --
   -----------------

   procedure Add_Failure
     (R : in out Result;
      Test_Name      : Test_String;
      Routine_Name   : Routine_String;
      Message        : Message_String) is

      Val : constant Test_Failure := (Test_Name, Routine_Name, Message);
      use Failure_Lists;

   begin
      Append (R.Failures_List, Val);
   end Add_Failure;

   -----------------
   -- Add_Success --
   -----------------

   procedure Add_Success
     (R                       : in out Result;
      Test_Name               : Test_String;
      Routine_Name            : Routine_String) is

      Val : constant Test_Success := (Test_Name, Routine_Name);
      use Success_Lists;

   begin
      Append (R.Successes_List, Val);
   end Add_Success;

   -----------------
   -- Set_Elapsed --
   -----------------

   procedure Set_Elapsed (R : in out Result;
                          T : Time_Measure.Time) is
   begin
      R.Elapsed_Time := T;
   end Set_Elapsed;

   -----------------
   -- Error_Count --
   -----------------

   function Error_Count (R : Result) return Errors_Range is
   begin
      return Error_Lists.Length (R.Errors_List);
   end Error_Count;

   ------------
   -- Errors --
   ------------

   procedure Errors
     (R : in out Result;
      E : in out Error_Lists.List)
   is
   begin
      Swap (E, R.Errors_List);
   end Errors;

   -------------------
   -- Failure_Count --
   -------------------

   function Failure_Count (R : Result) return Failures_Range is
   begin
      return Failure_Lists.Length (R.Failures_List);
   end Failure_Count;

   --------------
   -- Failures --
   --------------

   procedure Failures
     (R : in out Result;
      F : in out Failure_Lists.List)
   is
   begin
      Swap (F, R.Failures_List);
   end Failures;

   -------------
   -- Elapsed --
   -------------

   function Elapsed (R : Result) return Time_Measure.Time is
   begin
      return R.Elapsed_Time;
   end Elapsed;

   ------------
   -- Format --
   ------------

   function Format (Name : String) return Test_String is

      Length : constant Natural := Name'Length;
      Result : String (1 .. Test_String'Length) := (others => ' ');

   begin
      if Length > Result'Length then
         Result := Name (Name'First .. Name'First + Result'Length - 1);
      else
         Result (Result'First .. Result'First + Length - 1) := Name;
      end if;

      return Test_String (Result);
   end Format;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (R : in out Result; Subtest_Count : Count_Type) is
   begin
      R.Tests_Run := R.Tests_Run + Subtest_Count;
   end Start_Test;

   -------------------
   -- Success_Count --
   -------------------

   function Success_Count (R : Result)  return Successes_Range is
   begin
      return Success_Lists.Length (R.Successes_List);
   end Success_Count;

   ---------------
   -- Successes --
   ---------------

   procedure Successes (R : in out Result; S : in out Success_Lists.List) is
   begin
      Swap (S, R.Successes_List);
   end Successes;

   ----------------
   -- Successful --
   ----------------

   function Successful (R : Result) return Boolean is
   begin
      return Success_Count (R) = Test_Count (R);
   end Successful;

   ----------
   -- Swap --
   ----------

   procedure Swap (Left, Right : in out Error_Lists.List) is

      use Error_Lists;
      Temp : List (Count_Type (Errors_Range'Last));

   begin
      Transfer (Temp, Left);
      Transfer (Left, Right);
      Transfer (Right, Temp);
   end Swap;

   procedure Swap (Left, Right : in out Failure_Lists.List) is

      use Failure_Lists;
      Temp : List (Count_Type (Failures_Range'Last));

   begin
      Transfer (Temp, Left);
      Transfer (Left, Right);
      Transfer (Right, Temp);
   end Swap;

   procedure Swap (Left, Right : in out Success_Lists.List) is

      use Success_Lists;
      Temp : List (Count_Type (Successes_Range'Last));

   begin
      Transfer (Temp, Left);
      Transfer (Left, Right);
      Transfer (Right, Temp);
   end Swap;

   ----------------
   -- Test_Count --
   ----------------

   function Test_Count (R : Result) return Ada_Containers.Count_Type is
   begin
      return Ada_Containers.Count_Type (R.Tests_Run);
   end Test_Count;

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (Target : in out Error_Lists.List;
      Source : in out Error_Lists.List)
   is
      use Error_Lists;

   begin
      Clear (Target);
      Assign (Target, Source);
      Clear (Source);
   end Transfer;

   procedure Transfer
     (Target : in out Failure_Lists.List;
      Source : in out Failure_Lists.List)
   is
      use Failure_Lists;

   begin
      Clear (Target);
      Assign (Target, Source);
      Clear (Source);
   end Transfer;

   procedure Transfer
     (Target : in out Success_Lists.List;
      Source : in out Success_Lists.List)
   is
      use Success_Lists;

   begin
      Clear (Target);
      Assign (Target, Source);
      Clear (Source);
   end Transfer;

   procedure Clear (R : in out Result) is
   begin
      R.Tests_Run    := 0;
      R.Elapsed_Time := Time_Measure.Null_Time;
      Error_Lists.Clear (R.Errors_List);
      Failure_Lists.Clear (R.Failures_List);
      Success_Lists.Clear (R.Successes_List);
   end Clear;

end AUnit_Framework.Test_Results;
