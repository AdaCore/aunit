------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . T E X T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2019, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.IO;           use AUnit.IO;
with AUnit.Time_Measure; use AUnit.Time_Measure;

--  Very simple reporter to console
package body AUnit.Reporter.Text is

   procedure Indent (File : File_Type; N : Natural);
   --  Print N indentations to output

   procedure Dump_Result_List (File : File_Type; L : Result_Lists.List; Prefix : String);
   --  Dump a result list

   procedure Put_Measure is new Gen_Put_Measure;
   --  Output elapsed time

   procedure Report_Test (File : File_Type; Test : Test_Result; Prefix : String);
   --  Report a single assertion failure or unexpected exception

   generic
      with procedure Get (R : Result; L : in out Result_Lists.List);
      Label : String;
      Color : String;
   procedure Report_Tests
      (Engine : Text_Reporter;
       R      : Result'Class;
       File   : File_Type);
   --  Report a series of tests

   ANSI_Def    : constant String := ASCII.ESC & "[0m";
   ANSI_Green  : constant String := ASCII.ESC & "[32m";
   ANSI_Purple : constant String := ASCII.ESC & "[35m";
   ANSI_Red    : constant String := ASCII.ESC & "[31m";

   -------------------------
   -- Set_Use_ANSI_Colors --
   -------------------------

   procedure Set_Use_ANSI_Colors
     (Engine : in out Text_Reporter;
      Value  : Boolean) is
   begin
      Engine.Use_ANSI := Value;
   end Set_Use_ANSI_Colors;

   ------------
   -- Indent --
   ------------

   procedure Indent (File : File_Type; N : Natural) is
   begin
      for J in 1 .. N loop
         Put (File, "    ");
      end loop;
   end Indent;

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (File : File_Type; L : Result_Lists.List; Prefix : String) is

      use Result_Lists;

      C : Cursor := First (L);
   begin
      if Has_Element (C) then
         New_Line (File);
      end if;

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (File, Element (C), Prefix);
         Next (C);
      end loop;
   end Dump_Result_List;

   ---------
   -- Get --
   ---------

   procedure Report_Tests
      (Engine : Text_Reporter;
       R      : Result'Class;
       File   : File_Type)
   is
      S : Result_Lists.List;
   begin
      Get (Result (R), S);
      if Engine.Use_ANSI then
         Put (File, Color);
      end if;

      Dump_Result_List (File, S, Label);

      if Engine.Use_ANSI then
         Put (File, ANSI_Def);
      end if;
   end Report_Tests;

   ---------------------
   -- Report_OK_Tests --
   ---------------------

   procedure Report_OK_Tests
      (Engine : Text_Reporter;
       R      : Result'Class)
   is
      procedure Internal is new Report_Tests (Successes, "OK", ANSI_Green);
   begin
      Internal (Engine, R, Engine.File.all);
   end Report_OK_Tests;

   procedure Report_Fail_Tests
      (Engine : Text_Reporter;
       R      : Result'Class)
   is
      procedure Internal is new Report_Tests (Failures, "FAIL", ANSI_Purple);
   begin
      Internal (Engine, R, Engine.File.all);
   end Report_Fail_Tests;

   procedure Report_Error_Tests
      (Engine : Text_Reporter;
       R      : Result'Class)
   is
      procedure Internal is new Report_Tests (Errors, "ERROR", ANSI_Red);
   begin
      Internal (Engine, R, Engine.File.all);
   end Report_Error_Tests;

   ------------
   -- Report --
   ------------

   procedure Report
     (Engine  : Text_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options)
   is
      File    : File_Type renames Engine.File.all;
      S_Count : constant Integer := Integer (Success_Count (R));
      F_Count : constant Integer := Integer (Failure_Count (R));
      E_Count : constant Integer := Integer (Error_Count (R));
      T : AUnit_Duration;
   begin

      if Options.Report_Successes then
         Report_OK_Tests    (Text_Reporter'Class (Engine), R);
      end if;

      Report_Fail_Tests  (Text_Reporter'Class (Engine), R);
      Report_Error_Tests (Text_Reporter'Class (Engine), R);

      New_Line (File);
      Put (File, "Total Tests Run:   ");
      Put (File, Integer (Test_Count (R)), 0);
      New_Line (File);
      Put (File, "Successful Tests:  ");
      Put (File, S_Count, 0);
      New_Line (File);
      Put (File, "Failed Assertions: ");
      Put (File, F_Count, 0);
      New_Line (File);
      Put (File, "Unexpected Errors: ");
      Put (File, E_Count, 0);
      New_Line (File);

      if Elapsed (R) /= Time_Measure.Null_Time then
         T := Get_Measure (Elapsed (R));

         Put (File, "Cumulative Time: ");
         Put_Measure (File, T);
         Put_Line (File, " seconds");
      end if;
   end Report;

   -----------------
   -- Report_Test --
   -----------------

   procedure Report_Test (File : File_Type; Test : Test_Result; Prefix : String) is
      T : AUnit_Duration;
   begin
      Put (File, Prefix);
      Put (File, " ");
      Put (File, Test.Test_Name.all);

      if Test.Routine_Name /= null then
         Put (File, " : ");
         Put (File, Test.Routine_Name.all);
      end if;

      if Test.Elapsed /= Time_Measure.Null_Time then
         Put (File, " (in ");
         T := Get_Measure (Test.Elapsed);
         Put_Measure (File, T);
         Put (File, ")");
      end if;

      New_Line (File);

      if Test.Failure /= null then
         Indent (File, 1);
         Put_Line (File, Test.Failure.Message.all);
         Indent (File, 1);
         Put (File, "at ");
         Put (File, Test.Failure.Source_Name.all);
         Put (File, ":");
         Put (File, Integer (Test.Failure.Line), 0);
         New_Line (File);

      elsif Test.Error /= null then
         Indent (File, 1);
         Put_Line (File, Test.Error.Exception_Name.all);

         if Test.Error.Exception_Message /= null then
            Indent (File, 1);
            Put      (File, "Exception Message: ");
            Put_Line (File, Test.Error.Exception_Message.all);
         end if;

         if Test.Error.Traceback /= null then
            Indent (File, 1);
            Put_Line (File, "Traceback:");

            declare
               From, To : Natural := Test.Error.Traceback'First;
            begin
               while From <= Test.Error.Traceback'Last loop
                  To := From;
                  while To <= Test.Error.Traceback'Last
                    and then Test.Error.Traceback (To) /= ASCII.LF
                  loop
                     To := To + 1;
                  end loop;

                  Indent (File, 2);
                  Put_Line (File, Test.Error.Traceback (From .. To - 1));
                  From := To + 1;
               end loop;
            end;
         end if;

         New_Line (File);
      end if;
   end Report_Test;

end AUnit.Reporter.Text;
