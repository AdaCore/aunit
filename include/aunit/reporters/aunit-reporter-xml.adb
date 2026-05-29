------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
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
with AUnit.Test_Info;    use AUnit.Test_Info;

--  Very simple reporter to console

package body AUnit.Reporter.XML is

   procedure Dump_Result_List (File : File_Type; L : Result_Lists.List);
   --  List failed assertions

   procedure Put_Measure is new Gen_Put_Measure_In_Seconds;
   --  Output elapsed time

   procedure Report_Test (File : File_Type; Test : Test_Result);
   --  Report a single assertion failure or unexpected exception

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (File : File_Type; L : Result_Lists.List) is

      use Result_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (File, Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   ------------
   -- Report --
   ------------

   procedure Report
     (Engine  : XML_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options)
   is
      T    : AUnit_Duration;
      File : File_Type renames Engine.File.all;

      Total_Tests    : constant Integer := Integer (Test_Count (R));
      Failures_Count : constant Integer := Integer (Failure_Count (R));
      Errors_Count   : constant Integer := Integer (Error_Count (R));
      Total_Failures : constant Integer := Failures_Count + Errors_Count;

   begin
      Put_Line (File, "<?xml version='1.0' encoding='utf-8' ?>");

      Put (File, "<TestRun");
      if Elapsed (R) /= Time_Measure.Null_Time then
         T := Get_Measure (Elapsed (R));

         Put (File, " elapsed=""");
         Put_Measure (File, T);
         Put (File, """");
      end if;
      Put_Line (File, ">");

      Put_Line (File, "<Statistics>", Indent => 1);

      Put (File, "<Tests>", Indent => 2);
      Put (File, Total_Tests, 0);
      Put_Line (File, "</Tests>");

      Put (File, "<FailuresTotal>", Indent => 2);
      Put (File, Total_Failures, 0);
      Put_Line (File, "</FailuresTotal>");

      Put (File, "<Failures>", Indent => 2);
      Put (File, Failures_Count, 0);
      Put_Line (File, "</Failures>");

      Put (File, "<Errors>", Indent => 2);
      Put (File, Errors_Count, 0);
      Put_Line (File, "</Errors>");

      Put_Line (File, "</Statistics>", Indent => 1);

      declare
         S : Result_Lists.List;
      begin
         if Total_Tests /= Total_Failures then
            Put_Line (File, "<SuccessfulTests>", Indent => 1);
            Successes (R, S);
            Dump_Result_List (File, S);
            Put_Line (File, "</SuccessfulTests>", Indent => 1);
         end if;
      end;

      if Total_Failures /= 0 then
         Put_Line (File, "<FailedTests>", Indent => 1);
         declare
            F : Result_Lists.List;
         begin
            Failures (R, F);
            Dump_Result_List (File, F);
         end;

         declare
            E : Result_Lists.List;
         begin
            Errors (R, E);
            Dump_Result_List (File, E);
         end;
         Put_Line (File, "</FailedTests>", Indent => 1);
      end if;

      Put_Line (File, "</TestRun>");
   end Report;

   -----------------
   -- Report_Test --
   -----------------

   procedure Report_Test (File : File_Type; Test : Test_Result) is
      Is_Assert : Boolean;
      T         : AUnit_Duration;
   begin
      Put (File, "<Test", Indent => 2);

      Put (File, " name=""" & Test.Test_Name.all);
      if Test.Routine_Name /= null then
         Put (File, " : ");
         Put (File, Test.Routine_Name.all);
      end if;
      Put (File, """");

      if Test.Location /= null and then Test.Location.Tested_Name /= null then
         Put
           (File,
            " tested_function=""" & Test.Location.Tested_Name.all & """");
      end if;

      if Test.Elapsed /= Time_Measure.Null_Time then
         T := Get_Measure (Test.Elapsed);

         Put (File, " elapsed=""");
         Put_Measure (File, T);
         Put (File, """");
      end if;

      if Test.Failure /= null or else Test.Error /= null then
         Put_Line (File, ">");

         if Test.Failure /= null then
            Is_Assert := True;
         else
            Is_Assert := False;
         end if;

         Put (File, "<FailureType>", Indent => 3);

         if Is_Assert then
            Put (File, "Assertion");
         else
            Put (File, "Error");
         end if;

         Put_Line (File, "</FailureType>");
         Put (File, "<Message>", Indent => 3);
         if Is_Assert then
            Put (File, Test.Failure.Message.all);
         else
            Put (File, Test.Error.Exception_Name.all);
         end if;
         Put_Line (File, "</Message>");

         if Is_Assert then
            Put_Line (File, "<Location>", Indent => 3);
            Put_Line
              (File,
               "<File>" & Test.Failure.Source_Name.all & "</File>",
               Indent => 4);
            Put (File, "<Line>", Indent => 4);
            Put (File, Integer (Test.Failure.Line), 0);
            Put_Line (File, "</Line>");
            Put_Line (File, "</Location>", Indent => 3);

         else
            Put_Line (File, "<Exception>", Indent => 3);
            Put_Line
              (File,
               "<Message>" & Test.Error.Exception_Message.all & "</Message>",
               Indent => 4);

            if Test.Error.Exception_Message /= null then
               Put_Line
                 (File,
                  "<Information>"
                  & Test.Error.Exception_Message.all
                  & "</Information>",
                  Indent => 4);
            end if;

            if Test.Error.Traceback /= null then
               Put_Line (File, "<Traceback>", Indent => 4);
               Put_Line (File, Test.Error.Traceback.all, Indent => 5);
               Put_Line (File, "</Traceback>", Indent => 4);
            end if;

            Put_Line (File, "</Exception>", Indent => 3);
         end if;
         Put_Line (File, "</Test>", Indent => 2);
      else
         Put_Line (File, "/>");
      end if;

   end Report_Test;

end AUnit.Reporter.XML;
