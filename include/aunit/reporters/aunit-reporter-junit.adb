------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . J U N I T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                      Copyright (C) 2020, AdaCore                         --
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

with AUnit.IO;              use AUnit.IO;
with AUnit.Time_Measure;    use AUnit.Time_Measure;

package body AUnit.Reporter.JUnit is
   procedure Put_Special_Chars (File : AUnit.IO.File_Type; S : String);
   procedure Print_System_Out
     (File : AUnit.IO.File_Type; Output : Message_String; Indent : Natural);
   function Get_Combined_Time
     (S : Result_Lists.List; E : Result_Lists.List; F : Result_Lists.List)
      return AUnit_Duration;

   procedure Put_Special_Chars (File : AUnit.IO.File_Type; S : String) is
   begin
      for C of S loop
         case C is
            when '"'    =>
               Put (File, "&quot;");

            when '<'    =>
               Put (File, "&lt;");

            when '>'    =>
               Put (File, "&gt;");

            when '&'    =>
               Put (File, "&amp;");

            when others =>
               Put (File, (1 => C));
         end case;
      end loop;
   end Put_Special_Chars;

   procedure Print_System_Out
     (File : AUnit.IO.File_Type; Output : Message_String; Indent : Natural) is
   begin
      if Output /= null then
         Put (File, "<system-out>", Indent);
         Put (File, "<![CDATA[");
         Put (File, Output.all);
         Put (File, "]]>");
         Put_Line (File, "</system-out>");
      end if;
   end Print_System_Out;

   procedure Print_System_Err
     (File : AUnit.IO.File_Type; Error : Message_String; Indent : Natural) is
   begin
      if Error /= null then
         Put (File, "<system-err>", Indent);
         Put (File, "<![CDATA[");
         Put (File, Error.all);
         Put (File, "]]>");
         Put_Line (File, "</system-err>");
      end if;
   end Print_System_Err;
   procedure Put_Measure is new Gen_Put_Measure_In_Seconds;

   function Get_Combined_Time
     (S : Result_Lists.List; E : Result_Lists.List; F : Result_Lists.List)
      return AUnit_Duration
   is
      Duration_Sum : AUnit_Duration;

      use Result_Lists;

      procedure Get_Time (R : Result_Lists.List) is
         C : Cursor := First (R);
      begin
         if Has_Element (C) then
            Duration_Sum := Get_Measure (Result_Lists.Element (C).Elapsed);
            Next (C);
         end if;
         --  Use the first test result to initialize the duration_sum variable.

         while Has_Element (C) loop
            Duration_Sum :=
              Duration_Sum + Get_Measure (Result_Lists.Element (C).Elapsed);
            Next (C);
         end loop;
      end Get_Time;
   begin

      Get_Time (S);
      Get_Time (E);
      Get_Time (F);
      return Duration_Sum;
   end Get_Combined_Time;

   procedure Report_Test (File : AUnit.IO.File_Type; Test : Test_Result) is
   begin
      Put (File, "<testcase name=""", Indent => 2);
      Put_Special_Chars
        (File,
         Test.Test_Name.all
         & (if Test.Routine_Name = null
            then ""
            else " : " & Test.Routine_Name.all));
      Put (File, """ classname=""");
      Put_Special_Chars (File, Test.Package_Name.all);
      Put (File, """ file=""" & Test.Test_File.all & """ ");
      Put (File, "time=""");
      Put_Measure (File, Get_Measure (Test.Elapsed));

      if Test.Failure /= null then
         Put_Line (File, """>");
         Put (File, "<failure>", Indent => 3);
         Put (File, "<![CDATA[");
         Put
           (File,
            "Assertion failed: """
            & Test.Failure.Message.all
            & """ at "
            & Test.Failure.Source_Name.all
            & ":");
         Put (File, Integer (Test.Failure.Line), 0);
         Put (File, "]]>");
         Put_Line (File, "</failure>");
         Print_System_Out (File, Test.Standard_Output, Indent => 3);
         Print_System_Err (File, Test.Standard_Error, Indent => 3);
         Put_Line (File, "</testcase>", Indent => 2);

      elsif Test.Error /= null then
         Put_Line (File, """>");
         Put_Line (File, "<error>", Indent => 3);
         Put (File, "<![CDATA[");
         Put (File, Test.Error.Exception_Name.all);

         if Test.Error.Exception_Message /= null then
            Put_Line (File, ": " & Test.Error.Exception_Message.all);
         end if;
         New_Line (File);

         if Test.Error.Traceback /= null then
            Put_Line (File, "Traceback:");
            Put (File, Test.Error.Traceback.all);
         end if;
         Put_Line (File, "]]>");
         Print_System_Out (File, Test.Standard_Output, Indent => 3);
         Print_System_Err (File, Test.Standard_Error, Indent => 3);
         Put_Line (File, "</error>", Indent => 3);
         Put_Line (File, "</testcase>", Indent => 2);

      elsif Test.Standard_Output /= null or else Test.Standard_Error /= null
      then
         Print_System_Out (File, Test.Standard_Output, Indent => 3);
         Print_System_Err (File, Test.Standard_Error, Indent => 3);
         Put_Line (File, "</testcase>", Indent => 2);
      else
         Put_Line (File, """/>");
      end if;
   end Report_Test;

   procedure Dump_Result_List
     (File : AUnit.IO.File_Type; L : Result_Lists.List)
   is
      use Result_Lists;
      C : Cursor := First (L);
   begin
      while Has_Element (C) loop
         Report_Test (File, Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   procedure Report
     (Engine  : JUnit_Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options)
   is
      File     : AUnit.IO.File_Type renames Engine.File.all;
      T        : constant Time := Elapsed (R);
      Packages : constant Message_List_Access := Get_Packages (R);
   begin
      Put_Line (File, "<?xml version=""1.0"" encoding=""utf-8""?>");

      Put (File, "<testsuites skipped=""0"" tests=""");
      Put (File, Integer (Test_Count (R)), 0);
      Put (File, """ failures=""");
      Put (File, Integer (Failure_Count (R)), 0);
      Put (File, """ errors=""");
      Put (File, Integer (Error_Count (R)), 0);
      if T /= Time_Measure.Null_Time then
         Put (File, """ time=""");
         Put_Measure (File, Get_Measure (T));
      end if;
      Put_Line (File, """>");

      declare
         use Message_List;
         C : Cursor := First (Packages.all);
         Package_Name : Message_String;
      begin
         while Has_Element (C) loop
            Package_Name := Element (C);
            Put
              (File,
               "<testsuite name="""
               & Package_Name.all
               & """ skipped=""0"" tests=""",
               Indent => 1);
            Put (File, Integer (Total_Count (R, Package_Name.all)), 0);
            Put (File, """ failures=""");
            Put
              (File, Integer (Failure_Count (R, Package_Name.all)), 0);
            Put (File, """ errors=""");
            Put (File, Integer (Error_Count (R, Package_Name.all)), 0);

            declare
               S : Result_Lists.List;
               F : Result_Lists.List;
               E : Result_Lists.List;
            begin
               Successes (R, Package_Name.all, S);
               Failures (R, Package_Name.all, F);
               Errors (R, Package_Name.all, E);

               if T /= Time_Measure.Null_Time then
                  Put (File, """ time=""");
                  Put_Measure (File, Get_Combined_Time (S, E, F));
               end if;
               Put_Line (File, """>");

               Dump_Result_List (File, S);
               Dump_Result_List (File, F);
               Dump_Result_List (File, E);
            end;
            Put_Line (File, "</testsuite>", Indent => 1);
            C := Next (C);
         end loop;
      end;
      Put_Line (File, "</testsuites>");
   end Report;

end AUnit.Reporter.JUnit;
