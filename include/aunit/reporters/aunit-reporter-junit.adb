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
   
   procedure Put_Special_Chars (File : File_Type; S : String) is
   begin
      for C of S loop
         case C is
            when '"' => Put (File, "&quot;");
            when '<' => Put (File, "&lt;");
            when '>' => Put (File, "&gt;");
            when '&' => Put (File, "&amp;");
            when others => Put (File, (1 => C));
         end case;
      end loop;
   end Put_Special_Chars;
   
   procedure Put_Measure is
     new Gen_Put_Measure_In_Seconds;
   
   procedure Report_Test (File : File_Type; Test : Test_Result) is
   begin
      Put (File, "<testcase name=""");
      Put_Special_Chars (File, Test.Test_Name.all &
           (if Test.Routine_Name = null then ""
              else " : " & Test.Routine_Name.all));
      Put (File, """ time=""");
      Put_Measure (File, Get_Measure (Test.Elapsed));
      if Test.Failure /= null then
         Put_Line (File, """>"); 
         Put_Line (File, "<failure>");
         Put_Line (File, "<![CDATA[");
         Put (File, "        Assertion: ");
         Put_Line (File, Test.Failure.Message.all);
         Put (File, "        File: ");
         Put_Line (File, Test.Failure.Source_Name.all);
         Put (File, "        Line: ");
         Put (File, Integer (Test.Failure.Line), 0);
         New_Line (File);
         Put_Line (File, "]]>");
         Put_Line (File, "</failure>");
         Put_Line (File, "</testcase>");
      elsif Test.Error /= null then
         Put_Line (File, """>"); 
         Put_Line (File, "<error>");
         Put_Line (File, "<![CDATA[");
         Put (File, "      Exception: ");
         Put_Line (File, Test.Error.Exception_Name.all);
         if Test.Error.Exception_Message /= null then
            Put_Line (File, Test.Error.Exception_Message.all);
         end if;
         if Test.Error.Traceback /= null then
            Put      (File, "      Traceback: ");
            Put_Line (File, Test.Error.Traceback.all);
         end if;
         Put_Line (File, "]]>");
         Put_Line (File, "</error>");
         Put_Line (File, "</testcase>");
      else
         Put_Line (File, """ />");
      end if;      
   end Report_Test;
   
   procedure Dump_Result_List (File : File_Type; L : Result_Lists.List) is
      use Result_Lists;
      C : Cursor := First (L);
   begin
      while Has_Element (C) loop
         Report_Test (File, Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;
   
   procedure Report (Engine  : JUnit_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options)
   is     
      File : File_Type renames Engine.File.all;
      T    : constant Time := Elapsed (R);
   begin
      Put_Line (File, "<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line (File, "<testsuites>");
      Put (File, "<testsuite name=""" & "aunit_testsuite" & """ skipped=""0"" tests=""");
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
      if Options.Report_Successes then
         declare
            S : Result_Lists.List;
         begin
            Successes (R, S);
            Dump_Result_List (File, S);
         end;
      end if;      
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
      Put_Line (File, "</testsuite>");
      Put_Line (File, "</testsuites>");
   end Report;

end AUnit.Reporter.JUnit;
