------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . R E P O R T E R . T R X                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2020, AdaCore                   --
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

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada_Containers;

package body AUnit.Reporter.TRX is

   procedure Put_Line (File : Ada.Text_IO.File_Type; S : String) 
                       renames Ada.Text_IO.Put_Line;
   
   package SU renames Ada.Strings.Unbounded;

   type Test_Outcome is (Passed, Failed, Error);

   function Outcome_Str (Outcome : Test_Outcome) return String is
   begin
      return (case Outcome is
                 when Passed => "Passed",
                 when Failed => "Failed",
                 when Error  => "Failed");
   end Outcome_Str;

   type Test_Data is record
      Name : SU.Unbounded_String;
      Outcome : Test_Outcome;
      Duration : SU.Unbounded_String;
      Routine_Name : SU.Unbounded_String;
      -- for assertion failures
      Failure_Message : SU.Unbounded_String;
      Failure_Source_Name : SU.Unbounded_String;
      Line : SU.Unbounded_String;
      -- for exceptions
      Exception_Name : SU.Unbounded_String;
      Exception_Message : SU.Unbounded_String;
      Backtrace : SU.Unbounded_String;
   end record;

   function Get_Test_Data (Test : Test_Result) return Test_Data;

   package Test_Data_Vecs is new Ada.Containers.Vectors
       (Element_Type => Test_Data,
        Index_Type => Positive);

   procedure Iterate
     (Container : Test_Data_Vecs.Vector;
      Process   : not null access procedure (Position : Test_Data_Vecs.Cursor)) 
         renames Test_Data_Vecs.Iterate;

   function Get_All_Results (R : Result'Class) 
                             return Test_Data_Vecs.Vector is
      All_Results : Test_Data_Vecs.Vector;
      F, E, S : Result_Lists.List;

      procedure Get_Result_List (L : Result_Lists.List) is
         use Result_Lists;
         C : Cursor := First (L);
      begin
         --  Note: can't use Iterate because it violates restriction
         --  No_Implicit_Dynamic_Code
         while Has_Element (C) loop
            All_Results.Append (Get_Test_Data (Element (C)));
            Next (C);
         end loop;
      end Get_Result_List;

   begin
      Failures (R, F);
      Errors (R, E);
      Successes (R, S);
      Get_Result_List (F);
      Get_Result_List (E);
      Get_Result_List (S);
      return All_Results;
   end Get_All_Results;

   procedure Report (Engine  : TRX_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options)
   is
      pragma Unreferenced (Options);
      
      File    : Ada.Text_IO.File_Type renames Engine.File.all;

      use type Ada_Containers.Count_Type;
      Total : constant Ada_Containers.Count_Type := Test_Count (R);
      Failures : constant Ada_Containers.Count_Type := Failure_Count (R) 
                                                       + Error_Count (R);
      Successes : constant Ada_Containers.Count_Type := Total - Failures;

      function Str(I : Ada_Containers.Count_Type) return String is
         S : constant String := I'Image;
      begin
         return S(2..S'Last); -- remove leading space
      end Str;

      procedure Report_Test_Name (Test_Cursor : Test_Data_Vecs.Cursor) is
         Test : constant Test_Data := Test_Data_Vecs.Element (Test_Cursor);
      begin
         Put_Line (File, "    <UnitTest name=""" & SU.To_String(Test.Name) 
                   & """ id="""
                   & SU.To_String(Test.Name) & """ />");
      end Report_Test_Name;

      -- I found this example trx file
      pragma Style_Checks ("M200"); -- Allow long lines
      -- https://github.com/x97mdr/pickles/blob/master/src/Pickles/Pickles.Test/results-example-mstest.trx
      pragma Style_Checks ("M79");
      -- it says UnitTestResult can have StdOut (text) and ErrorInfo 
      -- containing Message and StackTrace
      procedure Report_Test (Test_Cursor : Test_Data_Vecs.Cursor) is
         Test : constant Test_Data := Test_Data_Vecs.Element (Test_Cursor);
      begin
         Put_Line (File, "    <UnitTestResult testName="""
                   & SU.To_String (Test.Name)
                   & """ testId=""" & SU.To_String (Test.Name)
                   & """ duration=""" & SU.To_String (Test.Duration)
                   & """ outcome=""" & Outcome_Str (Test.Outcome) & """>");
         if Test.Outcome /= Passed then
            Put_Line (File, "    <Output>");
            Put_Line (File, "      <ErrorInfo>");
            Put_Line (File, "        <Message>");
            if Test.Outcome = Failed then
               Put_Line (File, "Assertion failed on line"
                         & SU.To_String (Test.Line)
                         & " of " & SU.To_String (Test.Failure_Source_Name)
                         & ": """
                         & SU.To_String (Test.Failure_Message) & """");
            else
               Put_Line (File, "Raised " & SU.To_String (Test.Exception_Name)
                         & ":");
               Put_Line (File, "Exception message:");
               Put_Line (File, SU.To_String (Test.Exception_Message));
            end if;
            Put_Line (File, "        </Message>");
            if Test.Outcome = Error then
               Put_Line (File, "        <StackTrace>");
               Put_Line (File, SU.To_String (Test.Backtrace));
               Put_Line (File, "        </StackTrace>");
            end if;
            Put_Line (File, "      </ErrorInfo>");
            Put_Line (File, "    </Output>");
         end if;
         Put_Line (File, "    </UnitTestResult>");
      end Report_Test;
   begin -- Report
      Put_Line (File, "<?xml version=""1.0"" encoding=""UTF-8"" ?>");
      Put_Line (File, "<TestRun xmlns=""http://microsoft.com/schemas/"
                & "VisualStudio/TeamTest/2010"">");
      Put_Line (File, "  <ResultSummary outcome=""Completed"">");
      Put_Line (File, "    <Counters total=""" & Str(Total) & """ passed=""" & 
                  Str(Successes) & """ failed=""" & Str(Failures) & """ />");
      Put_Line (File, "  </ResultSummary>");

      declare
         Tests : constant Test_Data_Vecs.Vector := Get_All_Results (R);
      begin
         Put_Line (File, "  <TestDefinitions>");
         Iterate (Tests, Report_Test_Name'Access);
         Put_Line (File, "  </TestDefinitions>");

         Put_Line (File, "  <Results>");
         Iterate (Tests, Report_Test'Access);
         Put_Line (File, "  </Results>");
      end;

      Put_Line (File, "</TestRun>");
   end Report;

   function Get_Name (Test : Test_Result) return SU.Unbounded_String is 
   begin
      return SU.To_Unbounded_String (Test.Test_Name.all);
   end Get_Name;

   function Get_Outcome (Test : Test_Result) return Test_Outcome is
   begin
      if Test.Error /= null then
         return Error;
      end if;
      if Test.Failure /= null then
         return Failed;
      end if;
      return Passed;
   end Get_Outcome;

   function Get_Duration (Test : Test_Result) return SU.Unbounded_String is
      use type Ada.Calendar.Time;
      Elapsed_Seconds : Duration := Test.Elapsed.Stop - Test.Elapsed.Start;
      H, M, S  : Integer := 0;

      -- pad integer with leading zero to width 2
      function Str(I : Integer) return String is
         S : constant String := I'Image;
         S2 : String renames S(S'First + 1 .. S'Last);
      begin
         if I < 10 then
            return "0" & S2;
         else
            return S2;
         end if;
      end Str;

      function DecStr(D : Duration) return String is
         S : constant String := D'Image;
      begin
         return S(S'First + 3 .. S'Last);
      end DecStr;
   begin
      H := Integer (Elapsed_Seconds / 3600.0);
      Elapsed_Seconds := Elapsed_Seconds - (H * 3600.0);
      M := Integer (Elapsed_Seconds / 60.0);
      Elapsed_Seconds := Elapsed_Seconds - (M * 60.0);
      S := Integer (Elapsed_Seconds / 1.0);
      Elapsed_Seconds := Elapsed_Seconds - (S * 1.0);
      return SU.To_Unbounded_String (Str (H) & ":" & Str (M) & ":" & Str (S) 
                                     & "." & DecStr (Elapsed_Seconds));
   end Get_Duration;

   function Get_Routine_Name (Test : Test_Result) return SU.Unbounded_String is
   begin
      return (if Test.Routine_Name = null 
              then SU.To_Unbounded_String ("") 
              else SU.To_Unbounded_String (Test.Routine_Name.all));
   end Get_Routine_Name;

   function Get_Failure_Message (Test : Test_Result)
                                 return SU.Unbounded_String is
   begin
      return (if Test.Failure = null 
              then SU.To_Unbounded_String ("") 
              else SU.To_Unbounded_String (Test.Failure.Message.all));
   end Get_Failure_Message;

   function Get_Failure_Source_Name (Test : Test_Result)
                                     return SU.Unbounded_String is
   begin
      return (if Test.Failure = null 
              then SU.To_Unbounded_String ("")
              else SU.To_Unbounded_String (Test.Failure.Source_Name.all));
   end Get_Failure_Source_Name;

   function Get_Line (Test : Test_Result) return SU.Unbounded_String is
   begin
      return (if Test.Failure = null
              then SU.To_Unbounded_String ("")
              else SU.To_Unbounded_String (Test.Failure.Line'Image));
   end Get_Line;

   function Get_Exception_Name (Test : Test_Result)
                                return SU.Unbounded_String is
   begin
      return (if Test.Error = null
              then SU.To_Unbounded_String ("")
              else SU.To_Unbounded_String (Test.Error.Exception_Name.all));
   end Get_Exception_Name;

   function Get_Exception_Message (Test : Test_Result)
                                   return SU.Unbounded_String is
   begin
      return (if Test.Error = null then SU.To_Unbounded_String ("") else 
                (if Test.Error.Exception_message = null
                 then SU.To_Unbounded_String ("")
                 else
                  SU.To_Unbounded_String (Test.Error.Exception_Message.all)));
   end Get_Exception_Message;

   function Get_Backtrace (Test : Test_Result) return SU.Unbounded_String is
   begin
      return (if Test.Error = null then SU.To_Unbounded_String ("") else 
                (if Test.Error.Traceback = null 
                 then SU.To_Unbounded_String ("") 
                 else SU.To_Unbounded_String (Test.Error.Traceback.all)));
   end Get_Backtrace;

   function Get_Test_Data (Test : Test_Result) return Test_Data is
   begin
      return (Name => Get_Name (Test),
              Outcome => Get_Outcome (Test),
              Duration => Get_Duration (Test),
              Routine_Name => Get_Routine_Name (Test),
              Failure_Message => Get_Failure_Message (Test),
              Failure_Source_Name => Get_Failure_Source_Name (Test),
              Line => Get_Line (Test),
              Exception_Name => Get_Exception_Name (Test),
              Exception_Message => Get_Exception_Message (Test),
              Backtrace => Get_Backtrace (Test)
             );
   end Get_Test_Data;

end AUnit.Reporter.TRX;
