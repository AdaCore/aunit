--
--  Copyright (C) 2026, AdaCore
--

with Ada.Strings.Fixed;
with Ada_Containers; use Ada_Containers;

with AUnit;              use AUnit;
with AUnit.Assertions;   use AUnit.Assertions;
with AUnit.Reporter.JUnit;
with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Time_Measure; use AUnit.Time_Measure;

with Harness_IO;

procedure AUnit_Harness_Results is
   R        : Result;
   Output   : Message_String;
   Failure  : constant Test_Failure :=
     (Message     => Format ("failure"),
      Source_Name => Format ("test"),
      Line        => 1);
   Error    : constant Test_Error :=
     (Exception_Name => Format ("ERROR"), others => null);

   procedure Add_Group (Prefix : String; Package_Name : Message_String);

   procedure Add_Group (Prefix : String; Package_Name : Message_String) is
   begin
      Add_Success
        (R, Format (Prefix & "success"), null, Null_Time, Package_Name);
      Add_Failure
        (R, Format (Prefix & "failure"), null, Failure, Null_Time,
         Package_Name);
      Add_Error
        (R, Format (Prefix & "error"), null, Error, Null_Time, Package_Name);
   end Add_Group;
begin
   --  Exercise the short forms, explicit null, empty, and named packages in
   --  one result. All three package-less forms belong to the same suite.
   Add_Success (R, Format ("short success"), null, Null_Time);
   Add_Failure (R, Format ("short failure"), null, Failure, Null_Time);
   Add_Error (R, Format ("short error"), null, Error, Null_Time);
   Add_Group ("null ", null);
   Add_Group ("empty ", Format (""));
   Add_Group ("named ", Format ("Package"));
   Start_Test (R, 12);

   --  The public empty-name filter must continue to select every result.
   Assert
     (Total_Count (R) = 12, "Empty filter no longer selects all tests");
   Assert (Success_Count (R) = 4, "Wrong unfiltered success count");
   Assert (Failure_Count (R) = 4, "Wrong unfiltered failure count");
   Assert (Error_Count (R) = 4, "Wrong unfiltered error count");

   declare
      Packages : constant Message_List_Access := Get_Packages (R);
      C        : Message_List.Cursor := Packages.First;
      Unnamed  : Boolean := False;
      Named    : Boolean := False;
   begin
      Assert (Packages.Length = 2, "Expected two packages");
      while Message_List.Has_Element (C) loop
         declare
            Name : constant Message_String := Message_List.Element (C);
         begin
            if Name = null then
               Unnamed := True;
            elsif Name.all = "Package" then
               Named := True;
            else
               Assert (False, "Unexpected package name");
            end if;
         end;
         C := Message_List.Next (C);
      end loop;
      Assert (Unnamed and Named, "Missing unnamed or named package");
   end;

   Assert (Total_Count (R, null) = 9, "Wrong unnamed total count");
   Assert (Success_Count (R, null) = 3, "Wrong unnamed success count");
   Assert (Failure_Count (R, null) = 3, "Wrong unnamed failure count");
   Assert (Error_Count (R, null) = 3, "Wrong unnamed error count");
   Assert (Total_Count (R, Format ("Package")) = 3,
           "Wrong named total count");
   declare
      S, F, E : Result_Lists.List;
   begin
      Successes (R, S, null);
      Failures (R, F, null);
      Errors (R, E, null);
      Assert (S.Length = 3 and F.Length = 3 and E.Length = 3,
              "Wrong unnamed result lists");
   end;

   Output := Harness_IO.Capture_Report (R);

   if Output /= null then
      Assert
        (Ada.Strings.Fixed.Count (Output.all, "<testsuite name=") = 2,
         "Expected one named and one package-less suite");
      Assert
        (Ada.Strings.Fixed.Count
           (Output.all,
            "<testsuite name=""unnamed-testsuite"" skipped=""0"" tests=""9"""
            & " failures=""3"" errors=""3""") = 1,
         "Wrong package-less suite counts");
      Assert
        (Ada.Strings.Fixed.Count
           (Output.all,
            "<testsuite name=""Package"" skipped=""0"" tests=""3"""
            & " failures=""1"" errors=""1""") = 1,
         "Wrong named suite counts");
      Assert
        (Ada.Strings.Fixed.Count (Output.all, "<testcase name=") = 12,
         "JUnit duplicated or omitted test results");
      Assert
        (Ada.Strings.Fixed.Count (Output.all, "classname=""Package""") = 3,
         "Named tests appeared in the package-less suite");
   end if;

   declare
      Summary          : Result;
      Summary_Reporter : AUnit.Reporter.JUnit.JUnit_Reporter;
   begin
      Add_Success
        (Summary, Format ("results"), null, Null_Time,
         Format ("AUnit_Harness_Results"));
      Start_Test (Summary, 1);
      Summary_Reporter.Report (Summary);
   end;
end AUnit_Harness_Results;
