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

procedure AUnit_Harness_Package_Filters is
   R        : Result;
   Output   : Message_String;

   procedure Add_Group (Package_Name : Message_String);
   procedure Check_Filter (Name : Message_String; Expected : Count_Type);

   procedure Add_Group (Package_Name : Message_String) is
   begin
      Add_Success (R, Format ("success"), null, Null_Time, Package_Name);
      Add_Failure
        (R, Format ("failure"), null,
         (Message => Format ("failure"), Source_Name => Format ("test"),
          Line => 1), Null_Time, Package_Name);
      Add_Error
        (R, Format ("error"), null,
         (Exception_Name => Format ("ERROR"), others => null),
         Null_Time, Package_Name);
   end Add_Group;

   procedure Check_Filter (Name : Message_String; Expected : Count_Type) is
      S, F, E : Result_Lists.List;
   begin
      Assert (Total_Count (R, Name) = 3 * Expected, "Wrong total count");
      Assert (Success_Count (R, Name) = Expected, "Wrong success count");
      Assert (Failure_Count (R, Name) = Expected, "Wrong failure count");
      Assert (Error_Count (R, Name) = Expected, "Wrong error count");
      Successes (R, S, Name);
      Failures (R, F, Name);
      Errors (R, E, Name);
      Assert (S.Length = Expected, "Wrong success list");
      Assert (F.Length = Expected, "Wrong failure list");
      Assert (E.Length = Expected, "Wrong error list");
   end Check_Filter;
begin
   --  Both orders must safely compare null and nonnull package names.
   for Named_First in Boolean loop
      Clear (R);
      if Named_First then
         Add_Group (Format ("Package"));
      end if;
      Add_Group (null);
      Add_Group (null);
      if not Named_First then
         Add_Group (Format ("Package"));
      end if;
      Add_Group (Format ("Package"));
      Start_Test (R, 12);

      Check_Filter (Format (""), 4);
      Check_Filter (null, 2);
      Check_Filter (Format ("Package"), 2);
      Check_Filter (Format ("Missing"), 0);
      Assert (Total_Count (R) = 12, "Wrong default total count");
      Assert (Success_Count (R) = 4, "Wrong default success count");
      Assert (Failure_Count (R) = 4, "Wrong default failure count");
      Assert (Error_Count (R) = 4, "Wrong default error count");
      declare
         S, F, E : Result_Lists.List;
      begin
         Successes (R, S);
         Failures (R, F);
         Errors (R, E);
         Assert (S.Length = 4 and F.Length = 4 and E.Length = 4,
                 "Default extraction must select all packages");
      end;

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

      Output := Harness_IO.Capture_Report (R);
      if Output /= null then
         Assert
           (Ada.Strings.Fixed.Count (Output.all, "<testsuite name=") = 2,
            "Expected one named and one unnamed suite");
         Assert
           (Ada.Strings.Fixed.Count
              (Output.all,
               "<testsuite name=""unnamed-testsuite"" skipped=""0"""
              & " tests=""6"" failures=""2"" errors=""2""") = 1,
            "Wrong unnamed suite counts");
         Assert
           (Ada.Strings.Fixed.Count
              (Output.all,
               "<testsuite name=""Package"" skipped=""0"" tests=""6"""
               & " failures=""2"" errors=""2""") = 1,
            "Wrong named suite counts");
         Assert
           (Ada.Strings.Fixed.Count (Output.all, "<testcase name=") = 12,
            "JUnit duplicated or omitted tests");
         Assert
           (Ada.Strings.Fixed.Count (Output.all, "classname=""Package""") = 6,
            "Named tests appeared in the unnamed suite");
      end if;

      --  Empty and null package names share the unnamed suite.
      Add_Group (Format (""));
      Check_Filter (null, 3);
      Check_Filter (Format (""), 5);
   end loop;

   --  Publish the harness outcome after every assertion has passed. The
   --  fixture results above contain intentional failures and errors.
   declare
      Summary          : Result;
      Summary_Reporter : AUnit.Reporter.JUnit.JUnit_Reporter;
   begin
      Add_Success
        (Summary, Format ("package filters"), null, Null_Time,
         Format ("AUnit_Harness_Package_Filters"));
      Start_Test (Summary, 1);
      Summary_Reporter.Report (Summary);
   end;
end AUnit_Harness_Package_Filters;
