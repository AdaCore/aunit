--
--  Copyright (C) 2026, AdaCore
--

with Ada.Text_IO;
with AUnit.IO;
with AUnit.Reporter.JUnit;

package body Harness_IO is
   function Capture_Report
     (R : in out AUnit.Test_Results.Result'Class) return AUnit.Message_String
   is
      Reporter : AUnit.Reporter.JUnit.JUnit_Reporter;
      File     : aliased Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Name => "harness-report.tmp");
      Reporter.Set_File (File'Unchecked_Access);
      Reporter.Report (R);
      Ada.Text_IO.Close (File);
      return AUnit.IO.Read_File ("harness-report.tmp");
   end Capture_Report;
end Harness_IO;
