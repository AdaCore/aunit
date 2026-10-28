--
--  Copyright (C) 2026, AdaCore
--

with AUnit.Test_Results;

package Harness_IO is
   function Capture_Report
     (R : in out AUnit.Test_Results.Result'Class) return AUnit.Message_String;
   --  Capture a JUnit report on runtimes with file I/O. Return null when
   --  file I/O is unavailable; portable result checks still run.
end Harness_IO;
