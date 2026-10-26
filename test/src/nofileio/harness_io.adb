--
--  Copyright (C) 2026, AdaCore
--

package body Harness_IO is
   function Capture_Report
     (R : in out AUnit.Test_Results.Result'Class) return AUnit.Message_String
   is
      pragma Unreferenced (R);
   begin
      return null;
   end Capture_Report;
end Harness_IO;
