------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A U N I T . F R A M E W O R K                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2006, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit_Framework.Test_Results;
with AUnit_Framework.Tests.Test_Cases.Assertions;
with AUnit_Framework.Tests.Test_Suites;

--  Test Suite Framework
--
--  This unit provides a way to configure memory usage and capacity by
--  specifying the maximum number of tests and failure reports, and the maximum
--  size of any failure message. This is needed because the GNAT Pro baseline
--  ZFP profile does not support dynamic memory management. The parameters
--  provide a way to trade off between the number of messages, their size and
--  the consequent memory requirements.
generic
   Max_Exceptions_Per_Harness : Natural;   --  Max unhandled exceptions per
                                          --  harness
   Max_Failures_Per_Harness : Natural;     --  Max assertion failures per
                                          --  harness
   Max_Failure_Message_Size : Natural;     --  Max size of a routine failure
                                          --  message
   Max_Routines_Per_Test_Case : Natural;   --  Max routines per test case
   Max_Test_Cases_Per_Suite : Natural;     --  Max test cases per suite

   Max_Test_Name_Size : Natural;           --  Max size of a test case name
   Max_Routine_Description_Size : Natural; --  Max size of a test routine
                                          --  description
package AUnit_Framework.Framework is

   Max_Routines_Per_Harness : constant Natural :=
      Max_Routines_Per_Test_Case * Max_Test_Cases_Per_Suite;

   package Test_Results is
     new AUnit_Framework.Test_Results
       (Max_Routines_Per_Harness,
        Max_Exceptions_Per_Harness,
        Max_Failures_Per_Harness,
        Max_Failure_Message_Size,
        Max_Test_Name_Size,
        Max_Routine_Description_Size);

   package Tests is new AUnit_Framework.Tests (Test_Results);
   package Test_Cases is
     new Tests.Test_Cases
       (Max_Routines_Per_Test_Case,
        Max_Failures_Per_Harness,
        Max_Failure_Message_Size);
   package Assertions is new Test_Cases.Assertions;

   package Test_Suites is
     new Tests.Test_Suites (Test_Cases, Max_Test_Cases_Per_Suite);

   generic
      with function Suite return Test_Suites.Access_Test_Suite;
   procedure Test_Runner (Timed : Boolean := True);
   --  Instantiate Test_Runner at library level to minimize stack requirement

end AUnit_Framework.Framework;
