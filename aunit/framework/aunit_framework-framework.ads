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

pragma Restrictions (No_Implicit_Dynamic_Code);

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
   Max_Tests_Per_Harness : Positive;   --  Max test cases per harness
   Max_Errors_Per_Harness : Natural;   --  Max unhandled exceptions per harness
   Max_Failures_Per_Harness : Natural; --  Max assertion failures per harness
   Max_Failure_Message_Size : Natural; --  Max size of a routine failure
                                       --  message
   Max_Routines_Per_Test : Natural;    --  Max routines per test case
   Test_Name_Size : Natural;           --  Max size of a test case name
   Routine_Name_Size : Natural;        --  Max size of a test routine
                                       --  description
package AUnit_Framework.Framework is
   package Test_Results is
     new AUnit_Framework.Test_Results
       (Max_Tests_Per_Harness,
        Max_Errors_Per_Harness,
        Max_Failures_Per_Harness,
        Max_Failure_Message_Size,
        Test_Name_Size,
        Routine_Name_Size);

   package Tests is new AUnit_Framework.Tests (Test_Results);
   package Test_Cases is
     new Tests.Test_Cases
       (Max_Routines_Per_Test,
        Max_Failures_Per_Harness,
        Max_Failure_Message_Size);
   package Assertions is new Test_Cases.Assertions;

   package Test_Suites is new Tests.Test_Suites (Test_Cases);

   generic
      with function Suite return Test_Suites.Access_Test_Suite;
   package Harness is
      procedure Run (Timed : Boolean := False);
      --  ??? add timed parameter
   end Harness;
   --  Instantiate Harness at library level to minimize stack requirement

end AUnit_Framework.Framework;
