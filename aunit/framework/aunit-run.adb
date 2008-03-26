------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            A U N I T _ F R A M E W O R K . F R A M E W O R K             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2007, AdaCore                      --
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

with AUnit.Time_Measure;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package body AUnit.Run is

   Results : aliased Test_Results.Result;
   --  Test results for one harness run

   procedure Run (Suite   : Access_Test_Suite;
                  Timed   : Boolean;
                  Engine  : AUnit.Reporter.Reporter'Class;
                  Outcome : out Status);

   procedure Run (Suite   : Access_Test_Suite;
                  Timed   : Boolean;
                  Engine  : AUnit.Reporter.Reporter'Class;
                  Outcome : out Status) is
      Time : Time_Measure.Time;
   begin
      Test_Results.Clear (Results);

      if Timed then
         Time_Measure.Start_Measure (Time);
      end if;

      pragma Warnings (Off);
      AUnit.Test_Suites.Run (Suite, Results'Access, Outcome);
      pragma Warnings (On);

      if Timed then
         Time_Measure.Stop_Measure (Time);
         Test_Results.Set_Elapsed (Results, Time);
      end if;

      AUnit.Reporter.Report (Engine, Results);
   end Run;

   procedure Test_Runner
     (Reporter : AUnit.Reporter.Reporter'Class;
      Timed    : Boolean := True)
   is
      Outcome : Status;
      pragma Unreferenced (Outcome);
   begin
      Run (Suite, Timed, Reporter, Outcome);
   end Test_Runner;

   function Test_Runner_With_Status
     (Reporter : AUnit.Reporter.Reporter'Class;
      Timed    : Boolean := True) return Status
   is
      Result : Status;
   begin
      Run (Suite, Timed, Reporter, Result);
      return Result;
   end Test_Runner_With_Status;

end AUnit.Run;
