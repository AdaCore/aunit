------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ R U N N E R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2005, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Results;

--  Reporting mechanism:
with AUnit.Test_Results.Text_Reporter;
with Ada.Calendar; use Ada.Calendar;

--  Runner of test suites using text reporter
procedure AUnit.Test_Runner (Timed : Boolean := True) is
   Res : AUnit.Test_Results.Result;
   Start_Time, End_Time : Time;
   Test_Set : constant Access_Test_Suite := Suite;

begin

   --  Run them and report results:
   Start_Time := Clock;
   Run (Test_Set.all, Res);
   End_Time := Clock;

   if Timed then
      AUnit.Test_Results.Set_Elapsed (Res, End_Time - Start_Time);
   end if;

   AUnit.Test_Results.Text_Reporter.Report (Res);
end AUnit.Test_Runner;

