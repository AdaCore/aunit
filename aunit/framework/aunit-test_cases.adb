------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2000 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------
with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Assertions; use AUnit.Assertions;

pragma Elaborate_All (AUnit.Test_Results);

--  Test cases.
package body AUnit.Test_Cases is

   --  Run one test routine:
   procedure Run_Routine
     (Test : in out Test_Case'Class;
      Subtest : Routine_Spec; R : in out Result);

   --  Run one test routine:
   procedure Run_Routine
     (Test : in out Test_Case'Class;
      Subtest : Routine_Spec; R : in out Result) is

   begin
      Set_Up (Test);

      begin
         Subtest.Routine.all;
         Add_Success (R, Name (Test), Subtest.Routine_Name);
      exception
         when E : Assertion_Error =>
            Add_Failure (R, Name (Test), Subtest.Routine_Name, E);
         when E : others =>
            Add_Error (R, Name (Test), Subtest.Routine_Name, E);
      end;

      Tear_Down (Test);
   end Run_Routine;

   --  Run all routines registered for this test case:
   procedure Run (Test : in out Test_Case; R : in out Result) is
   begin
      --  Record number of test routines:
      Start_Test
        (R, Routine_Lists.Count (Test.Routines));

      Start (Test.Routines);
      while not Off (Test.Routines) loop
         Run_Routine (Test, Item (Test.Routines), R);
         Forth (Test.Routines);
      end loop;
   end Run;

   --  Default Set up routine:
   procedure Set_Up (Test : in out Test_Case) is
   begin null; end Set_Up;

   --  Default Tear down routine:
   procedure Tear_Down (Test : in out Test_Case) is
   begin null; end Tear_Down;


   --  Register the test routines.
   procedure Initialize (Test : in out Test_Case) is
   begin
      Register_Tests (Test_Case'Class (Test));
   end Initialize;

end AUnit.Test_Cases;
