------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ C A S E S                      --
--                                                                          --
--                                 B o d y                                  --
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

--  Test cases
package body AUnit_Framework.Tests.Test_Cases is

   The_Current_Test_Case : access Test_Case'Class := null;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Format_Message (S : String) return Message_String;
   --  Format error string

   procedure Initialize (T : access Test_Case'Class);
   --  Initialize test case

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Subtest : Routine_Spec;
      R       : access Result);
   --  Run one test routine

   -----------------------
   -- Current_Test_Case --
   -----------------------

   function Current_Test_Case return access Test_Case'Class is
   begin
      return The_Current_Test_Case;
   end Current_Test_Case;

   -----------------
   -- Add_Routine --
   -----------------

   procedure Add_Routine (T : in out Test_Case'Class; Val : Routine_Spec) is
   begin
      Routine_Lists.Append (T.Routines, Val);
   end Add_Routine;

   --------------------
   -- Format_Message --
   --------------------

   function Format_Message (S : String) return Message_String is
      Message : Message_String := (others => ' ');
   begin

      if S'Length >= Message'Length then
         Message :=
           S (S'First .. S'First + Message'Length - 1);
      else
         Message (1 .. S'Length) := S;
      end if;

      return Message;

   end Format_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : access Test_Case'Class) is
   begin
      Set_Name (T.all, Name (T.all));
      Routine_Lists.Clear (T.Routines);
      Register_Tests (T.all);
   end Initialize;

   ----------------------
   -- Register_Failure --
   ----------------------

   procedure Register_Failure (T : access Test_Case'Class; S : String) is
   begin
      Message_Lists.Append (T.Failures, Format_Message (S));
   end Register_Failure;

   ---------
   -- Run --
   ---------

   procedure Run (Test : access Test_Case; R : Result_Access) is
      use Routine_Lists;
      C : Cursor;
   begin
      Initialize (Test);
      Start_Test (R.all, Routine_Lists.Length (Test.Routines));
      Set_Up_Case (Test_Case'Class (Test.all));
      C := First (Test.Routines);

      while Has_Element (C) loop
         The_Current_Test_Case := Test;
         Run_Routine (Test, Element (C), R);
         Next (C);
      end loop;

      Tear_Down_Case (Test_Case'Class (Test.all));
   end Run;

   -----------------
   -- Run_Routine --
   -----------------

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Subtest : Routine_Spec;
      R       : access Result) is separate;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Test : in out Test_Case'Class; Name : Test_String) is
   begin
      Test.Name := Name;
   end Set_Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up;

   ------------------
   --  Set_Up_Case --
   ------------------

   procedure Set_Up_Case (Test : in out Test_Case) is
      --  Default
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down;

   --------------------
   -- Tear_Down_Case --
   --------------------

   procedure Tear_Down_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down_Case;

end AUnit_Framework.Tests.Test_Cases;
