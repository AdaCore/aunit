------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008-2009, AdaCore                  --
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

with AUnit.Assertions;  use AUnit.Assertions;
with AUnit.Tests;       use AUnit.Tests;

package body AUnit.Simple_Test_Cases is

   The_Current_Test_Case : Test_Case_Access := null;

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status);
   --  Run one test routine

   -----------------
   -- Run_Routine --
   -----------------

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status) is separate;

   ------------------
   -- Routine_Name --
   ------------------

   function Routine_Name (Test : Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return null;
   end Routine_Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down;

   ----------------------
   -- Register_Failure --
   ----------------------

   procedure Register_Failure
     (S           : String;
      Source_Name : String;
      Source_Line : Natural)
   is
   begin
      Failure_Lists.Append
        (The_Current_Test_Case.Failures,
         (Format (S), Format (Source_Name), Source_Line));
   end Register_Failure;

   ---------
   -- Run --
   ---------

   procedure Run
     (Test    : access Test_Case;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status)
   is
      Old : constant Test_Case_Access := The_Current_Test_Case;
   begin
      if Options.Filter = null
        or else Is_Active (Options.Filter.all, Test.all)
      then
         The_Current_Test_Case := Test_Case_Access (Test);
         Start_Test (R, 1);

         --  Run test routine
         Set_Up (Test_Case'Class (Test.all));
         Run_Routine (Test, Options, R, Outcome);
         Tear_Down (Test_Case'Class (Test.all));
         The_Current_Test_Case := Old;
      end if;
   end Run;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Test      : in out Test_Case;
      Condition : Boolean;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      pragma Unreferenced (Test);
   begin
      Assert (Condition, Message, Source, Line);
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Test      : in out Test_Case;
      Actual    : String;
      Expected  : String;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      Full_Message : String
         (1 .. Message'Length + 22 + Expected'Length + Actual'Length);
      First : Natural;
   begin
      --  In ZFP, we cannot use string concatenation directly
      Full_Message (1 .. Message'Length) := Message;
      First := Message'Length + 1;
      Full_Message (First) := ASCII.LF;
      First := First + 1;
      Full_Message (First .. First + 9) := "Expected: ";
      First := First + 10;
      Full_Message (First .. First + Expected'Length - 1) := Expected;
      First := First + Expected'Length;
      Full_Message (First) := ASCII.LF;
      First := First + 1;
      Full_Message (First .. First + 9) := "Received: ";
      First := First + 10;
      Full_Message (First .. Full_Message'Last) := Actual;

      Assert
         (Test,
          Condition => Expected = Actual,
          Message   => Full_Message,
          Source    => Source,
          Line      => Line);
   end Assert;

end AUnit.Simple_Test_Cases;
