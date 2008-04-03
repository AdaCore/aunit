------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

package body AUnit.Simple_Test_Cases is

   The_Current_Test_Case : Test_Case_Access := null;

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      R       : access Result;
      Outcome : out Status);
   --  Run one test routine

   -----------------
   -- Run_Routine --
   -----------------

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      R       : access Result;
      Outcome : out Status) is separate;

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

   procedure Run (Test    : access Test_Case;
                  R       : Result_Access;
                  Outcome : out Status) is
   begin
      The_Current_Test_Case := Test_Case_Access (Test);
      Start_Test (R.all, 1);

      --  Run test routine
      Set_Up (Test_Case'Class (Test.all));
      Run_Routine (Test, R, Outcome);
      Tear_Down (Test_Case'Class (Test.all));
   end Run;

end AUnit.Simple_Test_Cases;
