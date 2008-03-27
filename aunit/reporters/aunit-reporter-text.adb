------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . T E X T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2008, AdaCore                   --
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

with GNAT.IO;            use GNAT.IO;
with AUnit.Time_Measure; use AUnit.Time_Measure;

--  Very simple reporter to console
package body AUnit.Reporter.Text is

   procedure Dump_Error_List (L : in Error_Lists.List);
   --  List failed assertions

   procedure Dump_Failure_List (L : in Failure_Lists.List);
   --  List failed assertions

   procedure Dump_Success_List (L : in Success_Lists.List);
   --  List successful test routines

   procedure Report_Error (Error : Test_Failure);
   --  Report a single assertion failure or unexpected exception

   ----------------------
   --  Dump_Error_List --
   ----------------------

   procedure Dump_Error_List (L : Error_Lists.List) is

      use Error_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Error (Element (C));
         Next (C);
      end loop;
   end Dump_Error_List;

   ------------------------
   --  Dump_Failure_List --
   ------------------------

   procedure Dump_Failure_List (L : Failure_Lists.List) is

      use Failure_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Error (Element (C));
         Next (C);
      end loop;
   end Dump_Failure_List;

   -----------------------
   -- Dump_Success_List --
   -----------------------

   procedure Dump_Success_List (L : in Success_Lists.List) is

      use Success_Lists;

      procedure Report_Success (Success : Test_Success);

      procedure Report_Success (Success : Test_Success) is
      begin
         Put ("      ");
         Put (Success.Test_Name.all);
      end Report_Success;

      C : Cursor := First (L);

   begin
      while Has_Element (C) loop
         Report_Success (Element (C));
         Next (C);
      end loop;
   end Dump_Success_List;

   ------------
   -- Report --
   ------------

   procedure Report (Engine : Text_Reporter;
                     R      : in out Result)
   is
      pragma Unreferenced (Engine);
      T   : AUnit_Duration;
      procedure Put_Measure is new Gen_Put_Measure;
   begin
      Put_Line ("--------------------");
      New_Line;

      Put ("   Total Tests Run: ");
      Put (Integer (Test_Count (R)));
      New_Line; New_Line;

      declare
         S : Success_Lists.List;
      begin
         Put ("   Successful Tests: ");
         Put (Integer (Success_Count (R)));
         New_Line;

         Successes (R, S);
         Dump_Success_List (S);
      end;

      declare
         F : Failure_Lists.List;
      begin
         New_Line;
         Put ("   Failed Assertions: ");
         Put (Integer (Failure_Count (R)));
         New_Line;

         Failures (R, F);
         Dump_Failure_List (F);
         New_Line;
      end;

      declare
         E : Error_Lists.List;
      begin
         New_Line;
         Put ("   Unexpected Errors: ");
         Put (Integer (Error_Count (R)));
         New_Line;

         Errors (R, E);
         Dump_Error_List (E);
         New_Line;
      end;

      if Elapsed  (R) /= Time_Measure.Null_Time then
         New_Line;
         T := Get_Measure (Elapsed (R));

         Put ("Time: ");
         Put_Measure (T);
         Put_Line (" seconds");
      end if;
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Error : Test_Failure) is
   begin
      New_Line;
      Put ("      ");
      Put_Line (Error.Test_Name.all);
      Put ("      ");
      Put ("      ");
      Put_Line (Error.Message.all);
      if Error.Source_Name /= null then
         Put ("      ");
         Put ("      ");
         Put ("at ");
         Put (Error.Source_Name.all);
         Put (":");
         Put_Line (Error.Line);
      else
         New_Line;
      end if;
   end Report_Error;

end AUnit.Reporter.Text;
