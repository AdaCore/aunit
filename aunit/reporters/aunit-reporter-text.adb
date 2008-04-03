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

   procedure Dump_Result_List (L : Result_Lists.List);
   --  Dump a result list

   procedure Report_Test (Test : Test_Result);
   --  Report a single assertion failure or unexpected exception

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (L : Result_Lists.List) is

      use Result_Lists;

      C : Cursor := First (L);
   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

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
         S : Result_Lists.List;
      begin
         Put ("   Successful Tests: ");
         Put (Integer (Success_Count (R)));
         New_Line;

         Successes (R, S);
         Dump_Result_List (S);
      end;

      declare
         F : Result_Lists.List;
      begin
         New_Line;
         Put ("   Failed Assertions: ");
         Put (Integer (Failure_Count (R)));
         New_Line;

         Failures (R, F);
         Dump_Result_List (F);
         New_Line;
      end;

      declare
         E : Result_Lists.List;
      begin
         New_Line;
         Put ("   Unexpected Errors: ");
         Put (Integer (Error_Count (R)));
         New_Line;

         Errors (R, E);
         Dump_Result_List (E);
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

   -----------------
   -- Report_Test --
   -----------------

   procedure Report_Test (Test : Test_Result) is
      Error : Test_Failure_Access;
   begin
      if Test.Error /= null or else Test.Failure /= null then
         New_Line;
      end if;

      Put ("      ");
      Put (Test.Test_Name.all);

      if Test.Routine_Name /= null then
         Put (" : ");
         Put_Line (Test.Routine_Name.all);
      else
         New_Line;
      end if;

      if Test.Error /= null or else Test.Failure /= null then
         if Test.Error /= null then
            Error := Test.Error;
         else
            Error := Test.Failure;
         end if;

         Put ("      ");
         Put ("      ");
         Put_Line (Error.Message.all);

         if Error.Source_Name /= null then
            Put ("      ");
            Put ("      ");
            Put ("at ");
            Put (Error.Source_Name.all);
            Put (":");
            Put (Error.Line);
            New_Line;
         else
            New_Line;
         end if;
      end if;
   end Report_Test;

end AUnit.Reporter.Text;
