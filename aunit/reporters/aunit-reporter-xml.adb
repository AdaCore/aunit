------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
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
package body AUnit.Reporter.XML is

   procedure Dump_Error_List (L : in Error_Lists.List);
   --  List failed assertions

   procedure Dump_Failure_List (L : in Failure_Lists.List);
   --  List failed assertions

   procedure Dump_Success_List (L : in Success_Lists.List);
   --  List successful test routines

   procedure Report_Error (Error : Test_Failure; Is_Assert : Boolean);
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
         Report_Error (Element (C), False);
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
         Report_Error (Element (C), True);
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
         Put_Line ("    <Test>");
         Put      ("      <Name>");
         Put      (Success.Test_Name.all);
         Put_Line ("</Name>");
         Put_Line ("    </Test>");
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

   procedure Report (Engine : XML_Reporter;
                     R      : in out Result)
   is
      pragma Unreferenced (Engine);
      T   : AUnit_Duration;
      procedure Put_Measure is new Gen_Put_Measure;
   begin
      Put_Line ("<?xml version='1.0' encoding='utf-8' ?>");
      Put      ("<TestRun");

      if Elapsed  (R) /= Time_Measure.Null_Time then
         T := Get_Measure (Elapsed (R));

         Put (" elapsed='");
         Put_Measure (T);
         Put_Line ("'>");
      else
         Put_Line (">");
      end if;

      Put_Line ("  <Statistics>");
      Put      ("    <Tests>");
      Put (Integer (Test_Count (R)));
      Put_Line ("</Tests>");
      Put      ("    <FailuresTotal>");
      Put (Integer (Failure_Count (R)) + Integer (Error_Count (R)));
      Put_Line ("</FailuresTotal>");
      Put      ("    <Failures>");
      Put (Integer (Failure_Count (R)));
      Put_Line ("</Failures>");
      Put      ("    <Errors>");
      Put (Integer (Error_Count (R)));
      Put_Line ("</Errors>");
      Put_Line ("  </Statistics>");

      declare
         S : Success_Lists.List;
      begin
         Put_Line ("  <SuccessfulTests>");
         Successes (R, S);
         Dump_Success_List (S);
         Put_Line ("  </SuccessfulTests>");
      end;

      Put_Line ("  <FailedTests>");
      declare
         F : Failure_Lists.List;
      begin
         Failures (R, F);
         Dump_Failure_List (F);
      end;

      declare
         E : Error_Lists.List;
      begin
         Errors (R, E);
         Dump_Error_List (E);
      end;
      Put_Line ("  </FailedTests>");

      Put_Line ("</TestRun>");
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Error : Test_Failure; Is_Assert : Boolean) is
   begin
      Put_Line ("    <Test>");
      Put      ("      <Name>");
      Put      (Error.Test_Name.all);
      Put_Line ("</Name>");
      Put      ("      <FailureType>");
      if Is_Assert then
         Put   ("Assertion");
      else
         Put   ("Error");
      end if;
      Put_Line ("</FailureType>");
      Put      ("      <Message>");
      Put      (Error.Message.all);
      Put_Line ("</Message>");

      if Error.Source_Name /= null then
         Put_Line ("      <Location>");
         Put      ("        <File>");
         Put      (Error.Source_Name.all);
         Put_Line ("</File>");
         Put      ("        <Line>");
         Put      (Error.Line);
         Put_Line ("</Line>");
         Put_Line ("      </Location>");
      end if;

      Put_Line ("    </Test>");
   end Report_Error;

end AUnit.Reporter.XML;
