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

   procedure Dump_Result_List (L : Result_Lists.List);
   --  List failed assertions

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
         S : Result_Lists.List;
      begin
         Put_Line ("  <SuccessfulTests>");
         Successes (R, S);
         Dump_Result_List (S);
         Put_Line ("  </SuccessfulTests>");
      end;

      Put_Line ("  <FailedTests>");
      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (F);
      end;

      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (E);
      end;
      Put_Line ("  </FailedTests>");

      Put_Line ("</TestRun>");
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Test (Test : Test_Result) is
      Error     : Test_Failure_Access;
      Is_Assert : Boolean;
   begin
      Put_Line ("    <Test>");
      Put      ("      <Name>");
      Put      (Test.Test_Name.all);

      if Test.Routine_Name /= null then
         Put (" : ");
         Put (Test.Routine_Name.all);
      end if;

      Put_Line ("</Name>");

      if Test.Failure /= null or else Test.Error /= null then
         if Test.Failure /= null then
            Is_Assert := True;
            Error := Test.Failure;
         else
            Is_Assert := False;
            Error := Test.Error;
         end if;

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
      end if;

      Put_Line ("    </Test>");
   end Report_Test;

end AUnit.Reporter.XML;
