------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--      A U N I T . T E S T _ R E S U L T S . T E X T _ R E P O R T E R     --
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with AUnit.Options; use AUnit.Options;

--  Very simple reporter to console
package body AUnit.Test_Results.Text_Reporter is

   --  Report the contents of an error or failure list
   procedure Dump_Failure_List
     (L : Failure_Lists.List; Is_Assertion : Boolean := True);

   --  List successful tests
   procedure Dump_Success_List
     (L : Success_Lists.List; Is_Assertion : Boolean := True);

      --  Report the contents of an error or failure list
   procedure Dump_Failure_List
     (L : Failure_Lists.List; Is_Assertion : Boolean := True) is
      Err_Rec : Test_Failure;
      use Failure_Lists;

      Iter : List := L;
   begin
      Start (Iter);
      while not Off (Iter) loop
         Err_Rec := Item (Iter);
         Put
           ("      " & Err_Rec.Test_Name.all
            & ": " & Err_Rec.Routine_Name.all & ":: ");

         if not Is_Assertion then
            Put_Line ("**" & Exception_Name (Err_Rec.E.all) & "** : ");
            Put ("         ");
         end if;

         Put_Line (Exception_Message (Err_Rec.E.all));
         Forth (Iter);
      end loop;
   end Dump_Failure_List;

   --  List successful tests
   procedure Dump_Success_List
     (L : Success_Lists.List; Is_Assertion : Boolean := True) is
      Rec : Test_Success;
      use Success_Lists;

      Iter : List := L;
   begin
      Start (Iter);
      while not Off (Iter) loop
         Rec := Item (Iter);
         Put_Line
           ("      " & Rec.Test_Name.all
            & ": " & Rec.Routine_Name.all);
         Forth (Iter);
      end loop;
   end Dump_Success_List;

   --  Report on a test run
   procedure Report (R : Result) is
   begin
      Put_Line ("   Total Tests Run: " & Natural'Image (Test_Count (R)));

      New_Line;
      Put_Line ("   Successful Tests:" & Natural'Image (Success_Count (R)));

      if Verbose then
         Dump_Success_List (Successes (R));
      end if;


      New_Line;
      Put_Line ("   Failed Tests:" & Natural'Image (Failure_Count (R)));
      Dump_Failure_List (Failures (R));

      New_Line;
      Put_Line ("   Unexpected Errors:" & Natural'Image (Error_Count (R)));
      Dump_Failure_List (Errors (R), False);

      if Elapsed  (R) > 0.0 then
         New_Line;
         Put_Line ("Time: " & Duration'Image (Elapsed (R)) & " seconds");
      end if;
   end Report;

end AUnit.Test_Results.Text_Reporter;
