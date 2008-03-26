------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T . T E S T _ C A S E S . R U N _ R O U T I N E         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2008, AdaCore                      --
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

with Ada.Exceptions; use Ada.Exceptions;

separate (AUnit.Test_Cases)

--  Version for run-time libraries that support exception handling
procedure Run_Routine
  (Test    : access Test_Case'Class;
   Subtest : Routine_Spec;
   R       : access Result;
   Outcome : out Status) is

   Unexpected_Exception : Boolean := False;

   use Failure_Lists;

begin

   --  Reset failure list to capture failed assertions for one routine

   Clear (Test.Failures);

   --  Run test routine

   Set_Up (Test.all);

   begin
      Subtest.Routine.all (Test.all);
   exception
      when Assertion_Error =>
         null;
      when E : others =>
         Unexpected_Exception := True;
         Add_Error (R.all,
                    Name (Test.all),
                    Subtest.Routine_Name,
                    Format_Message (Exception_Name (E)));
   end;

   Tear_Down (Test.all);

   if not Unexpected_Exception and then Is_Empty (Test.Failures) then
      Outcome := Success;
      Add_Success (R.all, Name (Test.all), Subtest.Routine_Name);
   else
      Outcome := Failure;
      declare
         C : Cursor := First (Test.Failures);
      begin
         while Has_Element (C) loop
            Add_Failure (R.all,
                         Name (Test.all),
                         Subtest.Routine_Name,
                         Element (C));
            Next (C);
         end loop;
      end;
   end if;
end Run_Routine;
