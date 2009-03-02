------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T . T E S T _ C A S E S . R U N _ R O U T I N E         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2009, AdaCore                      --
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

with AUnit.Last_Chance_Handler;
separate (AUnit.Simple_Test_Cases)

--  Version for run-time libraries that support exception handling via gcc
--  builtin setjmp/longjmp mechanism.

procedure Run_Routine
  (Test    : access Test_Case'Class;
   R       : access Result;
   Outcome : out Status) is

   Unexpected_Exception : Boolean := False;

   function String_Compare (Str1, Str2 : String) return Boolean;
   --  Compares two strings.

   procedure Internal_Run_Test;
   --  Wrapper for running the test case

   --------------------
   -- String_Compare --
   --------------------

   function String_Compare (Str1, Str2 : String) return Boolean is
   begin
      if Str1'Length /= Str2'Length then
         return False;
      end if;
      for J in Str1'Range loop
         if Str1 (J) /= Str2 (J - Str1'First + Str2'First) then
            return False;
         end if;
      end loop;
      return True;
   end String_Compare;

   -----------------------
   -- Internal_Run_Test --
   -----------------------

   procedure Internal_Run_Test is
   begin
      AUnit.Simple_Test_Cases.Run_Test (Test.all);
   end Internal_Run_Test;

   function Internal_Setjmp is new AUnit.Last_Chance_Handler.Gen_Setjmp
     (Internal_Run_Test);

   Res : Integer;

   use Failure_Lists;

begin

   --  Reset failure list to capture failed assertions for one routine

   Clear (Test.Failures);

   begin
      Res := Internal_Setjmp;

      if Res /= 0 then
         declare
            Src : constant Message_String :=
                    AUnit.Last_Chance_Handler.Get_Source;
         begin
            if not String_Compare (Src.all, "aunit-assertions.adb:47") then
               Unexpected_Exception := True;
               Add_Error
                 (R.all,
                  Name (Test.all),
                  Routine_Name (Test.all),
                  (AUnit.Last_Chance_Handler.Get_Last_Msg,
                   AUnit.Last_Chance_Handler.Get_Source,
                   AUnit.Last_Chance_Handler.Get_Line));
            end if;
         end;
      end if;
   end;

   if not Unexpected_Exception and then Is_Empty (Test.Failures) then
      Outcome := Success;
      Add_Success (R.all, Name (Test.all), Routine_Name (Test.all));
   else
      Outcome := Failure;
      declare
         C : Cursor := First (Test.Failures);
      begin
         while Has_Element (C) loop
            Add_Failure
              (R.all, Name (Test.all), Routine_Name (Test.all), Element (C));
            Next (C);
         end loop;
      end;
   end if;
end Run_Routine;
