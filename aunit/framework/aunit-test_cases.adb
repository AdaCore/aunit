------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
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
with Ada.Unchecked_Conversion;

--  Test cases
package body AUnit.Test_Cases is

   package body Registration is separate;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize (T : access Test_Case'Class);
   --  Initialize test case

   -----------------
   -- Add_Routine --
   -----------------

   procedure Add_Routine (T : in out Test_Case'Class; Val : Routine_Spec) is
   begin
      Routine_Lists.Append (T.Routines, Val);
   end Add_Routine;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : access Test_Case'Class) is
   begin
      Routine_Lists.Clear (T.Routines);
      Register_Tests (T.all);
   end Initialize;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Test_Case) is
   begin
      Test.Routine.Routine (Test);
   end Run_Test;

   ---------
   -- Run --
   ---------

   procedure Run (Test : access Test_Case;
                  R    : Result_Access;
                  Outcome : out Status) is
      use Routine_Lists;
      Result : Status;
      C : Cursor;
   begin
      Outcome := Success;
      Initialize (Test);
      Set_Up_Case (Test_Case'Class (Test.all));
      C := First (Test.Routines);

      while Has_Element (C) loop
         Test.Routine := Element (C);
         AUnit.Simple_Test_Cases.Run
           (AUnit.Simple_Test_Cases.Test_Case (Test.all)'Access, R, Result);
         if Result = Failure then
            Outcome := Failure;
         end if;
         Next (C);
      end loop;

      Tear_Down_Case (Test_Case'Class (Test.all));
   end Run;

   -----------------
   -- Format_Name --
   -----------------

   function Format_Name (Test : Test_Case) return Message_String is
      Test_Name : Message_String :=
                    AUnit.Simple_Test_Cases.Name
                      (AUnit.Simple_Test_Cases.Test_Case'Class (Test));
      Ret       : constant Message_String :=
                    Message_Alloc (Test_Name'Length +
                                         Test.Routine.Routine_Name'Length + 3);
   begin
      for J in 1 .. Test_Name'Length loop
         Ret (J) := Test_Name (J - 1 + Test_Name'First);
      end loop;

      Ret (Test_Name'Length + 1) := ' ';
      Ret (Test_Name'Length + 2) := ':';
      Ret (Test_Name'Length + 3) := ' ';

      for J in Test_Name'Length + 4 .. Ret'Last loop
         Ret (J) := Test.Routine.Routine_Name
           (J - Test_Name'Length - 4 + Test.Routine.Routine_Name'First);
      end loop;

      Message_Free (Test_Name);

      return Ret;
   end Format_Name;

   ------------------
   --  Set_Up_Case --
   ------------------

   procedure Set_Up_Case (Test : in out Test_Case) is
      --  Default
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   procedure Tear_Down_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down_Case;

   package body Specific_Test_Case_Registration is

      ----------------------
      -- Register_Wrapper --
      ----------------------

      procedure Register_Wrapper
        (Test    : in out Specific_Test_Case'Class;
         Routine : Specific_Test_Routine;
         Name    : String) is

         function Conv is
            new Ada.Unchecked_Conversion (Specific_Test_Routine, Test_Routine);

      begin
         Registration.Register_Routine
           (Test_Case'Class (Test),
            Conv (Routine),
            Name);
      end Register_Wrapper;

   end Specific_Test_Case_Registration;

end AUnit.Test_Cases;
