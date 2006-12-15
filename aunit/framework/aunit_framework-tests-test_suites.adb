------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A U N I T _ F R A M E W O R K . T E S T S . T E S T _ S U I T E S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2006, AdaCore                   --
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

--  A collection of test cases
package body AUnit_Framework.Tests.Test_Suites is

   --------------
   -- Add_Test --
   --------------

   procedure Add_Test (S : access Test_Suite'Class; T : access Test'Class) is
   begin
      if Length (S.Tests) =  Count_Type (Max_Test_Cases_Per_Suite) then
         if T.all in Test_Case'Class then
            declare
               Error_Message       : constant String :=
                                      " overflows Max_Tests_Per_Suite:";
            begin
               Put (Name (Test_Case'Class (T.all)));
               Put_Line (Error_Message);
            end;
         else
            Put_Line ("Max_Tests_Per_Suite overflowed when adding sub-suite");
         end if;
      else
         Append (S.Tests, Test_Access'(T.all'Unchecked_Access));
      end if;
   end Add_Test;

   ---------
   -- Run --
   ---------

   procedure Run (Suite : access Test_Suite;
                  R       : Result_Access;
                  Outcome : out Status) is
      C : Cursor := First (Suite.Tests);
      Result : Status := Success;
   begin
      Outcome := Success;
      while Has_Element (C) loop
         Run (Element (C), R, Result);
         if Result = Failure then
            Outcome := Failure;
         end if;
         Next (C);
      end loop;
   end Run;

end AUnit_Framework.Tests.Test_Suites;
