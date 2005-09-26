------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ S U I T E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2005, AdaCore                     --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

--  A collection of test cases and sub-suites.
package body AUnit.Test_Suites is

   --  Add a test case or sub-suite to this one:
   procedure Add_Test (S : access Test_Suite; T : access Test'Class) is
   begin
      Extend (S.Test_Set, T.all);
   end Add_Test;

   --  Run each test case in this suite.  Run sub-suite test cases
   --  recursively:
   procedure Run (S : in out Test_Suite; R : in out Result) is
   begin
      Start (S.Test_Set);

      while not Off (S.Test_Set) loop
         declare
            Dispatcher : Test'Class := Item (S.Test_Set);
         begin
            Tests.Run (Dispatcher, R);
         end;

         Remove (S.Test_Set);
      end loop;
   end Run;

end AUnit.Test_Suites;
