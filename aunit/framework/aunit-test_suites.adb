------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ S U I T E S                    --
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

--  A collection of test cases and sub-suites.
with Ada.Unchecked_Deallocation;
package body AUnit.Test_Suites is

   type Access_Test is access all Test'Class;

   procedure Destroy is
      new Ada.Unchecked_Deallocation (Test'Class, Access_Test);

   --  Add a test case or sub-suite to this one:
   procedure Add_Test (S : access Test_Suite; T : access Test'Class) is
   begin
      Extend (S.Tests, T.all);
   end Add_Test;

   --  Run each test case in this suite.  Run sub-suite test cases
   --  recursively:
   procedure Run (S : in out Test_Suite; R : in out Result) is
   begin
      Start (S.Tests);

      while not Off (S.Tests) loop
         declare
            Dispatcher : Test'Class := Item (S.Tests);
         begin
            Tests.Run (Dispatcher, R);
         end;

         Forth (S.Tests);
      end loop;
   end Run;

end AUnit.Test_Suites;
