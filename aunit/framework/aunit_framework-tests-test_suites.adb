------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ S U I T E S                    --
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
      Append (S.Tests, Test_Access'(T.all'Unchecked_Access));
   end Add_Test;

   ---------
   -- Run --
   ---------

   procedure Run (S : access Test_Suite; R : Result_Access) is
      C : Cursor := First (S.Tests);
   begin
      while Has_Element (C) loop
         Run (Element (C), R);
         Next (C);
      end loop;
   end Run;

end AUnit_Framework.Tests.Test_Suites;
