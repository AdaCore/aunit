------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T E S T _ R U N N E R                    --
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

with AUnit_Framework.Test_Results.Text_Reporter;

--  Framework using text reporter
package body AUnit_Framework.Framework is

   package Reporter is new Test_Results.Text_Reporter;

   package body Harness is

      Results : aliased Test_Results.Result;
      --  Test results for one harness run

      ---------
      -- Run --
      ---------

      procedure Run (Timed : Boolean := False) is
         pragma Unreferenced (Timed);
      begin
         Test_Suites.Run (Suite, Results'Access);
         Reporter.Report (Results);
      end Run;

   end Harness;

end AUnit_Framework.Framework;
