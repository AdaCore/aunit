------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            A U N I T . R U N                             --
--                                                                          --
--                                 S p e c                                  --
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

with AUnit.Reporter;
with AUnit.Test_Suites;

--  Framework using text reporter
package AUnit.Run is

   generic
      with function Suite return AUnit.Test_Suites.Access_Test_Suite;
   procedure Test_Runner
     (Reporter : AUnit.Reporter.Reporter'Class;
      Options  : AUnit.AUnit_Options := Default_Options);

   generic
      with function Suite return AUnit.Test_Suites.Access_Test_Suite;
   function Test_Runner_With_Status
     (Reporter : AUnit.Reporter.Reporter'Class;
      Options  : AUnit.AUnit_Options := Default_Options) return Status;

end AUnit.Run;
