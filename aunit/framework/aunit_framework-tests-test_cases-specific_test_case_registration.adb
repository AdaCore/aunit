------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T. T E S T _ C A S E S . R E G I S T R A T I O N        --
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

--  Test routine registration
with Ada.Unchecked_Conversion;

separate (AUnit_Framework.Tests.Test_Cases)
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
