--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . A S S E R T I O N S                      --
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

--  Version for run-time libraries that do not support exception handling

package body AUnit.Tests.Test_Cases.Assertions is

   ------------
   -- Assert --
   ------------

   procedure Assert
     (T         : access Test_Case'Class;
      Condition : Boolean;
      Message   : String) is
   begin
      if not Condition then
         Register_Failure (T, Message);
      end if;
   end Assert;

   function Assert
     (T         : access Test_Case'Class;
      Condition : Boolean;
      Message   : String) return Boolean is
   begin
      Assert (T, Condition, Message);
      return Condition;
   end Assert;

end AUnit.Tests.Test_Cases.Assertions;
