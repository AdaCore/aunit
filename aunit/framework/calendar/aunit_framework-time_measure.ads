------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T _ F R A M E W O R K . T I M E _ M E A S U R E         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2006, AdaCore                        --
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

with Ada.Calendar;

package AUnit_Framework.Time_Measure is

   type Time is record
      Start : Ada.Calendar.Time;
      Stop  : Ada.Calendar.Time;
   end record;

   Null_Time : constant Time := (Start => Ada.Calendar.Time_Of (1901, 1, 1),
                                 Stop  => Ada.Calendar.Time_Of (1901, 1, 1));

   procedure Start_Measure (T : in out Time);
   --  Start a new measure

   procedure Stop_Measure (T : in out Time);
   --  Stop the measure

   function Get_Measure (T : in Time) return Duration;
   --  Get the measure

end AUnit_Framework.Time_Measure;
