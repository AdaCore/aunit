------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T I M E _ M E A S U R E                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2019, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

package body AUnit.Time_Measure is

   -------------------
   -- Start_Measure --
   -------------------

   procedure Start_Measure (T : in out Time) is
      pragma Unreferenced (T);
   begin
      null;
   end Start_Measure;

   ------------------
   -- Stop_Measure --
   ------------------

   procedure Stop_Measure (T : in out Time) is
      pragma Unreferenced (T);
   begin
      null;
   end Stop_Measure;

   -----------------
   -- Get_Measure --
   -----------------

   function Get_Measure (T : Time) return AUnit_Duration is
      pragma Unreferenced (T);
   begin
      return 0;
   end Get_Measure;

   ---------------------
   -- Gen_Put_Measure --
   ---------------------

   procedure Gen_Put_Measure (File    : AUnit.IO.File_Type;
                              Measure : AUnit_Duration) is
      pragma Unreferenced (File, Measure);
   begin
      null;
   end Gen_Put_Measure;

   --------------------------------
   -- Gen_Put_Measure_In_Seconds --
   --------------------------------

   procedure Gen_Put_Measure_In_Seconds (File    : AUnit.IO.File_Type;
                                         Measure : AUnit_Duration) is
      pragma Unreferenced (File, Measure);
   begin
      null;
   end Gen_Put_Measure_In_Seconds;

end AUnit.Time_Measure;
