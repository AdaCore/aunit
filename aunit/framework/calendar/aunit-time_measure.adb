------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T _ F R A M E W O R K . T I M E _ M E A S U R E         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2007, AdaCore                      --
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

package body AUnit.Time_Measure is

   -------------------
   -- Start_Measure --
   -------------------

   procedure Start_Measure (T : in out Time) is
   begin
      T.Start := Ada.Calendar.Clock;
   end Start_Measure;

   ------------------
   -- Stop_Measure --
   ------------------

   procedure Stop_Measure (T : in out Time) is
   begin
      T.Stop := Ada.Calendar.Clock;
   end Stop_Measure;

   -----------------
   -- Get_Measure --
   -----------------

   function Get_Measure (T : in Time) return AUnit_Duration is
      use type Ada.Calendar.Time;
   begin
      return AUnit_Duration (T.Stop - T.Start);
   end Get_Measure;

   ---------
   -- Put --
   ---------

   procedure Gen_Put_Measure (Measure : AUnit_Duration) is
      T   : Duration := Duration (Measure);
      Exp : Integer;
      Dec : Integer;
      Val : Integer;

   begin
      Exp := 0;

      while T >= 10.0 loop
         T := T / 10.0;
         Exp := Exp + 1;
      end loop;

      while T < 1.0 loop
         T := T * 10.0;
         Exp := Exp - 1;
      end loop;

      --  We have here 1.0 <= T < 10.0

      --  Integer (T - 0.5) is equivalent to Float'Floor, but works
      --  with durations !
      Val := Integer (T - 0.5);
      T := (T - Duration (Val)) * 1000.0;
      Dec := Integer (T - 0.5);

      Put (Val);
      Put (".");

      if Dec < 10 then
         Put ("00");
      elsif Dec < 100 then
         Put ("0");
      end if;

      Put (Dec);

      if Exp /= 0 then
         Put ("E");
         Put (Exp);
      end if;
   end Gen_Put_Measure;

end AUnit.Time_Measure;
