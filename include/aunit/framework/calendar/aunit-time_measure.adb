------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T I M E _ M E A S U R E                  --
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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

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

   function Get_Measure (T : Time) return AUnit_Duration is
      use type Ada.Calendar.Time;
   begin
      return AUnit_Duration (T.Stop - T.Start);
   end Get_Measure;

   ---------------------
   -- Gen_Put_Measure --
   ---------------------

   procedure Gen_Put_Measure (File    : AUnit.IO.File_Type;
                              Measure : AUnit_Duration) is
      H, M, S  : Integer := 0;
      T        : Duration := Duration (Measure);
      Force    : Boolean;

      procedure Put (N : Integer; Length : Integer);
      --  Put N using at least Length digits.

      procedure Put (N : Integer; Length : Integer) is
      begin
         for Dig in reverse 1 .. Length - 1 loop
            if N < 10**Dig then
               Put (File, "0");
            else
               exit;
            end if;
         end loop;

         Put (File, Trim (N'Img, Left));
      end Put;

   begin
      --  Calculate the number of hours, minutes and seconds
      while T >= 3600.0 loop
         H := H + 1;
         T := T - 3600.0;
      end loop;

      while T >= 60.0 loop
         M := M + 1;
         T := T - 60.0;
      end loop;

      while T >= 1.0 loop
         S := S + 1;
         T := T - 1.0;
      end loop;

      --  Now display the result
      Force := False;

      if H > 0 then
         Put (File, Trim (H'Img, Left));
         Put (File, "h");
         Force := True;
      end if;

      if M > 0 or else Force then
         if not Force then
            Put (File, Trim (M'Img, Left));
         else
            --  In case some output is already done, then we force a 2 digits
            --  output so that the output is normalized.
            Put (M, 2);
         end if;

         Put (File, "min. ");
         Force := True;
      end if;

      if not Force then
         Put (File, Trim (S'Img, Left));
      else
         Put (S, 2);
      end if;

      Put (File, ".");
      Put (Integer (T * 1_000_000.0), 6);
      Put (File, " sec.");
   end Gen_Put_Measure;

   --------------------------------
   -- Gen_Put_Measure_In_Seconds --
   --------------------------------

   procedure Gen_Put_Measure_In_Seconds (File    : AUnit.IO.File_Type;
                                         Measure : AUnit_Duration) is
      S  : Integer := 0;
      T  : Duration := Duration (Measure);

      procedure Put (N : Integer; Length : Integer);
      --  Put N using at least Length digits.

      procedure Put (N : Integer; Length : Integer) is
      begin
         for Dig in reverse 1 .. Length - 1 loop
            if N < 10**Dig then
               Put (File, "0");
            else
               exit;
            end if;
         end loop;

         Put (File, Trim (N'Img, Left));
      end Put;

   begin

      while T >= 1.0 loop
         S := S + 1;
         T := T - 1.0;
      end loop;

      Put (File, Trim (S'Img, Left));

      Put (File, ".");
      Put (Integer (T * 1_000_000.0), 9);
      Put (File, "s");
   end Gen_Put_Measure_In_Seconds;

end AUnit.Time_Measure;
