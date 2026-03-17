------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A U N I T . R E P O R T E R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                      Copyright (C) 2019, AdaCore                         --
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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with AUnit.Test_Info;       use AUnit.Test_Info;

package body AUnit.Reporter is

   procedure Set_File (Engine : in out Reporter; Value : AUnit.IO.File_Access)
   is
   begin
      Engine.File := Value;
   end Set_File;

   procedure Indent_Line (File : File_Type; Indent : Natural := 0) is
   begin
      for J in 1 .. Indent loop
         Put (File, "    ");
      end loop;
   end Indent_Line;

   procedure Put_Line (File : File_Type; Item : String; Indent : Natural) is 
   begin
      Indent_Line (File, Indent);
      AUnit.IO.Put_Line (File, Item);
   end Put_Line;

   procedure Put (File : File_Type; Item : String; Indent : Natural) is
   begin
      Indent_Line (File, Indent);
      AUnit.IO.Put (File, Item);
   end Put;

   procedure Print_Location_Suffix (File : File_Type; Test : Test_Result) is
      procedure Print_Location (File : File_Type; Loc : Tested_Location_Access)
      is
      begin
         if Loc = null then
            return;
         end if;

         Put
           (File,
            Loc.Tested_File.all
            & ":"
            & Trim (Loc.Tested_Line'Image, Left)
            & ":"
            & Trim (Loc.Tested_Column'Image, Left)
            & ":");
         if Loc.Tested_Name /= null then
            Put (File, " (" & Loc.Tested_Name.all & ")");
         end if;
      end Print_Location;
   begin
      Print_Location (File, Test.Location);

      if Test.Suffix /= null then
         if Test.Suffix.Suffix_Text /= null then
            Put (File, " " & Test.Suffix.Suffix_Text.all & " ");
         end if;

         Print_Location (File, Test.Suffix.Suffix_Location);

         if Test.Suffix.Additional_Suffix /= null then
            if Test.Suffix.Additional_Suffix.Suffix_Text /= null then
               Put
                 (File,
                  " " & Test.Suffix.Additional_Suffix.Suffix_Text.all & " ");
            end if;

            Print_Location
              (File, Test.Suffix.Additional_Suffix.Suffix_Location);
            Put (File, " ");
         end if;
      end if;
   end Print_Location_Suffix;

end AUnit.Reporter;
