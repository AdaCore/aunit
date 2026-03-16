------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A U N I T . I O                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GNAT.IO; use GNAT.IO;

package body AUnit.IO is

   Standard_Out : aliased constant File_Type := 1;

   function Standard_Output
     return File_Access is
     (Standard_Out'Access);

   procedure Put (File : File_Type;
                  Item : Integer;
                  Width : Integer := 0;
                  Base  : Integer := 0) is
      pragma Unreferenced (File, Width, Base);
   begin
      Put (Item);
   end Put;

   procedure Put (File : File_Type;
                  Item : String) is
      pragma Unreferenced (File);
   begin
      Put (Item);
   end Put;

   procedure Put_Line (File : File_Type;
                       Item : String) is
      pragma Unreferenced (File);
   begin
      Put_Line (Item);
   end Put_Line;

   procedure New_Line (File    : File_Type;
                       Spacing : Positive := 1) is
      pragma Unreferenced (File);
   begin
      New_Line (Spacing);
   end New_Line;

end AUnit.IO;
