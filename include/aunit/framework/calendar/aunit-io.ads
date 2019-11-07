------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A U N I T . R E P O R T E R                       --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Text_IO;
with Ada.Integer_Text_IO;

package AUnit.IO is

   subtype File_Type is Ada.Text_IO.File_Type;

   subtype File_Access is Ada.Text_IO.File_Access;

   function Standard_Output
     return File_Access renames Ada.Text_IO.Standard_Output;

   procedure Put (File  : File_Type;
                  Item  : Integer;
                  Width : Ada.Text_IO.Field := Ada.Integer_Text_IO.Default_Width;
                  Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base)
                  renames Ada.Integer_Text_IO.Put;

   procedure Put (File : File_Type;
                  Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line (File : File_Type;
                       Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line (File    : File_Type;
                       Spacing : Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

end AUnit.IO;
