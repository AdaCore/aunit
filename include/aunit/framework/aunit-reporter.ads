------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A U N I T . R E P O R T E R                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2008-2019, AdaCore                   --
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

with AUnit.IO;           use AUnit.IO;
with AUnit.Options;      use AUnit.Options;
with AUnit.Test_Results; use AUnit.Test_Results;

package AUnit.Reporter is

   Indent_Level : Natural := 0;
   --  Indicate how many indentation the reporter must currently print.

   type Reporter is abstract tagged private;

   procedure Set_File (Engine : in out Reporter; Value : AUnit.IO.File_Access);

   procedure Report
     (Engine  : Reporter;
      R       : in out Result'Class;
      Options : AUnit_Options := Default_Options)
   is abstract;
   --  This procedure is called by AUnit.Run to report the result after running
   --  the whole testsuite (or the selected subset of tests).

   procedure Indent_Line (File : File_Type; Indent : Natural := 0);
   --  Print spaces matching the current indentation level.

   procedure Put_Line (File : File_Type; Item : String; Indent : Natural);
   --  Print the indentation, the string and a linefeed.

   procedure Put (File : File_Type; Item : String; Indent : Natural);
   --  Print the indentation and the string.

private

   procedure Print_Location_Suffix (File : File_Type; Test : Test_Result);

   type Reporter is abstract tagged record
      File : AUnit.IO.File_Access := AUnit.IO.Standard_Output;
   end record;

end AUnit.Reporter;
