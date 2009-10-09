------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . T E X T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2009, AdaCore                   --
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

package AUnit.Reporter.Text is

   type Text_Reporter is new Reporter with null record;
   --  A very simple reporter to console
   --  All results are printed on the console

   procedure Report (Self : Text_Reporter; R : in out Result);

   type Result_Type is (Success, Failure, Error);
   procedure Put_Result_List
      (Self : Text_Reporter; List : Result_Lists.List; Kind : Result_Type);
   --  Report the list of tests that where run, along with their types

   type Text_Color_Reporter is new Text_Reporter with null record;
   --  A text reporter that highlights results in color (green for success,
   --  red for failure or error).
   --  This should only be used if your console supports color, since
   --  otherwise the control characters will make the output confusing

   procedure Put_Result_List
      (Self : Text_Color_Reporter;
       List : Result_Lists.List;
       Kind : Result_Type);

end AUnit.Reporter.Text;
