------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        A U N I T _ F R A M E W O R K . M E S S A G E _ S T R I N G S     --
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

generic
   Message_String_Pool_Size : Positive;
   --  Handling of strings used in reports
package AUnit_Framework.Message_Strings is

   type Message_String is private;

   String_Pool_Exhausted : exception;

   procedure New_Line (Spacing : Positive := 1);
   function New_String (S : String) return Message_String;
   procedure Put (I : Integer);
   procedure Put (M : Message_String);
   procedure Put (S : String);
   procedure Put_Line (M : Message_String);
   procedure Put_Line (S : String);

private
   subtype Index is Positive range 1 .. Message_String_Pool_Size;
   type Message_String is record
      First, Last : Index;
   end record;
end AUnit_Framework.Message_Strings;
