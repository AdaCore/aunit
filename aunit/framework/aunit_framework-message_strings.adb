------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        A U N I T _ F R A M E W O R K . M E S S A G E _ S T R I N G S     --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.IO;
package body AUnit_Framework.Message_Strings is

   String_Pool : String (Index);
   Next : Index := 1;

   procedure New_Line (Spacing : Positive := 1) renames GNAT.IO.New_Line;

   function New_String (S : String) return Message_String is
      Result : Message_String;
   begin
      if Natural (Next + S'Length) > String_Pool'Last then
         raise String_Pool_Exhausted;
      end if;

      Result.First := Next;
      Result.Last := Next + S'Length - 1;
      Next := Result.Last + 1;
      String_Pool (Result.First .. Result.Last) := S;
      return Result;
   end New_String;

   procedure Put (I : Integer) renames GNAT.IO.Put;

   procedure Put (M : Message_String) is
   begin
      GNAT.IO.Put (String_Pool (M.First .. M.Last));
   end Put;

   procedure Put (S : String) renames GNAT.IO.Put;

   procedure Put_Line (M : Message_String) is
   begin
      GNAT.IO.Put_Line (String_Pool (M.First .. M.Last));
   end Put_Line;

   procedure Put_Line (S : String) renames GNAT.IO.Put_Line;

end AUnit_Framework.Message_Strings;

