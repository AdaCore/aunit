------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . A S S E R T I O N S                      --
--                                                                          --
--                                 B o d y                                  --
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

--  Version for run-time libraries that support exception handling via
--  gcc builtin setjmp/longjmp

with AUnit.Last_Chance_Handler;

separate (AUnit.Assertions)
procedure Assert_Exception
  (Message : String;
   Source  : String := GNAT.Source_Info.File;
   Line    : Natural := GNAT.Source_Info.Line)
is
   function My_Setjmp is new AUnit.Last_Chance_Handler.Gen_Setjmp (Proc);
begin
   if My_Setjmp = 0 then
      --  Result is 0 when no exception has been raised.
      Register_Failure (Message, Source, Line);
   end if;
end Assert_Exception;
