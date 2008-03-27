------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     L A S T _ C H A N C E _ H A N D L E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                      Copyright (C) 2008, AdaCore                         --
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

--  This last chance handler implementation performs a longjmp using gcc
--  builtin to allow exception simulation on platforms where the run-time does
--  not allow exception propagation.

with System;
with AUnit; use AUnit;
with AUnit.Simple_Test_Cases;

package Last_Chance_Handler is

   function Get_Last_Msg return Message_String;
   function Get_Source return Message_String;
   function Get_Line return Natural;
   --  Return the last exception message

   function Setjmp (Test : AUnit.Simple_Test_Cases.Test_Case_Access)
                    return Integer;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

end Last_Chance_Handler;
