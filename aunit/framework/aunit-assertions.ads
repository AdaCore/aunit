------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T  . A S S E R T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2008, AdaCore                   --
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

with GNAT.Source_Info;

package AUnit.Assertions is

   Assertion_Error : exception;
   --  For run-time libraries that support exception handling, raised when an
   --  assertion fails in order to abandon execution of a test routine

   procedure Assert
     (Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);
   --  Test "Condition" and record "Message" if false.
   --  If the Ada run-time library supports exception handling, a failed
   --  condition passed to this routine causes the calling routine to be
   --  abandoned. Otherwise, a failed assertion returns and continues the
   --  caller.

   function Assert
     (Condition : Boolean;
      Message   : String;
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line) return Boolean;
   --  Functional version to allow calling routine to decide whether to
   --  continue or abandon execution

end AUnit.Assertions;
