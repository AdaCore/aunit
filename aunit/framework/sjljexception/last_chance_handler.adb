------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     L A S T _ C H A N C E _ H A N D L E R                --
--                                                                          --
--                                 B o d y                                  --
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
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C;
with Ada.Unchecked_Conversion;

package body Last_Chance_Handler is

   Exception_Msg    : Message_String := null;
   Exception_Source : Message_String := null;
   Exception_Line   : Natural := 0;

   The_Test : AUnit.Simple_Test_Cases.Test_Case_Access := null;

   procedure Test_Runner;
   --  Wrapper called by the C setjmp that runs the test case

   function C_Setjmp (Acc : System.Address) return Integer;
   pragma Import (C, C_Setjmp, "mysetjmp");
   procedure C_Longjmp;
   pragma Import (C, C_Longjmp, "mylongjmp");
   pragma No_Return (C_Longjmp);

   ---------------------------
   --  C Strings management --
   ---------------------------

   type chars_ptr is access all Character;
   pragma No_Strict_Aliasing (chars_ptr);

   function To_chars_ptr is
      new Ada.Unchecked_Conversion (System.Address, chars_ptr);

   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, System.Address);

   function To_Ada (Item : chars_ptr) return Message_String;

   -----------------
   -- Test_Runner --
   -----------------

   procedure Test_Runner is
   begin
      AUnit.Simple_Test_Cases.Run_Test (The_Test);
   end Test_Runner;

   ------------
   -- Setjmp --
   ------------

   function Setjmp (Test : AUnit.Simple_Test_Cases.Test_Case_Access)
                    return Integer is
   begin
      The_Test := Test;
      return C_Setjmp (Test_Runner'Address);
   end Setjmp;

   ------------------
   -- Get_Last_Msg --
   ------------------

   function Get_Last_Msg return Message_String is
   begin
      if Exception_Msg = null then
         return AUnit.Message_Alloc (0);
      else
         return Exception_Msg;
      end if;
   end Get_Last_Msg;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source return Message_String is
   begin
      if Exception_Msg = null then
         return AUnit.Message_Alloc (0);
      else
         return Exception_Source;
      end if;
   end Get_Source;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return Natural is
   begin
      if Exception_Msg = null then
         return 0;
      else
         return Exception_Line;
      end if;
   end Get_Line;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Item : chars_ptr) return Message_String is
      use Interfaces.C;
      Result : Message_String;
      Length : size_t := 0;

      function "+" (Left : chars_ptr; Right : size_t) return chars_ptr;
      function Peek (From : chars_ptr) return char;
      function To_Ada (Item : char) return Character;
      function "+" (Left : chars_ptr; Right : size_t) return chars_ptr is
      begin
         return To_chars_ptr (To_Address (Left) + Storage_Offset (Right));
      end "+";
      function Peek (From : chars_ptr) return char is
      begin
         return char (From.all);
      end Peek;
      function To_Ada (Item : char) return Character is
      begin
         return Character'Val (char'Pos (Item));
      end To_Ada;
   begin
      if Item = null then
         return null;
      end if;

      loop
         if Peek (Item + Length) = nul then
            exit;
         end if;

         Length := Length + 1;
      end loop;

      Result := AUnit.Message_Alloc (Natural (Length));

      for J in Result'Range loop
         Result (J) := To_Ada (Peek (Item + size_t (J - 1)));
      end loop;

      return Result;
   end To_Ada;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
   begin
      --  Save the exception message before performing the longjmp
      Exception_Msg    := To_Ada (To_chars_ptr (Msg));
      Exception_Source := Exception_Msg;
      Exception_Line   := Line;
      --  No return procedure.
      C_Longjmp;
   end Last_Chance_Handler;

end Last_Chance_Handler;
