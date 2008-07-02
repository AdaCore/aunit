--
--  Copyright (C) 2008, AdaCore
--

package String_Utils is

   type String_Iter is private;

   function Start (S : String) return String_Iter;

   function Get_Line (I : String_Iter) return String;

   procedure Next (I : in out String_Iter);

   function Has_More (I : String_Iter) return Boolean;

   procedure Free (I : in out String_Iter);

private

   type String_Access is access all String;
   type String_Iter is record
      S           : String_Access;
      Start, Stop : Natural;
   end record;

end String_Utils;
