--
--  Copyright (C) 2008, AdaCore
--

with Ada.Unchecked_Deallocation;

package body String_Utils is

   function Start (S : String) return String_Iter is
      I : String_Iter;
   begin
      I := (S => new String'(S),
            Start => S'First,
            Stop  => S'First - 1);
      Next (I);

      return I;
   end Start;

   function Get_Line (I : String_Iter) return String is
   begin
      if I.Start > I.S'Last then
         return "";
      else
         return I.S (I.Start .. I.Stop);
      end if;
   end Get_Line;

   procedure Next (I : in out String_Iter) is
      Trim_First : Boolean;
   begin
      I.Start := I.Stop + 1;
      Trim_First := True;
      for J in I.Start .. I.S'Last loop
         if I.S (J) = ASCII.CR
           or else I.S (J) = ASCII.LF
         then
            if Trim_First then
               I.Start := J + 1;
            else
               I.Stop := J - 1;
               return;
            end if;

         else
            Trim_First := False;
         end if;
      end loop;

      I.Stop := I.S'Last;
   end Next;

   function Has_More (I : String_Iter) return Boolean is
   begin
      return I.Start < I.S'Last;
   end Has_More;

   procedure Free (I : in out String_Iter) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (String, String_Access);
   begin
      Internal (I.S);
   end Free;

end String_Utils;
