------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           A U N I T . L I S T S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2005, AdaCore                     --
--                                                                          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

--  Simple linked lists. Adapted from EiffelBase LINKED_LIST
package body AUnit.Lists is

   --  Local Specs:
   function Last_Element (L : List) return Linkable_Access;
   --  Last node

   function Previous (L : List) return Linkable_Access;
   --  Previous node

   procedure Destroy is
      new Ada.Unchecked_Deallocation (Element, Element_Access);
   --  Cleanup discarded elements

   procedure Destroy is
      new Ada.Unchecked_Deallocation (Linkable, Linkable_Access);
   --  Cleanup discarded nodes


   --  Accessors:
   function First (L : List) return Element is
      --  First Item on list
   begin
      return L.First_Element.Item.all;
   end First;


   function Index (L : List) return Natural is
      --  Current index
      --  Doing it this way in case of list merging
      P : Linkable_Access;
      Iter : List := L;
      Result : Natural  := 0;
   begin
      if After (Iter) then
         Result := Count (Iter) + 1;
      elsif not Before (Iter) then
         P := L.Active;
         Start (Iter);
         Result := 1;
         while Iter.Active /= P loop
            Result := Result + 1;
            Forth (Iter);
         end loop;
      end if;

      return Result;
   end Index;


   function Item (L : List) return Element is
      --  Currrent item
   begin
      return L.Active.Item.all;
   end Item;


   function Last (L : List) return Element is
      --  Last item
   begin
      return Last_Element (L).Item.all;
   end Last;


   --  Measurement:
   function Count (L : List) return Natural is
      --  Number of items in list
   begin
      return L.Count;
   end Count;


   --  Status report:
   function Empty (L : List) return Boolean is
      --  Empty list?
   begin
      return Count (L) = 0;
   end Empty;


   function After (L : List) return Boolean is
      --  No valid cursor position to the right?
   begin
      return L.After;
   end After;

   function Before (L : List) return Boolean is
      --  No valid cursor position to the left?
   begin
      return L.Before;
   end Before;

   function Is_First (L : List) return Boolean is
      --  Cursor at first position?
   begin
      return not After (L) and not Before (L)
        and L.Active = L.First_Element;
   end Is_First;

   function Is_Last (L : List) return Boolean is
      --  Cursor at last position?
   begin
      return (not After (L) and not Before (L)
              and (L.Active /= null)) and then (L.Active.Right = null);
   end Is_Last;

   function Off (L : List) return Boolean is
      --  No current item?
   begin
      return L.After or else L.Before;
   end Off;


   --  Cursor movement:
   procedure Back (L : in out List) is
      --  Move to previous position
   begin
      if Empty (L) then
         L.Before := True;
         L.After := False;
      elsif After (L) then
         L.After := False;
      elsif Is_First (L) then
         L.Before := True;
      else
         L.Active := Previous (L);
      end if;
   end Back;


   procedure Finish (L : in out List)is
      --  Move to last position
      P : Linkable_Access;
   begin
      if not Empty (L) then
         P := L.Active;
         while P.Right /= null loop
            P := P.Right;
         end loop;

         L.Active := P;
         L.After := False;
         L.Before := False;
      else
         L.Before := True;
         L.After := False;
      end if;
   end Finish;


   procedure Forth (L : in out List) is
      --  Move to next position
      Old_Active : Linkable_Access;
   begin
      if Before (L) then
         L.Before := False;
         if Empty (L) then
            L.After := True;
         end if;
      else
         Old_Active := L.Active;
         L.Active := L.Active.Right;
         if L.Active = null then
            L.Active := Old_Active;
            L.After := True;
         end if;
      end if;
   end Forth;

   procedure Go_I_Th (L : in out List; I : Natural) is
      --  Move to i'th position
   begin
      if I = 0 then
         L.Before := True;
         L.After := False;
         L.Active := L.First_Element;
      elsif I = Count (L) + 1 then
         L.Before := False;
         L.After := True;
         L.Active := Last_Element (L);
      else
         Move (L, I - Index (L));
      end if;
   end Go_I_Th;

   procedure Move (L : in out List; I : Integer) is
      --  Move I positions
      Counter : Natural := 0;
      New_Index : Integer := 0;
      P : Linkable_Access;
   begin
      if I > 0 then
         if Before (L) then
            L.Before := False;
            Counter := 1;
         end if;

         P := L.Active;
         while (Counter /= I) and then (P /= null) loop
            L.Active := P;
            P := P.Right;
            Counter := Counter + 1;
         end loop;

         if P = null then
            L.After := True;
         else
            L.Active := P;
         end if;

      elsif I < 0 then
         New_Index := Index (L) + I;
         L.Before := True;
         L.After := False;
         L.Active := L.First_Element;
         if New_Index > 0 then
            Move (L, New_Index);
         end if;
      end if;
   end Move;

   procedure Start (L : in out List) is
      --  Move to first position
   begin
      if L.First_Element /= null then
         L.Active := L.First_Element;
         L.After := False;
      else
         L.After := True;
      end if;
      L.Before := False;
   end Start;

   --  Element change:

   procedure Extend (L : in out List; E : Element) is
      --  Add E to end. Do not move cursor
      P : constant Linkable_Access := new Linkable'(new Element'(E), null);
   begin
      if Empty (L) then
         L.First_Element := P;
         L.Active := P;
      else
         Put_Right (Last_Element (L), P);
         if After (L) then
            L.Active := P;
         end if;
      end if;
      L.Count := L.Count + 1;
   end Extend;


   procedure Put_Front (L : in out List; E : Element) is
      --  Add E to start.  Do not move cursor
      P : constant Linkable_Access := new Linkable'(new Element'(E), null);
   begin
      Put_Right (P, L.First_Element);
      L.First_Element := P;

      if Before (L) or else Empty (L) then
         L.Active := P;
      end if;
      L.Count := L.Count + 1;
   end Put_Front;

   procedure Put_Left (L : in out List; E : Element) is
      --  Add E to left of cursor. Do not move cursor
      P : Linkable_Access;
   begin
      if Empty (L) then
         Put_Front (L, E);
      elsif After (L) then
         Back (L);
         Put_Right (L, E);
         Move (L, 2);
      else
         P := new Linkable'(L.Active.Item, null);
         Put_Right (P, L.Active.Right);
         L.Active.Item := new Element'(E);
         Put_Right (L.Active, P);
         L.Active := P;
         L.Count := L.Count + 1;
      end if;
   end Put_Left;

   procedure Put_Right (L : in out List; E : Element) is
      --  Add E to right of cursor.  Do not move cursor
      P : constant Linkable_Access := new Linkable'(new Element'(E), null);
   begin
      if Before (L) then
         Put_Right (P, L.First_Element);
         L.First_Element := P;
         L.Active := P;
      else
         Put_Right (P, L.Active.Right);
         Put_Right (L.Active, P);
      end if;
      L.Count := L.Count + 1;
   end Put_Right;



   procedure Replace (L : in out List; E : Element) is
      --  Replace current item with E
   begin
      L.Active.Item.all := E;
   end Replace;


   --  Removal:
   procedure Remove (L : in out List) is
      --  Remove current item.  Move cursor to right
      Removed, Succ : Linkable_Access;
   begin
      Removed := L.Active;
      if Is_First (L) then
         L.First_Element := L.First_Element.Right;
         L.Active.Right := null;
         L.Active := L.First_Element;

         if Count (L) = 1 then
            L.After := True;
         end if;

      elsif Is_Last (L) then
         L.Active := Previous (L);
         if L.Active /= null then
            if L.Active /= null then
               L.Active.Right := null;
            end if;
         end if;
         L.After := True;
      else
         Succ := L.Active.Right;
         Put_Right (Previous (L), Succ);
         L.Active.Right := null;
         L.Active := Succ;
      end if;

      Destroy (Removed.Item);
      Destroy (Removed);
      L.Count := L.Count - 1;
   end Remove;


   procedure Remove_Left (L : in out List) is
      --  Remove item to left of cursor.  Do not move cursor
   begin
      Move (L, -2);
      Remove_Right (L);
      Forth (L);
   end Remove_Left;

   procedure Remove_Right (L : in out List) is
      --  Remove item to right of cursor.  Do not move cursor
      Removed, Succ : Linkable_Access;
   begin
      if Before (L) then
         Removed := L.First_Element;
         L.First_Element := L.First_Element.Right;
         L.Active.Right :=  null;
         L.Active := L.First_Element;
      else
         Succ := L.Active.Right;
         Removed := Succ;
         Put_Right (L.Active, Succ.Right);
         Succ.Right := null;
      end if;
      L.Count := L.Count - 1;
      Destroy (Removed.Item);
      Destroy (Removed);
   end Remove_Right;

   procedure Wipe_Out (L : in out List) is
      --  Remove all items.
   begin
      Start (L);
      while not Off (L) loop
         Remove (L);
      end loop;
   end Wipe_Out;


   --  Local bodies:
   function Last_Element (L : List) return Linkable_Access is
      --  Last node
      P, Result : Linkable_Access;
   begin
      if not Empty (L) then
         Result := L.Active;
         P := L.Active.Right;
         while P /= null loop
            Result := P;
            P := P.Right;
         end loop;
      end if;
      return Result;
   end Last_Element;


   function Previous (L : List) return Linkable_Access is
      --  Previous node
      P, Result : Linkable_Access;
   begin
      if After (L) then
         Result := L.Active;
      elsif not (Is_First (L) or Before (L)) then
         P := L.First_Element;
         while P.Right /= L.Active loop
            P := P.Right;
         end loop;
         Result := P;
      end if;
      return Result;
   end Previous;


   procedure Put_Right (L : Linkable_Access; R : Linkable_Access) is
      --  Add R to right of L
   begin
      L.Right := R;
   end Put_Right;

end AUnit.Lists;

