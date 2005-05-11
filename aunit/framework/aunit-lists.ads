------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           A U N I T . L I S T S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                      Copyright (C) 2000-2005 AdaCore                     --
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
generic
   type Element (<>) is private;

   --  Simple generic linked lists. Adapted from EiffelBase LINKED_LIST
package AUnit.Lists is
   pragma Preelaborate (AUnit.Lists);

   type List is tagged private;

   --  Accessors:

   --  First Item on list
   function First (L : List) return Element;

   --  Current index
   function Index (L : List) return Natural;

   --  Currrent item
   function Item (L : List) return Element;

   --  Last item
   function Last (L : List) return Element;


   --  Measurement:

   --  Number of items in list
   function Count (L : List) return Natural;


   --  Status report:

   --  Empty list?
   function Empty (L : List) return Boolean;

   --  No valid cursor position to the right?
   function After (L : List) return Boolean;

   --  No valid cursor position to the left?
   function Before (L : List) return Boolean;

   --  Cursor at first position?
   function Is_First (L : List) return Boolean;

   --  Cursor at last position?
   function Is_Last (L : List) return Boolean;

   --  No current item?
   function Off (L : List) return Boolean;


   --  Cursor movement:

   --  Move to previous position
   --  require: not Before (L)
   --  ensure: Index (L) = old Index (L) - 1
   procedure Back (L : in out List);

   --  Move to last position
   --  ensure: not Empty (L) implies Last (L)
   --          Empty (L) implies Before (L)
   procedure Finish (L : in out List);

   --  Move to next position
   --  require: not After (L)
   --  ensure: Index (L) = old Index (L) + 1
   procedure Forth (L : in out List);

   --  Move to i'th position
   --  require: I in 0..Count (L) + 1
   --  ensure: Index (L) = I
   procedure Go_I_Th (L : in out List; I : Natural);

   --  Move I positions
   --  ensure:
   --    old Index (L) + I > Count (L) implies Off (L)
   --    old Index (L) + I < 1 implies Off (L)
   --    not Off (L) implies Index (L) = old Index (L) + I
   --    (old Index (L) + I >= 0 and old Index (L) + I <= Count (L))
   --      implies Index (L) = old Index (L)
   --    old Index (L) + I <= 0 implies Before (L)
   --    old Index (L) + I >= Count (L) + 1 implies After (L)
   procedure Move (L : in out List; I : Integer);

   --  Move to first position
   --  ensure:
   --    not Empty (L) implies Is_First (L)
   --    Empty (L) implies After (L)
   procedure Start (L : in out List);


   --  Element change:

   --  Add E to end. Do not move cursor
   procedure Extend (L : in out List; E : Element);

   --  Add E to start.  Do not move cursor
   --  ensure:
   --    Count (L) = old Count (L) + 1
   --    First (L) = E
   procedure Put_Front (L : in out List; E : Element);

   --  Add E to left of cursor. Do not move cursor
   --  require: not Before (L)
   --  ensure:
   --    Count (L) = old Count (L) + 1
   --    Index (L) = old Index (L) + 1
   procedure Put_Left (L : in out List; E : Element);

   --  Add E to right of cursor.  Do not move cursor
   --  require: not After (L)
   --  ensure:
   --    Count (L) = old Count (L) + 1
   --    Index (L) = old Index (L)
   procedure Put_Right (L : in out List; E : Element);

   --  Replace current item with E
   --    require: not Off (L)
   --    ensure: Item (L) = E
   procedure Replace (L : in out List; E : Element);


   --  Removal:

   --  Remove current item.  Move cursor to right
   --    require: not Off (L)
   --    ensure: Empty (L) implies After (L)
   procedure Remove (L : in out List);

   --  Remove item to left of cursor.  Do not move cursor
   --    require:
   --      Index (L) > 1
   --      not Before (L)
   --    ensure:
   --      Count (L) = old Count (L) - 1
   --      Index (L) = old Index (L) - 1
   procedure Remove_Left (L : in out List);

   --  Remove item to right of cursor.  Do not move cursor
   --    require: Index (L) < Count (L)
   --    ensure:
   --      Count (L) = old Count (L) - 1
   --      Index (L) = old Index (L)
   procedure Remove_Right (L : in out List);

   --  Remove all items
   --    ensure: Empty (L)
   procedure Wipe_Out (L : in out List);

private

   --  List node
   type Linkable;

   type Linkable_Access is access all Linkable;

   type List is tagged record
      Count : Natural := 0;
      Before : Boolean := True;
      After : Boolean := False;
      First_Element, Active : Linkable_Access;
   end record;

   --  Declared to allow indefinite "Item" component in "Linkable":
   type Element_Access is access all Element;

   type Linkable is record
      Item : Element_Access;
      Right : Linkable_Access := null;
   end record;

   --  Add R to right of L
   procedure Put_Right (L : Linkable_Access; R : Linkable_Access);

end AUnit.Lists;

