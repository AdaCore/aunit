with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body AUnit.Memory.Utils is

   type Bounds is record
      First : Natural;
      Last  : Natural;
   end record;
   type Bounds_Access is access all Bounds;

   type Fat_Pointer is record
      Address       : System.Address;
      Bound_Address : Bounds_Access;
   end record;
   pragma Pack (Fat_Pointer);

   function Message_Alloc (Length : Natural) return Message_String is
      function To_Message is new Ada.Unchecked_Conversion
        (Fat_Pointer, Message_String);
      function To_Bounds_Access is new Ada.Unchecked_Conversion
        (System.Address, Bounds_Access);
      function To_Address is new Ada.Unchecked_Conversion
        (Bounds_Access, System.Address);
      Ret : Fat_Pointer;
   begin
      Ret.Bound_Address := To_Bounds_Access
        (AUnit_Alloc (size_t (Length + (Bounds'Size / 8))));
      Ret.Bound_Address.First := 1;
      Ret.Bound_Address.Last := Length;
      Ret.Address := To_Address (Ret.Bound_Address) + (Bounds'Size / 8);
      return To_Message (Ret);
   end Message_Alloc;

   function Gen_Alloc return Name is
      function To_Name is new Ada.Unchecked_Conversion (System.Address, Name);
   begin
      return To_Name (AUnit_Alloc (Object'Size / 8));
   end Gen_Alloc;

end AUnit.Memory.Utils;
