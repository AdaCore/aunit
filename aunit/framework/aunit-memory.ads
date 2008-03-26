with System;

package AUnit.Memory is

   type size_t is mod 2 ** Standard'Address_Size;

   function AUnit_Alloc (Size : size_t) return System.Address;

   procedure AUnit_Free (Obj : System.Address);

private

   pragma Inline (AUnit_Alloc);
   pragma Inline (AUnit_Free);

end AUnit.Memory;
