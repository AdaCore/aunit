--
--  Copyright (C) 2008, AdaCore
--
package body Rectangle is

   function Get_Area (Obj : Rectangle_Type) return Natural is
   begin
      return Obj.Get_Width * Obj.Get_Height;
   end Get_Area;

end Rectangle;
