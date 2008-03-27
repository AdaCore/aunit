--
--  Copyright (C) 2008, AdaCore
--

package body Square is

   procedure Set_Width (Obj : in out Square_Type; Width : Natural) is
   begin
      Rectangle.Rectangle_Type (Obj).Set_Width (Width);
      Rectangle.Rectangle_Type (Obj).Set_Height (Width);
   end Set_Width;

   procedure Set_Height (Obj : in out Square_Type; Height : Natural) is
   begin
      Rectangle.Rectangle_Type (Obj).Set_Width (Height);
      Rectangle.Rectangle_Type (Obj).Set_Height (Height);
   end Set_Height;

end Square;
