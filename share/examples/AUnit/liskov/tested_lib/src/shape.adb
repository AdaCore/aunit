--
--  Copyright (C) 2008, AdaCore
--
package body Shape is

   procedure Set_Width (Obj : in out Shape_Type; Width : Natural) is
   begin
      Obj.Width := Width;
   end Set_Width;

   procedure Set_Height (Obj : in out Shape_Type; Height : Natural) is
   begin
      Obj.Height := Height;
   end Set_Height;

   function Get_Width (Obj : in Shape_Type) return Natural is
   begin
      return Obj.Width;
   end Get_Width;

   function Get_Height (Obj : in Shape_Type) return Natural is
   begin
      return Obj.Height;
   end Get_Height;

end Shape;
