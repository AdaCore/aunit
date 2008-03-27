--
--  Copyright (C) 2008, AdaCore
--

package Shape is

   type Shape_Type is abstract tagged private;
   type Shape_Access is access all Shape_Type'Class;

   procedure Set_Width (Obj : in out Shape_Type; Width : Natural);
   procedure Set_Height (Obj : in out Shape_Type; Height : Natural);

   function Get_Width (Obj : in Shape_Type) return Natural;
   function Get_Height (Obj : in Shape_Type) return Natural;

   function Get_Area (Obj : in Shape_Type) return Natural is abstract;

private

   type Shape_Type is abstract tagged record
      Width : Natural;
      Height : Natural;
   end record;

end Shape;
