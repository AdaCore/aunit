--
--  Copyright (C) 2008, AdaCore
--

with Shape;

package Rectangle is

   type Rectangle_Type is new Shape.Shape_Type with private;

   function Get_Area (Obj : Rectangle_Type) return Natural;

private

   type Rectangle_Type is new Shape.Shape_Type with null record;

end Rectangle;
