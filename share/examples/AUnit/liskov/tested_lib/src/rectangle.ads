--
--  Copyright (C) 2008, AdaCore
--

with Shape;

package Rectangle is

   type Rectangle_Type is new Shape.Shape_Type with private;

   function Area (Obj : Rectangle_Type) return Natural;
--  ???commented out because of bogus precommit checks
--   pragma Postcondition (Area'Result = Width (Obj) * Height (Obj));

private

   type Rectangle_Type is new Shape.Shape_Type with null record;

end Rectangle;
