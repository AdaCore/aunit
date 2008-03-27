--
--  Copyright (C) 2008, AdaCore
--

with Rectangle;

package Square is

   type Square_Type is new Rectangle.Rectangle_Type with private;

   procedure Set_Width (Obj : in out Square_Type; Width : Natural);
   procedure Set_Height (Obj : in out Square_Type; Height : Natural);

private

   type Square_Type is new Rectangle.Rectangle_Type with null record;

end Square;
