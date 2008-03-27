--
--  Copyright (C) 2008, AdaCore
--
with AUnit; use AUnit;
with Shape.Test_Case;

package Rectangle.Test_Case is

   type The_Test_Case is new Shape.Test_Case.The_Test_Case with null record;

   function Name (Test : The_Test_Case) return Message_String;

   procedure Run_Test (Test : in out The_Test_Case);
   procedure Set_Up (Test : in out The_Test_Case);

   procedure Test_Get_Area (T : in out The_Test_Case);

end Rectangle.Test_Case;
