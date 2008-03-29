--
--  Copyright (C) 2008, AdaCore
--
with AUnit; use AUnit;
with Rectangle.Test_Case;

package Rectangle.Test_Liskov is

   type The_Test_Case is
     new Rectangle.Test_Case.The_Test_Case with null record;

   function Name (Test : The_Test_Case) return Message_String;

   procedure Run_Test (Test : in out The_Test_Case);
   procedure Set_Up (Test : in out The_Test_Case);

end Rectangle.Test_Liskov;
