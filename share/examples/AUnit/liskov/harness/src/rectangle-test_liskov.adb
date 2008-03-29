--
--  Copyright (C) 2008, AdaCore
--
with Shape.Test_Case;

package body Rectangle.Test_Liskov is

   function Name (Test : The_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("test that Rectangle IS A Shape (liskov)");
   end Name;

   procedure Run_Test (Test : in out The_Test_Case) is
   begin
      --  Test that a Rectangle is a correct Shape
      --  (Liskov substitution principle)
      Shape.Test_Case.Run_Test (Shape.Test_Case.The_Test_Case (Test));
   end Run_Test;

   Local_Rectangle : aliased Rectangle_Type;
   procedure Set_Up (Test : in out The_Test_Case) is
   begin
      Test.The_Shape := Local_Rectangle'Access;
   end Set_Up;
end Rectangle.Test_Liskov;
