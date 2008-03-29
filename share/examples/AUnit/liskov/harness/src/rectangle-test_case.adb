--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions;   use AUnit.Assertions;

with Shape;

package body Rectangle.Test_Case is

   function Name (Test : The_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("Rectangle test case");
   end Name;

   procedure Run_Test (Test : in out The_Test_Case) is
   begin
      --  Tests for the Rectangle object
      Test_Set_Width (Test);
      Test_Set_Height (Test);
      Test_Get_Area (Test);

      --  Test that a Rectangle is a correct Shape
      --  (Liskov substitution principle)
      Shape.Test_Case.Run_Test (Shape.Test_Case.The_Test_Case (Test));
   end Run_Test;

   Local_Rectangle : aliased Rectangle_Type;
   procedure Set_Up (Test : in out The_Test_Case) is
   begin
      Test.The_Shape := Local_Rectangle'Access;
   end Set_Up;

   procedure Test_Get_Area (T : in out The_Test_Case) is
   begin
      Shape.Set_Width (T.The_Shape.all, 3);
      Shape.Set_Height (T.The_Shape.all, 5);
      Assert (Shape.Area (T.The_Shape.all) = 15,
              "Wrong area returned for object rectangle");
   end Test_Get_Area;

end Rectangle.Test_Case;
