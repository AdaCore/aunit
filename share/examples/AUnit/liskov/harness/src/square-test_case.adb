--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions;   use AUnit.Assertions;

with Shape;

package body Square.Test_Case is

   ----------
   -- Name --
   ----------

   function Name (Test : The_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("Square test case");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Run_Test (Test : in out The_Test_Case) is
   begin
      --  Tests for the square object
      Test_Set_Width (Test);
      Test_Set_Height (Test);
      Test_Get_Area (Test);

      --  Test that a Square is a correct Rectangle
      --  (Liskov substitution principle)
      Rectangle.Test_Case.Run_Test
        (Rectangle.Test_Case.The_Test_Case (Test));
   end Run_Test;

   -----------------
   -- Set_Up_Case --
   -----------------

   procedure Set_Up (Test : in out The_Test_Case) is
   begin
      Test.The_Shape := new Square_Type;
   end Set_Up;

   -------------------
   -- Test_Get_Area --
   -------------------

   procedure Test_Get_Area (T : in out The_Test_Case) is
   begin
      Shape.Set_Width (T.The_Shape.all, 3);
      Assert (Square.Get_Area (Square_Type (T.The_Shape.all)) = 9,
              "Wrong area returned for object square");
      Shape.Set_Height (T.The_Shape.all, 5);
      Assert (Square.Get_Area (Square_Type (T.The_Shape.all)) = 25,
              "Wrong area returned for object square");
   end Test_Get_Area;

end Square.Test_Case;
