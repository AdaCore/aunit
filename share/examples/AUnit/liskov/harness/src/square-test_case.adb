--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions;   use AUnit.Assertions;

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
   end Run_Test;

   -----------------
   -- Set_Up_Case --
   -----------------

   Local_Square : aliased Square_Type;

   procedure Set_Up (Test : in out The_Test_Case) is
   begin
      Test.The_Shape := Local_Square'Access;
   end Set_Up;

   -------------------
   -- Test_Get_Area --
   -------------------

   procedure Test_Get_Area (T : in out The_Test_Case) is
   begin
      T.The_Shape.Set_Width (3);
      Assert (T.The_Shape.Area = 9,
              "Wrong area returned for object square");
      T.The_Shape.Set_Height (5);
      Assert (T.The_Shape.Area = 25,
              "Wrong area returned for object square");
   end Test_Get_Area;

end Square.Test_Case;
