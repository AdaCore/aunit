--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Shape.Test_Case is

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out The_Test_Case) is
   begin
      Test_Set_Width (Test);
      Test_Set_Height (Test);
   end Run_Test;

   --------------------
   -- Test_Set_Width --
   --------------------

   procedure Test_Set_Width (T : in out The_Test_Case) is
   begin
      Set_Width (T.The_Shape.all, 3);
      Assert
        (Get_Width (T.The_Shape.all) = 3,
         "Get_Width did not return the correct value after a Set_Width");

      Set_Width (T.The_Shape.all, 7);
      Assert
        (Get_Width (T.The_Shape.all) = 7,
         "Get_Width did not return the correct value after a 2nd Set_Width");
   end Test_Set_Width;

   procedure Test_Set_Height (T : in out The_Test_Case) is
   begin
      Set_Height (T.The_Shape.all, 3);
      Assert
        (Get_Height (T.The_Shape.all) = 3,
         "Get_Height did not return the correct value after a Set_Height");

      Set_Height (T.The_Shape.all, 7);
      Assert
        (Get_Height (T.The_Shape.all) = 7,
         "Get_Height did not return the correct value after a 2nd Set_Height");
   end Test_Set_Height;

end Shape.Test_Case;
