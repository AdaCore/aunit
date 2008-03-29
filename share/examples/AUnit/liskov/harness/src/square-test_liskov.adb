--
--  Copyright (C) 2008, AdaCore
--

with Rectangle.Test_Case;

package body Square.Test_Liskov is

   ----------
   -- Name --
   ----------

   function Name (Test : The_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("Test Square IS A Rectangle");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Run_Test (Test : in out The_Test_Case) is
   begin
      --  Test that a Square is a correct Rectangle
      --  (Liskov substitution principle)
      Rectangle.Test_Case.Run_Test
        (Rectangle.Test_Case.The_Test_Case (Test));
   end Run_Test;

   -----------------
   -- Set_Up_Case --
   -----------------

   Local_Square : aliased Square_Type;

   procedure Set_Up (Test : in out The_Test_Case) is
   begin
      Test.The_Shape := Local_Square'Access;
   end Set_Up;

end Square.Test_Liskov;
