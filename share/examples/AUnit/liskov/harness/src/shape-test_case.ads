--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Simple_Test_Cases;

package Shape.Test_Case is

   type The_Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case
   with record
      The_Shape : Shape_Access;
   end record;

   procedure Run_Test (Test : in out The_Test_Case);

   procedure Test_Set_Width (T : in out The_Test_Case);
   procedure Test_Set_Height (T : in out The_Test_Case);
   procedure Test_Get_Area (T : in out The_Test_Case) is abstract;

end Shape.Test_Case;
