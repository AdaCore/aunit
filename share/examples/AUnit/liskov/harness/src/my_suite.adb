--
--  Copyright (C) 2008, AdaCore
--
with Rectangle.Test_Case;
with Square.Test_Case;

package body My_Suite is

   Result       : aliased AUnit.Test_Suites.Test_Suite;

   Rectangle_TC : aliased Rectangle.Test_Case.The_Test_Case;
   Square_TC    : aliased Square.Test_Case.The_Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Rectangle_TC'Access);
      AUnit.Test_Suites.Add_Test (Result'Access, Square_TC'Access);

      return Result'Access;
   end Suite;

end My_Suite;
