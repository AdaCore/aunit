--
--  Copyright (C) 2008, AdaCore
--

with AUnit.Test_Caller;

package body Square.Tests.Suite is

   package Runner is new AUnit.Test_Caller (Square.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Test_Width  : aliased Runner.Test_Case;
   Test_Height : aliased Runner.Test_Case;
   Test_Area   : aliased Runner.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Runner.Create
        (TC           => Test_Width,
         Name         => "Square : Test width",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/square-tests.ads",
         Location     =>
           (Tested_File   => new String'("square.ads"),
            Tested_Line   => 13,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Width")),
         Suffix       => null,
         Test         => Test_Set_Width'Access);

      Runner.Create
        (TC           => Test_Height,
         Name         => "Square : Test height",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/square-tests.ads",
         Location     =>
           (Tested_File   => new String'("square.ads"),
            Tested_Line   => 18,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Height")),
         Suffix       => null,
         Test         => Test_Set_Height'Access);
      Runner.Create
        (TC           => Test_Area,
         Name         => "Square : Test area",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/square-tests.ads",
         Location     =>
           (Tested_File   => new String'("rectangle.ads"),
            Tested_Line   => 11,
            Tested_Column => 4,
            Tested_Name => new String'("Area")),
         Suffix       => null,
         Test         => Test_Get_Area'Access);
      Result.Add_Test (Test_Width'Access);
      Result.Add_Test (Test_Height'Access);
      Result.Add_Test (Test_Area'Access);

      return Result'Access;
   end Suite;

end Square.Tests.Suite;
