--
--  Copyright (C) 2008, AdaCore
--

with AUnit.Test_Caller;
with AUnit.Test_Info; use AUnit.Test_Info;

package body Rectangle.Tests.Suite is

   package Runner is new AUnit.Test_Caller (Rectangle.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Test_Width  : aliased Runner.Test_Case;
   Test_Height : aliased Runner.Test_Case;
   Test_Area   : aliased Runner.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Runner.Create
        (TC           => Test_Width,
         Name         => "Rectangle : Test width",
         Test_Package => "Rectangle.Tests",
         Test_File    => "/tests/rectangle-tests.ads",
         Location     =>
           (Tested_File   => new String'("shape.ads"),
            Tested_Line   => 21,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Width")),
         Suffix       =>
           new Test_Suffix'
             (Suffix_Text       => new String'("inherited at"),
              Suffix_Location   =>
                new Tested_Location'
                  ((Tested_File   => new String'("rectangle.ads"),
                    Tested_Line   => 16,
                    Tested_Column => 4,
                    Tested_Name => null)),
              Additional_Suffix => null),
         Test         => Test_Set_Width'Access);
      Runner.Create
        (TC           => Test_Height,
         Name         => "Rectangle : Test height",
         Test_Package => "Rectangle.Tests",
         Test_File    => "/tests/rectangle-tests.ads",
         Location     =>
           (Tested_File   => new String'("shape.ads"),
            Tested_Line   => 22,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Height")),
         Suffix       =>
           new Test_Suffix'
             (Suffix_Text       => new String'("inherited at"),
              Suffix_Location   =>
                new Tested_Location'
                  ((Tested_File   => new String'("rectangle.ads"),
                    Tested_Line   => 16,
                    Tested_Column => 4,
                    Tested_Name => null)),
              Additional_Suffix => null),
         Test         => Test_Set_Width'Access);

      Runner.Create
        (TC           => Test_Area,
         Name         => "Rectangle : Test area",
         Test_Package => "Rectangle.Tests",
         Test_File    => "/tests/rectangle-tests.ads",
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

end Rectangle.Tests.Suite;
