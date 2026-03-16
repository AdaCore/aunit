--
--  Copyright (C) 2008, AdaCore
--
with Ada.Unchecked_Conversion;
with AUnit.Test_Caller;
with AUnit.Test_Info; use AUnit.Test_Info;
with Rectangle.Tests;

package body Square.Tests.Suite_Liskov is

   package Runner is new AUnit.Test_Caller (Square.Tests.Test);
   package Rectangle_Runner is new AUnit.Test_Caller (Rectangle.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Test_Width  : aliased Runner.Test_Case;
   Test_Height : aliased Runner.Test_Case;
   Test_Area   : aliased Runner.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      function Convert is new
        Ada.Unchecked_Conversion
          (Rectangle_Runner.Test_Method,
           Runner.Test_Method);
   begin
      Runner.Create
        (TC           => Test_Width,
         Name         => "Square as Rectangle (liskov) : Test width",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/shape-tests.ads",
         Location     =>
           (Tested_File   => new String'("shape-tests.ads"),
            Tested_Line   => 13,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Width")),
         Suffix       => null,
         Test         =>
           Convert
             (Rectangle_Runner.Test_Method'
                (Rectangle.Tests.Test_Set_Width'Access)));

      Runner.Create
        (TC           => Test_Height,
         Name         => "Square as Rectangle (liskov) : Test height",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/shape-tests.ads",
         Location     =>
           (Tested_File   => new String'("shape-tests.ads"),
            Tested_Line   => 14,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Height")),
         Suffix       => null,
         Test         =>
           Convert
             (Rectangle_Runner.Test_Method'
                (Rectangle.Tests.Test_Set_Height'Access)));

      Runner.Create
        (TC           => Test_Area,
         Name         => "Square as Rectangle (liskov) : Test area",
         Test_Package => "Shape.Tests",
         Test_File    => "/tests/shape-tests.ads",
         Location     =>
           (Tested_File   => new String'("shape-tests.ads"),
            Tested_Line   => 14,
            Tested_Column => 4,
            Tested_Name => new String'("Set_Height")),
         Suffix       =>
           new Test_Suffix'
             (Suffix_Text       => new String'("inherited at"),
              Suffix_Location   =>
                new Tested_Location'
                  (Tested_File   => new String'("rectangle.ads"),
                   Tested_Line   => 11,
                   Tested_Column => 4,
                   Tested_Name => null),

              Additional_Suffix => null),

         Test         =>
           Convert
             (Rectangle_Runner.Test_Method'
                (Rectangle.Tests.Test_Get_Area'Access)));

      Result.Add_Test (Test_Width'Access);
      Result.Add_Test (Test_Height'Access);
      Result.Add_Test (Test_Area'Access);

      return Result'Access;
   end Suite;

end Square.Tests.Suite_Liskov;
