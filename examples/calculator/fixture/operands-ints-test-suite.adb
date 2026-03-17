--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Caller;

package body Operands.Ints.Test.Suite is

   package Caller is new AUnit.Test_Caller (Operands.Ints.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Operands.Ints.Image",
            Test_Package => "Operands.Ints.Test",
            Test_File    => "/tests/operands-ints-test.ads",
            Location     =>
              (Tested_File   => new String'("operands-int.ads"),
               Tested_Line   => 8,
               Tested_Column => 4,
               Tested_Name => new String'("Image")),
            Suffix       => null,
            Test         => Test_Image'Access));
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Operands.Ints.Value",
            Test_Package => "Operands.Ints.Test",
            Test_File    => "/tests/operands-ints-test.ads",
            Location     =>
              (Tested_File   => new String'("operands-ints-test.ads"),
               Tested_Line   => 10,
               Tested_Column => 4,
               Tested_Name => new String'("Value")),
            Suffix       => null,
            Test         => Test_Value'Access));

      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Operands.Ints.Set",
            Test_Package => "Operands.Ints.Test",
            Test_File    => "/tests/operands-ints-test.ads",
            Location     =>
              (Tested_File   => new String'("operands-ints-test.ads"),
               Tested_Line   => 12,
               Tested_Column => 4,
               Tested_Name => new String'("Set")),
            Suffix       => null,
            Test         => Test_Set'Access));
      return Ret;
   end Suite;

end Operands.Ints.Test.Suite;
