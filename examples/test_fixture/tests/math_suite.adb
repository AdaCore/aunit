--
--  Copyright (C) 2008, AdaCore
--
with Math.Test; use Math.Test;
with AUnit.Test_Caller;

package body Math_Suite is

   package Caller is new AUnit.Test_Caller (Math.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test addition",
            Test_Package => "Math.Test",
            Test_File    => "/tests/math-test.ads",
            Location     =>
              (Tested_File   => new String'("math.ads"),
               Tested_Line   => 8,
               Tested_Column => 4,
               Tested_Name => new String'("""+""")),
            Suffix       => null,
            Test         => Test_Addition'Access));

      Ret.Add_Test
        (Caller.Create
           (Name         => "Test subtraction",
            Test_Package => "Math.Test",
            Test_File    => "/tests/math-test.ads",
            Location     =>
              (Tested_File   => new String'("math.ads"),
               Tested_Line   => 10,
               Tested_Column => 4,
               Tested_Name => new String'("""-""")),
            Suffix       => null,
            Test         => Test_Subtraction'Access));
      return Ret;
   end Suite;

end Math_Suite;
