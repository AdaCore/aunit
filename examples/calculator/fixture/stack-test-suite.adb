--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Caller;

package body Stack.Test.Suite is

   package Caller is new AUnit.Test_Caller (Stack.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Stack.Push",
            Test_Package => "Stack.Test",
            Test_File    => "/tests/stack-test.ads",
            Location     =>
              (Tested_File   => new String'("stack.ads"),
               Tested_Line   => 14,
               Tested_Column => 4,
               Tested_Name => new String'("Push")),
            Suffix       => null,
            Test         => Test_Push'Access));
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Stack.Pop",
            Test_Package => "Stack.Test",
            Test_File    => "/tests/stack-test.ads",
            Location     =>
              (Tested_File   => new String'("stack.ads"),
               Tested_Line   => 18,
               Tested_Column => 4,
               Tested_Name => new String'("Pop")),
            Suffix       => null,
            Test         => Test_Pop'Access));

      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Stack.Length",
            Test_Package => "Stack.Test",
            Test_File    => "/tests/stack-test.ads",
            Location     =>
              (Tested_File   => new String'("stack.ads"),
               Tested_Line   => 22,
               Tested_Column => 4,
               Tested_Name => new String'("Length")),
            Suffix       => null,
            Test         => Test_Length'Access));

      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Stack.Top",
            Test_Package => "Stack.Test",
            Test_File    => "/tests/stack-test.ads",
            Location     =>
              (Tested_File   => new String'("stack.ads"),
               Tested_Line   => 26,
               Tested_Column => 4,
               Tested_Name => new String'("Top")),
            Suffix       => null,
            Test         => Test_Top'Access));
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test Stack.Next_To_Top",
            Test_Package => "Stack.Test",
            Test_File    => "/tests/stack-test.ads",
            Location     =>
              (Tested_File   => new String'("stack.ads"),
               Tested_Line   => 30,
               Tested_Column => 4,
               Tested_Name => new String'("Next_To_Top")),
            Suffix       => null,
            Test         => Test_Top'Access));
      return Ret;
   end Suite;

end Stack.Test.Suite;
