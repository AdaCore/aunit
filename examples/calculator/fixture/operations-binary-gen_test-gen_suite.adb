--
--  Copyright (C) 2008, AdaCore
--

package body Operations.Binary.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test " & Instance_Name & ".Pop",
            Test_Package => "Operations.Binary.Gen_Test",
            Test_File    => "/tests/operations-binary-gen_test.ads",
            Location     =>
              (Tested_File   => new String'("operation-binary.ads"),
               Tested_Line   => 15,
               Tested_Column => 4,
               Tested_Name => new String'("Pop")),
            Suffix       => null,
            Test         => Test_Pop_Access));

      Ret.Add_Test
        (Caller.Create
           (Name         => "Test " & Instance_Name & ".Push",
            Test_Package => "Operations.Binary.Gen_Test",
            Test_File    => "/tests/operations-binary-gen_test.ads",
            Location     =>
              (Tested_File   => new String'("operation-binary.ads"),
               Tested_Line   => 26,
               Tested_Column => 4,
               Tested_Name => new String'("Push")),
            Suffix       => null,
            Test         => Test_Push_Access));
      Ret.Add_Test
        (Caller.Create
           (Name         => "Test " & Instance_Name & ".Execute",
            Test_Package => "Operations.Binary.Gen_Test",
            Test_File    => "/tests/operations-binary-gen_test.ads",
            Location     =>
              (Tested_File   => new String'("operation-binary.ads"),
               Tested_Line   => 35,
               Tested_Column => 4,
               Tested_Name => new String'("Execute")),
            Suffix       => null,
            Test         => Test_Push_Access));
      return Ret;
   end Suite;

end Operations.Binary.Gen_Test.Gen_Suite;
