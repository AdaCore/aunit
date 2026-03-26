--
--  Copyright (C) 2009-2026, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Suites.Tests.Suite is

   package Caller is new AUnit.Test_Caller (AUnit.Test_Suites.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Add test case",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"),
             48,
             4,
             new String'("Add_Test")),
            null,
            Test_Add_Test_Case'Access));

      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Empty suite",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_Empty'Access));

      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Run with successful test",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_With_Success'Access));

      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Run with a failing test",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_With_Failure'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Run with a test raising an exception",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_With_Exception'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test Run with various tests",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_With_All'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("Test that Set_Up/Tear_Down are called",
            "AUnit.Test_Suites.Tests",
            "src/aunit-test_suites-tests.ads",
            (new String'("aunit-test_suites.ads"), 52, 4, new String'("Run")),
            null,
            Test_Run_With_Setup'Access));
      return S;
   end Test_Suite;

end AUnit.Test_Suites.Tests.Suite;
