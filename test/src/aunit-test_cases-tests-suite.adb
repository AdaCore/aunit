--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Cases.Tests.Suite is

   package Caller is new AUnit.Test_Caller (AUnit.Test_Cases.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Routines",
             "AUnit.Test_Cases.Tests",
             "src/aunit-test_cases-tests.ads",
             (new String'("aunit-test_cases-tests_fixtures.ads"),
              14,
              4,
              new String'("Register_Tests")),
             null,
             Test_Register_Tests'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Set_Up",
             "AUnit.Test_Cases.Tests",
             "src/aunit-test_cases-tests.ads",
             (new String'("aunit-test_cases-tests_fixtures.ads"),
              32,
              4,
              new String'("Set_Up")),
             null,
             Test_Set_Up'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Tear_Down",
             "AUnit.Test_Cases.Tests",
             "src/aunit-test_cases-tests.ads",
             (new String'("aunit-test_cases-tests_fixtures.ads"),
              35,
              4,
              new String'("Tear_Down")),
             null,
             Test_Torn_Down'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Run",
             "AUnit.Test_Cases.Tests",
             "src/aunit-test_cases-tests.ads",
             (new String'("aunit-simple_test_cases.ads"),
              79,
              14,
              new String'("Run")),
             null,
             Test_Run'Access)));

      return S;
   end Test_Suite;

end AUnit.Test_Cases.Tests.Suite;
