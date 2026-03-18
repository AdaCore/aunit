--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Fixtures.Tests.Suite is

   package Caller is new AUnit.Test_Caller (AUnit.Test_Fixtures.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Set_Up",
             "AUnit.Test_Fixtures.Tests",
             "src/aunit-test_fixtures-tests.ads",
             (new String'("aunit-test_fixtures-tests_fixtures.ads"),
              14,
              4,
              new String'("Set_Up")),
             null,
             Test_Set_Up'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Tear_Down",
             "AUnit.Test_Fixtures.Tests",
             "src/aunit-test_fixtures-tests.ads",
             (new String'("aunit-test_fixtures-tests_fixtures.ads"),
              15,
              4,
              new String'("Tear_Down")),
             null,
             Test_Tear_Down_Success'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Tear_Down",
             "AUnit.Test_Fixtures.Tests",
             "src/aunit-test_fixtures-tests.ads",
             (new String'("aunit-test_fixtures-tests_fixtures.ads"),
              15,
              4,
              new String'("Tear_Down")),
             null,
             Test_Tear_Down_Failure'Access)));
      AUnit.Test_Suites.Add_Test
        (S,
         (Caller.Create
            ("Test Tear_Down",
             "AUnit.Test_Fixtures.Tests",
             "src/aunit-test_fixtures-tests.ads",
             (new String'("aunit-test_fixtures-tests_fixtures.ads"),
              15,
              4,
              new String'("Tear_Down")),
             null,
             Test_Tear_Down_Error'Access)));

      return S;
   end Test_Suite;

end AUnit.Test_Fixtures.Tests.Suite;
