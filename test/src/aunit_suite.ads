with AUnit; use AUnit;
with AUnit.Test_Suites;
package AUnit_Suite is
   function Suite_Suites return Test_Suites.Access_Test_Suite;
   function Suite_Cases return Test_Suites.Access_Test_Suite;
   function Suite_Fixtures return Test_Suites.Access_Test_Suite;
end AUnit_Suite;
