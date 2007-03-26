pragma Restrictions (No_Implicit_Dynamic_Code);
with AUnit_Framework.Framework;
package AUnit is new AUnit_Framework.Framework
  (Max_Exceptions_Per_Harness => 5,
   Max_Failures_Per_Harness   => 20,
   Max_Routines_Per_Test_Case => 50,
   Max_Test_Cases_Per_Suite   => 50);
