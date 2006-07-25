with AUnit.Framework;
package Framework is new AUnit.Framework
  (Max_Tests_Per_Harness      => 50,
   Max_Errors_Per_Harness     => 5,
   Max_Failures_Per_Harness   => 20,
   Max_Failure_Message_Size   => 150,
   Max_Routines_Per_Test      => 50,
   Test_Name_Size             => 30,
   Routine_Name_Size          => 50);
