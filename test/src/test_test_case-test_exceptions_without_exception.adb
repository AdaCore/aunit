separate (Test_Test_Case)
procedure Test_Exceptions (T : in out Test_Cases.Test_Case'Class) is
   pragma Unreferenced (T);
begin
   Assert (False, "exceptions not supported: not tested");
end Test_Exceptions;
