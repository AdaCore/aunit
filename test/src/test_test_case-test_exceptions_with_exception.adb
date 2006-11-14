separate (Test_Test_Case)
procedure Test_Exceptions (T : in out Test_Cases.Test_Case'Class) is
begin
   raise Constraint_Error;
end Test_Exceptions;
