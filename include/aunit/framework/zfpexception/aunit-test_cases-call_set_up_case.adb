with AUnit.Last_Chance_Handler; use AUnit.Last_Chance_Handler;
with AUnit.Memory.Utils;        use AUnit.Memory.Utils;

separate (AUnit.Test_Cases)
function Call_Set_Up_Case
  (Test : in out Test_Case'Class) return Test_Error_Access is
   function Alloc_Error is new Gen_Alloc (Test_Error, Test_Error_Access);

   procedure Internal_Set_Up_Case is
   begin
      Set_Up_Case (Test);
   end Internal_Set_Up_Case;

   function Internal_Setjmp is new Gen_Setjmp (Internal_Set_Up_Case);

   Error : Test_Error_Access := null;
begin
   if Internal_Setjmp /= 0 then
      Error := Alloc_Error;
      Error.Exception_Name    := Get_Exception_Name;
      Error.Exception_Message := Get_Exception_Message;
      Error.Traceback         := null;
   end if;
   return Error;
end Call_Set_Up_Case;
