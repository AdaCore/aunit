with Ada.Exceptions;          use Ada.Exceptions;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with AUnit.Memory.Utils;      use AUnit.Memory.Utils;

separate (AUnit.Test_Cases)
function Call_Set_Up_Case
  (Test : in out Test_Case'Class) return Test_Error_Access is
   function Alloc_Error is new Gen_Alloc (Test_Error, Test_Error_Access);
begin
   Set_Up_Case (Test);
   return null;
exception when E : others =>
      return Error : constant Test_Error_Access := Alloc_Error do
         Error.Exception_Name    := Format (Exception_Name (E));
         Error.Exception_Message := Format (Exception_Message (E));
         Error.Traceback         := Format (Symbolic_Traceback (E));
      end return;
end Call_Set_Up_Case;
