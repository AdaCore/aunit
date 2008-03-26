with GNAT.IO; use GNAT.IO;
procedure Last_Chance_Handler
  (Source_Location : System.Address; Line : Integer)
is

   pragma Unreferenced (Source_Location, Line);

   procedure Stop (ID : Integer := 0);
   pragma Import (C, Stop, "taskSuspend");
begin
   Put_Line ("Exception occurred");
   Stop;
end Last_Chance_Handler;
