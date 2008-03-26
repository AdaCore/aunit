with System;
with AUnit; use AUnit;

package Last_Chance_Handler is

   function Get_Last_Msg return Message_String;
   --  Return the last exception message

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

end Last_Chance_Handler;
