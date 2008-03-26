with System;

package Last_Chance_Handler is

   procedure Handler (Msg : System.Address; Location : Integer);
   pragma No_Return (Handler);
   pragma Export (C, Handler, "__gnat_last_chance_handler");

end Last_Chance_Handler;
