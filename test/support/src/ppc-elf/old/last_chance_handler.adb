with Textio;
with Unchecked_Conversion;
package body Last_Chance_Handler is

   --  Put a message and abort

   -------------
   -- Handler --
   -------------

   procedure Handler (Msg : System.Address; Location : Integer) is
      pragma Unreferenced (Location);

      procedure C_Abort;
      pragma Import (C, C_Abort, "abort");
      pragma No_Return (C_Abort);

      subtype Chars is String (1..100);
      type Ptr is access all Chars;
      function To_Ptr is new Unchecked_Conversion (System.Address, Ptr);
      V : Ptr := To_Ptr (Msg);
      I : integer := Chars'First;
   begin
      while I <= Chars'Last and then V (I) /= Ascii.Nul loop
         I := I + 1;
      end loop;

      Textio.Put_Line (V (1 .. I - 1));
      Textio.Put_Line ("---> Last_Chance_Handler exit...");

      --  No return procedure.
      C_Abort;
   end Handler;

end Last_Chance_Handler;
