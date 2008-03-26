--  Dummy version that contains minimal output procedure (which do nothing).
package body Textio is

   procedure New_Line is
      procedure Internal;
      pragma Import (C, Internal, "bug_putnl");
   begin
      Internal;
   end New_Line;

   procedure Put (Item : in Character) is
      procedure Internal (C   : Character);
      pragma Import (C, Internal, "putchar");
   begin
      Internal (Item);
   end Put;

   procedure Put (Item : in String) is
      procedure Internal (S   : String;
                         Len : Natural);
      pragma Import (C, Internal, "bug_putstring");
   begin
      Internal (Item, Item'Length);
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

end Textio;
